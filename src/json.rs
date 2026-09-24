//! JSON to objects and back.

use std::collections::BTreeMap;
use std::rc::Rc;

use num_bigint::BigInt;
use serde::de::{DeserializeSeed, Deserializer, MapAccess, SeqAccess, Visitor};
use serde_json::Value;

use crate::error::{Error, EvalError};
use crate::eval::{IntoObject, Object};
use crate::hex::{encode, parse_shared_bytes};

/// How JSON strings are converted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Strings {
    /// Strings stay strings.
    #[default]
    Plain,
    /// "0x..." hex strings become bytes (an odd number of digits gets a leading zero: "0x1" is
    /// `b"01"`), as addresses, hashes, calldata and numbers are written in Ethereum JSON-RPC.
    Hex,
}

/// JSON to bind with `Context::bind`: text or a `serde_json::Value`, with the way to convert
/// its strings. Text and values bound as they are (without `Json`) are converted as `Plain`.
pub struct Json<T> {
    json: T,
    strings: Strings,
}

impl<T> Json<T> {
    pub fn new(json: T, strings: Strings) -> Self {
        Json { json, strings }
    }

    pub fn plain(json: T) -> Self {
        Json::new(json, Strings::Plain)
    }

    pub fn hex(json: T) -> Self {
        Json::new(json, Strings::Hex)
    }
}

impl IntoObject for Json<&str> {
    fn into_object(self) -> Result<Object, Error> {
        Ok(parse_json(self.json, self.strings)?)
    }
}

impl IntoObject for Json<String> {
    fn into_object(self) -> Result<Object, Error> {
        Ok(parse_json(&self.json, self.strings)?)
    }
}

impl IntoObject for Json<&Value> {
    fn into_object(self) -> Result<Object, Error> {
        Ok(from_json(self.json, self.strings))
    }
}

impl IntoObject for Json<Value> {
    fn into_object(self) -> Result<Object, Error> {
        Ok(from_json(&self.json, self.strings))
    }
}

impl IntoObject for &str {
    fn into_object(self) -> Result<Object, Error> {
        Json::plain(self).into_object()
    }
}

impl IntoObject for String {
    fn into_object(self) -> Result<Object, Error> {
        Json::plain(self).into_object()
    }
}

impl IntoObject for &Value {
    fn into_object(self) -> Result<Object, Error> {
        Json::plain(self).into_object()
    }
}

impl IntoObject for Value {
    fn into_object(self) -> Result<Object, Error> {
        Json::plain(self).into_object()
    }
}

/// Parses JSON text directly into an object, the same as `from_json` would give, without
/// building a `serde_json::Value` first.
pub fn parse_json(text: &str, strings: Strings) -> Result<Object, serde_json::Error> {
    let mut deserializer = serde_json::Deserializer::from_str(text);
    let obj = JsonSeed(strings).deserialize(&mut deserializer)?;
    deserializer.end()?;
    Ok(obj)
}

// with the 'arbitrary_precision' feature, serde_json passes a number to a deserializer
// as a map with this single key and the number as a string
const NUMBER_TOKEN: &str = "$serde_json::private::Number";

fn number(text: &str) -> Object {
    match text.parse::<BigInt>() {
        Ok(int) => Object::Int(int),
        Err(_) => Object::Float(text.parse().unwrap_or(f64::NAN)),
    }
}

fn string(s: &str, strings: Strings) -> Object {
    match strings {
        Strings::Hex => match parse_shared_bytes(s) {
            Some(bytes) => Object::Bytes(bytes),
            None => Object::Str(s.to_string()),
        },
        Strings::Plain => Object::Str(s.to_string()),
    }
}

#[derive(Clone, Copy)]
struct JsonSeed(Strings);

impl<'de> DeserializeSeed<'de> for JsonSeed {
    type Value = Object;

    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Object, D::Error> {
        deserializer.deserialize_any(self)
    }
}

impl<'de> Visitor<'de> for JsonSeed {
    type Value = Object;

    fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str("any JSON value")
    }

    fn visit_unit<E>(self) -> Result<Object, E> {
        Ok(Object::Null)
    }

    fn visit_bool<E>(self, b: bool) -> Result<Object, E> {
        Ok(Object::Bool(b))
    }

    fn visit_i64<E>(self, x: i64) -> Result<Object, E> {
        Ok(Object::Int(x.into()))
    }

    fn visit_u64<E>(self, x: u64) -> Result<Object, E> {
        Ok(Object::Int(x.into()))
    }

    fn visit_f64<E>(self, x: f64) -> Result<Object, E> {
        Ok(Object::Float(x))
    }

    fn visit_str<E>(self, s: &str) -> Result<Object, E> {
        Ok(string(s, self.0))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Object, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0));
        while let Some(item) = seq.next_element_seed(self)? {
            items.push(item);
        }
        Ok(Object::List(Rc::new(items)))
    }

    fn visit_map<A: MapAccess<'de>>(self, mut map: A) -> Result<Object, A::Error> {
        let mut fields = BTreeMap::new();
        while let Some(name) = map.next_key::<String>()? {
            if name == NUMBER_TOKEN && fields.is_empty() {
                return Ok(number(&map.next_value::<String>()?));
            }
            fields.insert(name, map.next_value_seed(self)?);
        }
        Ok(Object::Record(Rc::new(fields)))
    }
}

/// Converts a JSON value into an object.
pub fn from_json(value: &Value, strings: Strings) -> Object {
    match value {
        Value::Null => Object::Null,
        Value::Bool(b) => Object::Bool(*b),
        // with 'arbitrary_precision' the number keeps its exact text
        Value::Number(n) => number(&n.to_string()),
        Value::String(s) => string(s, strings),
        Value::Array(items) => Object::List(Rc::new(
            items.iter().map(|item| from_json(item, strings)).collect(),
        )),
        Value::Object(fields) => Object::Record(Rc::new(
            fields
                .iter()
                .map(|(name, value)| (name.clone(), from_json(value, strings)))
                .collect::<BTreeMap<_, _>>(),
        )),
    }
}

/// Converts an object into JSON: bytes become "0x..." strings, integers keep their
/// precision. Functions, infinite floats and NaN cannot be converted.
pub fn to_json(obj: &Object) -> Result<Value, EvalError> {
    // lists and records can be nested as deep as a program makes them
    crate::grow(|| to_nested_json(obj))
}

fn to_nested_json(obj: &Object) -> Result<Value, EvalError> {
    Ok(match obj {
        Object::Null => Value::Null,
        Object::Bool(b) => Value::Bool(*b),
        Object::Int(x) => serde_json::from_str(&x.to_string()).expect("integer is valid JSON"),
        Object::Float(x) => match serde_json::Number::from_f64(*x) {
            Some(number) => Value::Number(number),
            None => return Err(EvalError::NotJson(obj.clone())),
        },
        Object::Str(s) => Value::String(s.clone()),
        Object::Bytes(b) => Value::String(encode(b)),
        Object::List(items) => Value::Array(items.iter().map(to_json).collect::<Result<_, _>>()?),
        Object::Record(fields) => Value::Object(
            fields
                .iter()
                .map(|(name, value)| Ok((name.clone(), to_json(value)?)))
                .collect::<Result<_, EvalError>>()?,
        ),
        Object::Func(_) | Object::Native(_) => return Err(EvalError::NotJson(obj.clone())),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn record(fields: Vec<(&str, Object)>) -> Object {
        Object::Record(Rc::new(
            fields
                .into_iter()
                .map(|(name, value)| (name.to_string(), value))
                .collect(),
        ))
    }

    fn int(s: &str) -> Object {
        Object::Int(s.parse().unwrap())
    }

    #[test]
    fn test_from_json() {
        let tests = vec![
            (json!(null), Object::Null),
            (json!(true), Object::Bool(true)),
            (json!(42), int("42")),
            (json!(-1.5), Object::Float(-1.5)),
            // any "0x..." hex string is bytes, wherever it is
            (json!("0x10"), Object::Bytes(vec![0x10].into())),
            (json!("0x1"), Object::Bytes(vec![0x01].into())),
            (json!("0x"), Object::Bytes(vec![].into())),
            (
                json!(["0xab", "0xABC"]),
                Object::List(Rc::new(vec![
                    Object::Bytes(vec![0xab].into()),
                    Object::Bytes(vec![0x0a, 0xbc].into()),
                ])),
            ),
            // other strings are kept as they are
            (json!("0xzz"), Object::Str("0xzz".to_string())),
            (json!("0X10"), Object::Str("0X10".to_string())),
            (json!("10"), Object::Str("10".to_string())),
            (json!("hello"), Object::Str("hello".to_string())),
            (
                json!([1, "a"]),
                Object::List(Rc::new(vec![int("1"), Object::Str("a".to_string())])),
            ),
            (
                // numbers beyond u64 keep their precision
                serde_json::from_str("123456789012345678901234567890").unwrap(),
                int("123456789012345678901234567890"),
            ),
            (
                json!({
                    "to": "0xdac17f958d2ee523a2206206994597c13d831ec7",
                    "value": "0xde0b6b3a7640000",
                    "id": "0x01",
                    "name": "0x",
                    "nested": {"list": ["0x02", null]},
                }),
                record(vec![
                    (
                        "to",
                        Object::Bytes(
                            vec![
                                0xda, 0xc1, 0x7f, 0x95, 0x8d, 0x2e, 0xe5, 0x23, 0xa2, 0x20, 0x62,
                                0x06, 0x99, 0x45, 0x97, 0xc1, 0x3d, 0x83, 0x1e, 0xc7,
                            ]
                            .into(),
                        ),
                    ),
                    (
                        "value",
                        Object::Bytes(vec![0x0d, 0xe0, 0xb6, 0xb3, 0xa7, 0x64, 0x00, 0x00].into()),
                    ),
                    ("id", Object::Bytes(vec![0x01].into())),
                    ("name", Object::Bytes(vec![].into())),
                    (
                        "nested",
                        record(vec![(
                            "list",
                            Object::List(Rc::new(vec![
                                Object::Bytes(vec![2].into()),
                                Object::Null,
                            ])),
                        )]),
                    ),
                ]),
            ),
        ];

        for (json, expected) in tests {
            assert_eq!(from_json(&json, Strings::Hex), expected, "{}", json);
            // deserialized directly from the text, the object is the same
            assert_eq!(
                parse_json(&json.to_string(), Strings::Hex).unwrap(),
                expected,
                "{}",
                json
            );
        }
    }

    #[test]
    fn test_direct_deserialization() {
        let texts = [
            r#"{"n": [0, -1, 18446744073709551616, -1e3, 1.5e300, 0.1], "s": "\u0030x1\n"}"#,
            r#"{"$serde_json::private::Number": "1"}"#,
            r#"[{"a": {}}, [], "", "0x", "0X1", null, true]"#,
            " 42 ",
        ];
        for text in texts {
            let value: Value = serde_json::from_str(text).unwrap();
            assert_eq!(
                parse_json(text, Strings::Hex).unwrap(),
                from_json(&value, Strings::Hex),
                "{}",
                text
            );
        }
        // the transactions of a block
        for line in std::fs::read_to_string("examples/block.ndjson")
            .unwrap()
            .lines()
        {
            let value: Value = serde_json::from_str(line).unwrap();
            assert_eq!(
                parse_json(line, Strings::Hex).unwrap(),
                from_json(&value, Strings::Hex)
            );
        }
        assert!(parse_json("{", Strings::Plain).is_err());
        assert!(parse_json("1 2", Strings::Plain).is_err());
    }

    #[test]
    fn test_to_json() {
        let src =
            r#"{"a":[1,-2.5,true,null,"s","0x00ff"],"big":123456789012345678901234567890,"b":{}}"#;
        let obj = from_json(&serde_json::from_str(src).unwrap(), Strings::Hex);
        // keys are sorted
        let expected =
            r#"{"a":[1,-2.5,true,null,"s","0x00ff"],"b":{},"big":123456789012345678901234567890}"#;
        assert_eq!(to_json(&obj).unwrap().to_string(), expected);
        let native = Object::Native(Rc::new(crate::Native::pure("f", 0, |_| Ok(Object::Null))));
        assert_eq!(to_json(&native), Err(EvalError::NotJson(native.clone())));
        // JSON has no infinity: a number too big for a float cannot be converted back
        let inf = parse_json("22e2222", Strings::Plain).unwrap();
        assert_eq!(inf, Object::Float(f64::INFINITY));
        assert_eq!(to_json(&inf), Err(EvalError::NotJson(inf.clone())));
        assert!(to_json(&Object::Float(f64::NAN)).is_err());
    }
}
