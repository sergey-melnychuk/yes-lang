use std::collections::BTreeMap;
use std::rc::Rc;

use num_bigint::BigInt;
use serde::de::{Deserialize, Deserializer, MapAccess, SeqAccess, Visitor};
use serde_json::Value;

use crate::error::EvalError;
use crate::eval::{Bytes, Object};

fn hex_digit(c: u8) -> Option<u8> {
    match c {
        b'0'..=b'9' => Some(c - b'0'),
        b'a'..=b'f' => Some(c - b'a' + 10),
        b'A'..=b'F' => Some(c - b'A' + 10),
        _ => None,
    }
}

/// Decodes an even number of hex digits (without '0x').
pub(crate) fn decode_hex(hex: &str) -> Option<Vec<u8>> {
    let hex = hex.as_bytes();
    if !hex.len().is_multiple_of(2) {
        return None;
    }
    hex.as_chunks::<2>()
        .0
        .iter()
        .map(|pair| Some(hex_digit(pair[0])? << 4 | hex_digit(pair[1])?))
        .collect()
}

/// Decodes a "0x..." hex string like `parse_bytes`, into a single allocation.
fn parse_shared_bytes(s: &str) -> Option<Bytes> {
    let hex = s.strip_prefix("0x")?.as_bytes();
    if !hex.iter().all(u8::is_ascii_hexdigit) {
        return None;
    }
    let digit = |c: u8| hex_digit(c).expect("validated");
    // an odd number of digits: the first byte has one digit
    let (first, rest) = hex.split_at(hex.len() % 2);
    // with an exact size known upfront, collecting allocates once
    let bytes: Rc<[u8]> = first
        .iter()
        .map(|&c| digit(c))
        .chain(
            rest.as_chunks::<2>()
                .0
                .iter()
                .map(|[hi, lo]| digit(*hi) << 4 | digit(*lo)),
        )
        .collect();
    Some(bytes.into())
}

/// Decodes a "0x..." hex string, an odd number of digits is padded with a leading zero
/// (numbers are written like that, e.g. "0x1").
pub(crate) fn parse_bytes(s: &str) -> Option<Vec<u8>> {
    let hex = s.strip_prefix("0x")?;
    if hex.len().is_multiple_of(2) {
        decode_hex(hex)
    } else {
        decode_hex(&format!("0{}", hex))
    }
}

pub(crate) fn parse_quantity(s: &str) -> Option<BigInt> {
    let hex = s.strip_prefix("0x")?;
    // from_str_radix and parse_bytes would also accept a sign or '_'
    if hex.is_empty() || !hex.bytes().all(|c| c.is_ascii_hexdigit()) {
        return None;
    }
    // fast path: most quantities (gas, nonce, ...) fit into 64 bits
    if hex.len() <= 16 {
        return u64::from_str_radix(hex, 16).ok().map(BigInt::from);
    }
    BigInt::parse_bytes(hex.as_bytes(), 16)
}

/// A value that can be bound to a name with `Context::bind`: JSON text, a parsed JSON value
/// (both converted with `from_json`), or an object as it is.
pub trait IntoObject {
    fn into_object(self) -> Result<Object, serde_json::Error>;
}

impl IntoObject for &str {
    fn into_object(self) -> Result<Object, serde_json::Error> {
        // directly into an object, without building a `serde_json::Value` first
        Ok(serde_json::from_str::<Json>(self)?.0)
    }
}

impl IntoObject for String {
    fn into_object(self) -> Result<Object, serde_json::Error> {
        self.as_str().into_object()
    }
}

impl IntoObject for &Value {
    fn into_object(self) -> Result<Object, serde_json::Error> {
        Ok(from_json(self))
    }
}

impl IntoObject for Value {
    fn into_object(self) -> Result<Object, serde_json::Error> {
        Ok(from_json(&self))
    }
}

impl IntoObject for Object {
    fn into_object(self) -> Result<Object, serde_json::Error> {
        Ok(self)
    }
}

/// An object deserialized directly from JSON, the same as `from_json` would give.
struct Json(Object);

// with the 'arbitrary_precision' feature, serde_json passes a number to a deserializer
// as a map with this single key and the number as a string
const NUMBER_TOKEN: &str = "$serde_json::private::Number";

fn number(text: &str) -> Object {
    match text.parse::<BigInt>() {
        Ok(int) => Object::Int(int),
        Err(_) => Object::Float(text.parse().unwrap_or(f64::NAN)),
    }
}

fn string(s: &str) -> Object {
    match parse_shared_bytes(s) {
        Some(bytes) => Object::Bytes(bytes),
        None => Object::Str(s.to_string()),
    }
}

impl<'de> Deserialize<'de> for Json {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_any(JsonVisitor).map(Json)
    }
}

struct JsonVisitor;

impl<'de> Visitor<'de> for JsonVisitor {
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
        Ok(string(s))
    }

    fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Object, A::Error> {
        let mut items = Vec::with_capacity(seq.size_hint().unwrap_or(0));
        while let Some(Json(item)) = seq.next_element()? {
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
            let Json(value) = map.next_value()?;
            fields.insert(name, value);
        }
        Ok(Object::Record(Rc::new(fields)))
    }
}

/// Converts JSON into an object: every "0x..." hex string is decoded into `Bytes`
/// (use `int(...)` for numbers), any other string is kept as `Str`.
pub fn from_json(value: &Value) -> Object {
    match value {
        Value::Null => Object::Null,
        Value::Bool(b) => Object::Bool(*b),
        // with 'arbitrary_precision' the number keeps its exact text
        Value::Number(n) => number(&n.to_string()),
        Value::String(s) => string(s),
        Value::Array(items) => Object::List(Rc::new(items.iter().map(from_json).collect())),
        Value::Object(fields) => Object::Record(Rc::new(
            fields
                .iter()
                .map(|(name, value)| (name.clone(), from_json(value)))
                .collect::<BTreeMap<_, _>>(),
        )),
    }
}

/// Converts an object into JSON: bytes become "0x..." strings, integers keep their
/// precision. Functions cannot be converted.
pub fn to_json(obj: &Object) -> Result<Value, EvalError> {
    Ok(match obj {
        Object::Null => Value::Null,
        Object::Bool(b) => Value::Bool(*b),
        Object::Int(x) => serde_json::from_str(&x.to_string()).expect("integer is valid JSON"),
        Object::Float(x) => serde_json::Number::from_f64(*x).map_or(Value::Null, Value::Number),
        Object::Str(s) => Value::String(s.clone()),
        Object::Bytes(b) => {
            let hex: String = b.iter().map(|byte| format!("{:02x}", byte)).collect();
            Value::String(format!("0x{}", hex))
        }
        Object::List(items) => Value::Array(items.iter().map(to_json).collect::<Result<_, _>>()?),
        Object::Record(fields) => Value::Object(
            fields
                .iter()
                .map(|(name, value)| Ok((name.clone(), to_json(value)?)))
                .collect::<Result<_, EvalError>>()?,
        ),
        Object::Func(_) | Object::Builtin(_) => return Err(EvalError::NotJson(obj.clone())),
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
            assert_eq!(from_json(&json), expected, "{}", json);
            // deserialized directly from the text, the object is the same
            assert_eq!(
                json.to_string().into_object().unwrap(),
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
            assert_eq!(text.into_object().unwrap(), from_json(&value), "{}", text);
        }
        // the transactions of a block
        for line in std::fs::read_to_string("examples/block.ndjson")
            .unwrap()
            .lines()
        {
            let value: Value = serde_json::from_str(line).unwrap();
            assert_eq!(line.into_object().unwrap(), from_json(&value));
        }
        assert!("{".into_object().is_err());
        assert!("1 2".into_object().is_err());
    }

    #[test]
    fn test_to_json() {
        let src =
            r#"{"a":[1,-2.5,true,null,"s","0x00ff"],"big":123456789012345678901234567890,"b":{}}"#;
        let obj = from_json(&serde_json::from_str(src).unwrap());
        // keys are sorted
        let expected =
            r#"{"a":[1,-2.5,true,null,"s","0x00ff"],"b":{},"big":123456789012345678901234567890}"#;
        assert_eq!(to_json(&obj).unwrap().to_string(), expected);
        assert_eq!(
            to_json(&Object::Builtin(crate::Builtin::Len)),
            Err(EvalError::NotJson(Object::Builtin(crate::Builtin::Len)))
        );
    }
}
