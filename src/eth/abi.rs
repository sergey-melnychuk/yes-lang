//! Decoding of the Ethereum ABI: calldata of function calls and the logs of events.
//!
//! Decoding is strict: offsets and lengths must be within the data, and values must be
//! padded as the ABI requires, otherwise decoding fails. Nothing is allocated for a length
//! before it is known to fit into the data.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use num_bigint::{BigInt, BigUint};

use crate::eth::keccak;
use crate::eval::Object;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    Address,
    Bool,
    Uint(usize),
    Int(usize),
    FixedBytes(usize),
    Bytes,
    String,
    Array(Box<Type>),
    FixedArray(Box<Type>, usize),
    Tuple(Vec<Param>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Param {
    pub(crate) ty: Type,
    pub(crate) name: Option<String>,
    pub(crate) indexed: bool,
}

/// A function or an event: `transfer(address to, uint256 amount)`,
/// `Transfer(address indexed from, address indexed to, uint256 value)`.
#[derive(Debug)]
pub(crate) struct Signature {
    pub(crate) params: Vec<Param>,
    /// Keccak-256 of the canonical form (`transfer(address,uint256)`).
    pub(crate) hash: Vec<u8>,
}

thread_local! {
    // parsing a signature on every call would take longer than decoding
    static SIGNATURES: RefCell<HashMap<String, Rc<Signature>>> = RefCell::new(HashMap::new());
}

pub(crate) fn signature(text: &str) -> Result<Rc<Signature>, String> {
    if let Some(signature) = SIGNATURES.with(|cache| cache.borrow().get(text).cloned()) {
        return Ok(signature);
    }
    let mut parser = Parser::new(text);
    let name = parser.word();
    if name.is_empty() {
        return Err(format!("expected a name in '{}'", text));
    }
    parser.expect(b'(')?;
    let params = parser.params()?;
    parser.end()?;
    let canonical = format!("{}{}", name, canonical_params(&params));
    let signature = Rc::new(Signature {
        hash: keccak(canonical.as_bytes()),
        params,
    });
    SIGNATURES.with(|cache| {
        let mut cache = cache.borrow_mut();
        if cache.len() >= 1024 {
            cache.clear();
        }
        cache.insert(text.to_string(), signature.clone());
    });
    Ok(signature)
}

/// Parameter types: `(address,uint256)` or `address,uint256`, names are allowed.
pub(crate) fn params(text: &str) -> Result<Vec<Param>, String> {
    let wrapped;
    let text = match text.trim_start().strip_prefix('(') {
        Some(_) => text,
        None => {
            wrapped = format!("({})", text);
            &wrapped
        }
    };
    let mut parser = Parser::new(text);
    parser.expect(b'(')?;
    let params = parser.params()?;
    parser.end()?;
    Ok(params)
}

// tuples and arrays in tuples and arrays..., deeper than any real signature
const MAX_TYPE_NESTING: usize = 32;

struct Parser<'a> {
    text: &'a str,
    at: usize,
    nesting: usize,
}

impl<'a> Parser<'a> {
    fn new(text: &'a str) -> Self {
        Parser {
            text,
            at: 0,
            nesting: 0,
        }
    }

    fn skip_spaces(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_whitespace()) {
            self.at += 1;
        }
    }

    fn peek(&self) -> Option<u8> {
        self.text.as_bytes().get(self.at).copied()
    }

    fn word(&mut self) -> &'a str {
        self.skip_spaces();
        let start = self.at;
        while self
            .peek()
            .is_some_and(|c| c.is_ascii_alphanumeric() || c == b'_' || c == b'$')
        {
            self.at += 1;
        }
        &self.text[start..self.at]
    }

    fn expect(&mut self, c: u8) -> Result<(), String> {
        self.skip_spaces();
        if self.peek() == Some(c) {
            self.at += 1;
            Ok(())
        } else {
            Err(format!(
                "expected '{}' at {} in '{}'",
                c as char, self.at, self.text
            ))
        }
    }

    fn end(&mut self) -> Result<(), String> {
        self.skip_spaces();
        match self.peek() {
            None => Ok(()),
            Some(_) => Err(format!(
                "unexpected '{}' in '{}'",
                &self.text[self.at..],
                self.text
            )),
        }
    }

    // after '(' up to and including ')'
    fn params(&mut self) -> Result<Vec<Param>, String> {
        let mut params = Vec::new();
        self.skip_spaces();
        if self.peek() == Some(b')') {
            self.at += 1;
            return Ok(params);
        }
        loop {
            params.push(self.param()?);
            self.skip_spaces();
            match self.peek() {
                Some(b',') => self.at += 1,
                Some(b')') => {
                    self.at += 1;
                    return Ok(params);
                }
                _ => {
                    return Err(format!(
                        "expected ',' or ')' at {} in '{}'",
                        self.at, self.text
                    ));
                }
            }
        }
    }

    // a type, then optionally 'indexed' and a name
    fn param(&mut self) -> Result<Param, String> {
        let ty = self.ty()?;
        let mut param = Param {
            ty,
            name: None,
            indexed: false,
        };
        loop {
            let word = self.word();
            match word {
                "" => return Ok(param),
                "indexed" if !param.indexed && param.name.is_none() => param.indexed = true,
                // data locations of Solidity, as in 'string memory name'
                "memory" | "calldata" | "storage" => {}
                name if param.name.is_none() => param.name = Some(name.to_string()),
                word => return Err(format!("unexpected '{}' in '{}'", word, self.text)),
            }
        }
    }

    fn ty(&mut self) -> Result<Type, String> {
        self.nesting += 1;
        if self.nesting > MAX_TYPE_NESTING {
            return Err(format!("types nested too deep in '{}'", self.text));
        }
        let ty = self.nested_ty();
        self.nesting -= 1;
        ty
    }

    // a tuple type has components: an empty one would take no space in the data, so that
    // any number of them would "fit"
    fn tuple(&mut self) -> Result<Type, String> {
        let params = self.params()?;
        if params.is_empty() {
            return Err(format!("empty tuple in '{}'", self.text));
        }
        Ok(Type::Tuple(params))
    }

    fn nested_ty(&mut self) -> Result<Type, String> {
        self.skip_spaces();
        let mut ty = if self.peek() == Some(b'(') {
            self.at += 1;
            self.tuple()?
        } else {
            let word = self.word();
            if word == "tuple" && self.peek() == Some(b'(') {
                self.at += 1;
                self.tuple()?
            } else {
                elementary(word)
                    .ok_or_else(|| format!("unknown type '{}' in '{}'", word, self.text))?
            }
        };
        // array suffixes: 'T[]', 'T[3]', 'T[][2]'
        while self.peek() == Some(b'[') {
            self.at += 1;
            let size = self.word();
            self.expect(b']')?;
            self.nesting += 1;
            if self.nesting > MAX_TYPE_NESTING {
                return Err(format!("types nested too deep in '{}'", self.text));
            }
            ty = if size.is_empty() {
                Type::Array(Box::new(ty))
            } else {
                match size.parse::<usize>() {
                    Ok(k) if k > 0 => Type::FixedArray(Box::new(ty), k),
                    _ => return Err(format!("invalid array size '{}' in '{}'", size, self.text)),
                }
            };
        }
        Ok(ty)
    }
}

fn elementary(word: &str) -> Option<Type> {
    let bits = |n: &str| match n {
        "" => Some(256),
        n => n
            .parse::<usize>()
            .ok()
            .filter(|b| *b > 0 && *b <= 256 && b % 8 == 0),
    };
    Some(match word {
        "address" => Type::Address,
        "bool" => Type::Bool,
        "string" => Type::String,
        "bytes" => Type::Bytes,
        _ => {
            if let Some(n) = word.strip_prefix("uint") {
                Type::Uint(bits(n)?)
            } else if let Some(n) = word.strip_prefix("int") {
                Type::Int(bits(n)?)
            } else {
                let n = word.strip_prefix("bytes")?;
                Type::FixedBytes(n.parse().ok().filter(|n| (1..=32).contains(n))?)
            }
        }
    })
}

fn canonical(ty: &Type) -> String {
    match ty {
        Type::Address => "address".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Uint(bits) => format!("uint{}", bits),
        Type::Int(bits) => format!("int{}", bits),
        Type::FixedBytes(n) => format!("bytes{}", n),
        Type::Bytes => "bytes".to_string(),
        Type::String => "string".to_string(),
        Type::Array(ty) => format!("{}[]", canonical(ty)),
        Type::FixedArray(ty, k) => format!("{}[{}]", canonical(ty), k),
        Type::Tuple(params) => canonical_params(params),
    }
}

fn canonical_params(params: &[Param]) -> String {
    let types: Vec<String> = params.iter().map(|param| canonical(&param.ty)).collect();
    format!("({})", types.join(","))
}

pub(crate) fn is_dynamic(ty: &Type) -> bool {
    match ty {
        Type::Bytes | Type::String | Type::Array(_) => true,
        Type::FixedArray(ty, _) => is_dynamic(ty),
        Type::Tuple(params) => params.iter().any(|param| is_dynamic(&param.ty)),
        _ => false,
    }
}

// the size in the head of a tuple: static values are inline, dynamic ones are an offset
fn head_size(ty: &Type) -> usize {
    if is_dynamic(ty) {
        return 32;
    }
    // saturating: sizes come from the signature, which a program can make up
    match ty {
        Type::FixedArray(ty, k) => k.saturating_mul(head_size(ty)),
        Type::Tuple(params) => params.iter().fold(0, |size: usize, param| {
            size.saturating_add(head_size(&param.ty))
        }),
        _ => 32,
    }
}

/// A record when all the values have names, a list otherwise.
pub(crate) fn values(params: &[&Param], values: Vec<Object>) -> Object {
    if !params.is_empty() && params.iter().all(|param| param.name.is_some()) {
        let fields: BTreeMap<String, Object> = params
            .iter()
            .map(|param| param.name.clone().unwrap())
            .zip(values)
            .collect();
        Object::Record(Rc::new(fields))
    } else {
        Object::List(Rc::new(values))
    }
}

fn word(data: &[u8], at: usize) -> Result<&[u8], String> {
    at.checked_add(32)
        .and_then(|end| data.get(at..end))
        .ok_or_else(|| format!("data too short: {} bytes, a word at {}", data.len(), at))
}

// an offset or a length, which must fit into the data
fn size(data: &[u8], at: usize) -> Result<usize, String> {
    let word = word(data, at)?;
    let size = BigUint::from_bytes_be(word);
    usize::try_from(size)
        .ok()
        .filter(|size| *size <= data.len())
        .ok_or_else(|| format!("offset or length out of bounds at {}", at))
}

/// Decodes the values of a tuple, `data` starts at its encoding.
pub(crate) fn decode_tuple(types: &[&Type], data: &[u8]) -> Result<Vec<Object>, String> {
    let mut at = 0;
    let mut values = Vec::with_capacity(types.len());
    for ty in types {
        let value = if is_dynamic(ty) {
            let offset = size(data, at)?;
            decode(ty, &data[offset..])?
        } else {
            decode(ty, data.get(at..).ok_or("data too short")?)?
        };
        values.push(value);
        at = at.saturating_add(head_size(ty));
    }
    Ok(values)
}

/// Decodes a value, `data` starts at its encoding.
pub(crate) fn decode(ty: &Type, data: &[u8]) -> Result<Object, String> {
    crate::grow(|| decode_value(ty, data))
}

fn decode_value(ty: &Type, data: &[u8]) -> Result<Object, String> {
    let zeros = |bytes: &[u8]| bytes.iter().all(|b| *b == 0);
    match ty {
        Type::Address => {
            let word = word(data, 0)?;
            if !zeros(&word[..12]) {
                return Err("invalid address: not padded with zeros".to_string());
            }
            Ok(Object::Bytes(word[12..].to_vec().into()))
        }
        Type::Bool => match word(data, 0)? {
            word if zeros(&word[..31]) && word[31] <= 1 => Ok(Object::Bool(word[31] == 1)),
            _ => Err("invalid bool".to_string()),
        },
        Type::Uint(bits) => {
            let word = word(data, 0)?;
            if !zeros(&word[..32 - bits / 8]) {
                return Err(format!("invalid uint{}: too big", bits));
            }
            Ok(Object::Int(BigInt::from(BigUint::from_bytes_be(word))))
        }
        Type::Int(bits) => {
            let word = word(data, 0)?;
            // the bytes above the value repeat its sign bit
            let sign = if word[32 - bits / 8] & 0x80 != 0 {
                0xff
            } else {
                0
            };
            if !word[..32 - bits / 8].iter().all(|b| *b == sign) {
                return Err(format!("invalid int{}: out of range", bits));
            }
            Ok(Object::Int(BigInt::from_signed_bytes_be(word)))
        }
        Type::FixedBytes(n) => {
            let word = word(data, 0)?;
            if !zeros(&word[*n..]) {
                return Err(format!("invalid bytes{}: not padded with zeros", n));
            }
            Ok(Object::Bytes(word[..*n].to_vec().into()))
        }
        Type::Bytes | Type::String => {
            let len = size(data, 0)?;
            let bytes = data
                .get(32..32 + len)
                .ok_or_else(|| format!("data too short for {} bytes", len))?;
            match ty {
                Type::String => match std::str::from_utf8(bytes) {
                    Ok(s) => Ok(Object::Str(s.to_string())),
                    Err(_) => Err("invalid string: not UTF-8".to_string()),
                },
                _ => Ok(Object::Bytes(bytes.to_vec().into())),
            }
        }
        Type::Array(item) => {
            let len = size(data, 0)?;
            let items = &data[32..];
            // each item takes space, so a huge length cannot fit
            if len.saturating_mul(head_size(item).max(1)) > items.len() {
                return Err(format!("data too short for {} items", len));
            }
            let types = vec![item.as_ref(); len];
            Ok(Object::List(Rc::new(decode_tuple(&types, items)?)))
        }
        Type::FixedArray(item, k) => {
            if k.saturating_mul(head_size(item).max(1)) > data.len() {
                return Err(format!("data too short for {} items", k));
            }
            let types = vec![item.as_ref(); *k];
            Ok(Object::List(Rc::new(decode_tuple(&types, data)?)))
        }
        Type::Tuple(params) => {
            let types: Vec<&Type> = params.iter().map(|param| &param.ty).collect();
            let params: Vec<&Param> = params.iter().collect();
            Ok(values(&params, decode_tuple(&types, data)?))
        }
    }
}
