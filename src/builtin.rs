use num_bigint::{BigInt, BigUint};
use sha3::{Digest, Keccak256};

use crate::error::EvalError;
use crate::eval::Object;
use crate::json::{parse_bytes, parse_quantity};

/// Functions implemented natively. A name refers to a builtin only when it is not bound
/// in the scope, so builtins can be shadowed (e.g. `let len = 42;`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Len,
    Int,
    Bytes,
    Keccak,
    Selector,
}

impl Builtin {
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Builtin::Len),
            "int" => Some(Builtin::Int),
            "bytes" => Some(Builtin::Bytes),
            "keccak" => Some(Builtin::Keccak),
            "selector" => Some(Builtin::Selector),
            _ => None,
        }
    }

    pub fn name(&self) -> &'static str {
        match self {
            Builtin::Len => "len",
            Builtin::Int => "int",
            Builtin::Bytes => "bytes",
            Builtin::Keccak => "keccak",
            Builtin::Selector => "selector",
        }
    }

    pub(crate) fn call(&self, mut args: Vec<Object>) -> Result<Object, EvalError> {
        // all builtins take exactly one argument
        if args.len() != 1 {
            return Err(EvalError::ApplyArgsCount(1, args.len()));
        }
        let arg = args.remove(0);
        let invalid = |arg: Object| Err(EvalError::Argument(self.name(), arg));
        match (self, arg) {
            // number of items: characters of a string, bytes, list items or record fields
            (Builtin::Len, Object::Str(s)) => Ok(Object::Int(s.chars().count().into())),
            (Builtin::Len, Object::Bytes(b)) => Ok(Object::Int(b.len().into())),
            (Builtin::Len, Object::List(items)) => Ok(Object::Int(items.len().into())),
            (Builtin::Len, Object::Record(fields)) => Ok(Object::Int(fields.len().into())),

            // unsigned big-endian bytes (e.g. an ABI word), a decimal or a "0x..." hex string
            (Builtin::Int, Object::Int(x)) => Ok(Object::Int(x)),
            (Builtin::Int, Object::Bytes(b)) => {
                Ok(Object::Int(BigInt::from(BigUint::from_bytes_be(&b))))
            }
            (Builtin::Int, Object::Str(s)) => {
                let int = if s.starts_with("0x") {
                    parse_quantity(&s)
                } else {
                    s.parse().ok()
                };
                match int {
                    Some(int) => Ok(Object::Int(int)),
                    None => invalid(Object::Str(s)),
                }
            }

            // a "0x..." hex string is decoded, any other string is taken as UTF-8
            (Builtin::Bytes, Object::Bytes(b)) => Ok(Object::Bytes(b)),
            (Builtin::Bytes, Object::Str(s)) if s.starts_with("0x") => match parse_bytes(&s) {
                Some(b) => Ok(Object::Bytes(b)),
                None => invalid(Object::Str(s)),
            },
            (Builtin::Bytes, Object::Str(s)) => Ok(Object::Bytes(s.into_bytes())),

            // Keccak-256 of bytes or of a UTF-8 string
            (Builtin::Keccak, Object::Bytes(b)) => Ok(Object::Bytes(keccak(&b))),
            (Builtin::Keccak, Object::Str(s)) => Ok(Object::Bytes(keccak(s.as_bytes()))),

            // function selector: first 4 bytes of keccak of the signature
            (Builtin::Selector, Object::Str(s)) => {
                Ok(Object::Bytes(keccak(s.as_bytes())[..4].to_vec()))
            }

            (_, arg) => invalid(arg),
        }
    }
}

fn keccak(data: &[u8]) -> Vec<u8> {
    Keccak256::digest(data).to_vec()
}
