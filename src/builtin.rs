use std::rc::Rc;

use num_bigint::{BigInt, BigUint};
use sha3::{Digest, Keccak256};

use crate::error::EvalError;
use crate::eval::Object;
use crate::json::{parse_bytes, parse_quantity};

/// Calls a function value (a function or a builtin) with evaluated arguments: builtins
/// like `map` call functions, each interpreter provides its own way to do it.
pub(crate) type Caller<'a> = &'a dyn Fn(&Object, Vec<Object>) -> Result<Object, EvalError>;

/// Functions implemented natively. A name refers to a builtin only when it is not bound
/// in the scope, so builtins can be shadowed (e.g. `let len = 42;`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Builtin {
    Len,
    Int,
    Bytes,
    Keccak,
    Selector,
    Any,
    All,
    Map,
    Filter,
}

impl Builtin {
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        match name {
            "len" => Some(Builtin::Len),
            "int" => Some(Builtin::Int),
            "bytes" => Some(Builtin::Bytes),
            "keccak" => Some(Builtin::Keccak),
            "selector" => Some(Builtin::Selector),
            "any" => Some(Builtin::Any),
            "all" => Some(Builtin::All),
            "map" => Some(Builtin::Map),
            "filter" => Some(Builtin::Filter),
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
            Builtin::Any => "any",
            Builtin::All => "all",
            Builtin::Map => "map",
            Builtin::Filter => "filter",
        }
    }

    fn arity(&self) -> usize {
        match self {
            Builtin::Any | Builtin::All | Builtin::Map | Builtin::Filter => 2,
            _ => 1,
        }
    }

    pub(crate) fn call(&self, args: Vec<Object>, caller: Caller) -> Result<Object, EvalError> {
        if args.len() != self.arity() {
            return Err(EvalError::ApplyArgsCount(self.arity(), args.len()));
        }
        let mut args = args.into_iter();
        let arg = args.next().unwrap();
        let invalid = |arg: Object| Err(EvalError::Argument(self.name(), arg));
        if let Some(f) = args.next() {
            // higher-order: a list and a function of an item
            return match arg {
                Object::List(items) => self.call_for_each(&items, &f, caller),
                arg => invalid(arg),
            };
        }
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
                Some(b) => Ok(Object::Bytes(b.into())),
                None => invalid(Object::Str(s)),
            },
            (Builtin::Bytes, Object::Str(s)) => Ok(Object::Bytes(s.into_bytes().into())),

            // Keccak-256 of bytes or of a UTF-8 string
            (Builtin::Keccak, Object::Bytes(b)) => Ok(Object::Bytes(keccak(&b).into())),
            (Builtin::Keccak, Object::Str(s)) => Ok(Object::Bytes(keccak(s.as_bytes()).into())),

            // function selector: first 4 bytes of keccak of the signature
            (Builtin::Selector, Object::Str(s)) => {
                Ok(Object::Bytes(keccak(s.as_bytes())[..4].to_vec().into()))
            }

            (_, arg) => invalid(arg),
        }
    }
}

impl Builtin {
    fn call_for_each(
        &self,
        items: &[Object],
        f: &Object,
        caller: Caller,
    ) -> Result<Object, EvalError> {
        let test = |item: &Object| match caller(f, vec![item.clone()])? {
            Object::Bool(b) => Ok(b),
            obj => Err(EvalError::NotBoolean(obj)),
        };
        match self {
            // 'any' and 'all' stop at the first item that decides the result
            Builtin::Any => {
                for item in items {
                    if test(item)? {
                        return Ok(Object::Bool(true));
                    }
                }
                Ok(Object::Bool(false))
            }
            Builtin::All => {
                for item in items {
                    if !test(item)? {
                        return Ok(Object::Bool(false));
                    }
                }
                Ok(Object::Bool(true))
            }
            Builtin::Map => {
                let items = items
                    .iter()
                    .map(|item| caller(f, vec![item.clone()]))
                    .collect::<Result<_, _>>()?;
                Ok(Object::List(Rc::new(items)))
            }
            Builtin::Filter => {
                let mut selected = Vec::new();
                for item in items {
                    if test(item)? {
                        selected.push(item.clone());
                    }
                }
                Ok(Object::List(Rc::new(selected)))
            }
            _ => unreachable!("not a higher-order builtin: {}", self.name()),
        }
    }
}

fn keccak(data: &[u8]) -> Vec<u8> {
    Keccak256::digest(data).to_vec()
}
