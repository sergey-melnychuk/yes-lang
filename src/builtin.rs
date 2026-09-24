//! The builtins of the language: natives available in every program. A name refers to a
//! builtin only when nothing is bound to it, so builtins can be shadowed (`let len = 42;`).

use std::collections::HashMap;
use std::rc::Rc;

use num_bigint::{BigInt, BigUint};

use crate::error::EvalError;
use crate::eval::Object;
use crate::hex::{encode, parse_bytes, parse_quantity};
use crate::native::{Caller, Native};

thread_local! {
    static BUILTINS: HashMap<&'static str, Rc<Native>> = builtins()
        .into_iter()
        .map(|native| (native.name(), Rc::new(native)))
        .collect();
}

/// The builtin with this name.
pub(crate) fn builtin(name: &str) -> Option<Rc<Native>> {
    BUILTINS.with(|builtins| builtins.get(name).cloned())
}

fn builtins() -> Vec<Native> {
    vec![
        // number of items: characters of a string, bytes, list items or record fields
        Native::pure("len", 1, |args| match &args[0] {
            Object::Str(s) => Ok(Object::Int(s.chars().count().into())),
            Object::Bytes(b) => Ok(Object::Int(b.len().into())),
            Object::List(items) => Ok(Object::Int(items.len().into())),
            Object::Record(fields) => Ok(Object::Int(fields.len().into())),
            arg => Err(EvalError::Argument("len", arg.clone())),
        }),
        // unsigned big-endian bytes (e.g. an ABI word), a decimal or a "0x..." hex string
        Native::pure("int", 1, |args| {
            let int = match &args[0] {
                Object::Int(x) => Some(x.clone()),
                Object::Bytes(b) => Some(BigInt::from(BigUint::from_bytes_be(b))),
                Object::Str(s) if s.starts_with("0x") => parse_quantity(s),
                Object::Str(s) => s.parse().ok(),
                _ => None,
            };
            int.map(Object::Int)
                .ok_or_else(|| EvalError::Argument("int", args[0].clone()))
        }),
        // a "0x..." hex string is decoded, any other string is taken as UTF-8
        Native::pure("bytes", 1, |args| match &args[0] {
            Object::Bytes(b) => Ok(Object::Bytes(b.clone())),
            Object::Str(s) if s.starts_with("0x") => parse_bytes(s)
                .map(|b| Object::Bytes(b.into()))
                .ok_or_else(|| EvalError::Argument("bytes", args[0].clone())),
            Object::Str(s) => Ok(Object::Bytes(s.as_bytes().to_vec().into())),
            arg => Err(EvalError::Argument("bytes", arg.clone())),
        }),
        // bytes as a "0x..." hex string, e.g. to look up an address in a record
        Native::pure("hex", 1, |args| match &args[0] {
            Object::Bytes(b) => Ok(Object::Str(encode(b))),
            arg => Err(EvalError::Argument("hex", arg.clone())),
        }),
        // a list and a function of an item; 'any' and 'all' stop at the first item that
        // decides the result
        Native::new("any", 2, |args, call| {
            for item in list("any", &args[0])? {
                if test(call, &args[1], item)? {
                    return Ok(Object::Bool(true));
                }
            }
            Ok(Object::Bool(false))
        }),
        Native::new("all", 2, |args, call| {
            for item in list("all", &args[0])? {
                if !test(call, &args[1], item)? {
                    return Ok(Object::Bool(false));
                }
            }
            Ok(Object::Bool(true))
        }),
        Native::new("map", 2, |args, call| {
            let items = list("map", &args[0])?
                .iter()
                .map(|item| call(&args[1], vec![item.clone()]))
                .collect::<Result<_, _>>()?;
            Ok(Object::List(Rc::new(items)))
        }),
        Native::new("filter", 2, |args, call| {
            let mut selected = Vec::new();
            for item in list("filter", &args[0])? {
                if test(call, &args[1], item)? {
                    selected.push(item.clone());
                }
            }
            Ok(Object::List(Rc::new(selected)))
        }),
    ]
}

fn list<'a>(name: &'static str, arg: &'a Object) -> Result<&'a [Object], EvalError> {
    match arg {
        Object::List(items) => Ok(items),
        arg => Err(EvalError::Argument(name, arg.clone())),
    }
}

fn test(call: Caller, f: &Object, item: &Object) -> Result<bool, EvalError> {
    match call(f, vec![item.clone()])? {
        Object::Bool(b) => Ok(b),
        obj => Err(EvalError::NotBoolean(obj)),
    }
}
