use std::fmt::{Display, Formatter};

use num_bigint::BigInt;

use crate::eval::Object;
use crate::parser::Operator;
use crate::token::{Pos, Token};

#[derive(Debug, Eq, PartialEq)]
pub enum TokenError {
    Unexpected(String),
    Unterminated(&'static str),
    InvalidBytes(String),
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::Unexpected(c) => write!(f, "Unexpected: '{}'", c),
            TokenError::Unterminated(what) => write!(f, "Unterminated {}", what),
            TokenError::InvalidBytes(hex) => write!(f, "Invalid hex bytes: b\"{}\"", hex),
        }
    }
}

impl std::error::Error for TokenError {}

#[derive(Debug, Eq, PartialEq)]
pub enum ParserError {
    Unexpected(Token),
    Mismatch(Token, Token),
    Prefix(Token),
    Infix(Token),
    InvalidLiteral(String),
    DuplicateKey(String),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::Unexpected(token) => write!(f, "Unexpected '{:?}'", token),
            ParserError::Mismatch(expected, got) => {
                write!(f, "Expected '{:?}' but got '{:?}'", expected, got)
            }
            ParserError::Prefix(token) => write!(f, "No prefix version of '{:?}' exists", token),
            ParserError::Infix(token) => write!(f, "No infix version of '{:?}' exists", token),
            ParserError::InvalidLiteral(lit) => write!(f, "Invalid literal: '{}'", lit),
            ParserError::DuplicateKey(key) => write!(f, "Duplicate key: '{}'", key),
        }
    }
}

impl std::error::Error for ParserError {}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    NotFound(String),
    InfixOp(Operator, Object, Object),
    PrefixOp(Operator, Object),
    NotBoolean(Object),
    NotFunction(Object),
    Argument(&'static str, Object),
    ApplyArgsCount(usize, usize),
    DivisionByZero,
    StackOverflow(usize),
    CallLimit(u64),
    NotJson(Object),
    Index(Object, Object),
    IndexOutOfBounds(BigInt, usize),
    Field(Object, String),
    /// An error at a position in the source.
    At(Box<EvalError>, Pos),
}

impl EvalError {
    /// Adds the position, unless the error already has one (the innermost position is kept).
    pub(crate) fn at(self, pos: Pos) -> Self {
        match self {
            EvalError::At(..) => self,
            e => EvalError::At(Box::new(e), pos),
        }
    }

    pub fn position(&self) -> Option<Pos> {
        match self {
            EvalError::At(_, pos) => Some(*pos),
            _ => None,
        }
    }

    /// The error without its position.
    pub fn without_position(self) -> Self {
        match self {
            EvalError::At(e, _) => *e,
            e => e,
        }
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::NotFound(name) => write!(f, "Not found: '{}'", name),
            EvalError::InfixOp(op, lhs, rhs) => {
                write!(f, "Cannot apply: ({:?}, {:?}, {:?})", op, lhs, rhs)
            }
            EvalError::PrefixOp(op, obj) => write!(f, "Cannot apply: ({:?}, {:?})", op, obj),
            EvalError::NotBoolean(obj) => write!(f, "Expected boolean but got '{}'", obj),
            EvalError::NotFunction(obj) => write!(f, "'{}' is not a function", obj),
            EvalError::Argument(name, obj) => {
                write!(f, "Invalid argument of '{}': '{}'", name, obj)
            }
            EvalError::ApplyArgsCount(exp, got) => write!(
                f,
                "Cannot apply: expected number of arguments: {}, got: {}",
                exp, got
            ),
            EvalError::DivisionByZero => write!(f, "Division by zero"),
            EvalError::StackOverflow(depth) => {
                write!(f, "Stack overflow: call depth exceeds {}", depth)
            }
            EvalError::CallLimit(calls) => write!(f, "Call limit exceeded: {} calls", calls),
            EvalError::NotJson(obj) => write!(f, "Cannot convert '{}' to JSON", obj),
            EvalError::At(e, pos) => write!(f, "{} at {}", e, pos),
            EvalError::Index(obj, idx) => write!(f, "Cannot index '{}' with '{}'", obj, idx),
            EvalError::IndexOutOfBounds(idx, len) => {
                write!(f, "Index out of bounds: {} (length: {})", idx, len)
            }
            EvalError::Field(obj, name) => write!(f, "Cannot access '{}' of '{}'", name, obj),
        }
    }
}

impl std::error::Error for EvalError {}

#[derive(Debug)]
pub enum Error {
    Lexer(TokenError, Pos),
    Parser(ParserError, Pos),
    Eval(EvalError),
    IO(std::io::Error),
    Json(serde_json::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Lexer(e, pos) => write!(f, "{} at {}", e, pos),
            Error::Parser(e, pos) => write!(f, "{} at {}", e, pos),
            Error::Eval(e) => write!(f, "{}", e),
            Error::IO(e) => write!(f, "{}", e),
            Error::Json(e) => write!(f, "Invalid JSON: {}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<EvalError> for Error {
    fn from(e: EvalError) -> Self {
        Self::Eval(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::IO(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Self::Json(e)
    }
}
