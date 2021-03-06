use std::fmt::{Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

use crate::eval::Object;
use crate::parser::{Expression, Operator};
use crate::token::Token;

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) enum TokenError {
    Eof,
    Unexpected(String),
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::Eof => write!(f, "Unexpected EOF"),
            TokenError::Unexpected(c) => write!(f, "Unexpected: '{}'", c),
        }
    }
}

impl std::error::Error for TokenError {}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum ParserError {
    Unexpected(Token),
    Mismatch(Token, Token),
    Prefix(Token),
    Infix(Token),
    Op(Operator, Expression),
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
            ParserError::Op(op, expr) => write!(f, "Cannot parse {:?} from {:?}", op, expr),
        }
    }
}

impl std::error::Error for ParserError {}

#[derive(Debug, PartialEq)]
pub(crate) enum EvalError {
    ParseInt(ParseIntError),
    ParseFloat(ParseFloatError),
    NotFound(String),
    InfixOp(Operator, Object, Object),
    PrefixOp(Operator, Object),
    NotBoolean(Object),
    NotFunction(String),
    Apply(Expression),
    ApplyArgsCount(usize, usize),
    InvalidLiteral(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::ParseInt(e) => write!(f, "Parsing integer failed: {}", e),
            EvalError::ParseFloat(e) => write!(f, "Parsing float failed: {}", e),
            EvalError::NotFound(name) => write!(f, "Not found: '{}'", name),
            EvalError::InfixOp(op, lhs, rhs) => {
                write!(f, "Cannot apply: ({:?}, {:?}, {:?})", op, lhs, rhs)
            }
            EvalError::PrefixOp(op, obj) => write!(f, "Cannot apply: ({:?}, {:?})", op, obj),
            EvalError::NotBoolean(obj) => write!(f, "Expected boolean but got '{}'", obj),
            EvalError::NotFunction(obj) => write!(f, "'{}' is not a function", obj),
            EvalError::Apply(expr) => write!(f, "Cannot apply: '{:?}'", expr),
            EvalError::ApplyArgsCount(exp, got) => write!(
                f,
                "Cannot apply: expected number of arguments: {}, got: {}",
                exp, got
            ),
            EvalError::InvalidLiteral(lit) => write!(f, "Invalid literal: '{}'", lit),
        }
    }
}

impl std::error::Error for EvalError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            EvalError::ParseInt(e) => Some(e),
            EvalError::ParseFloat(e) => Some(e),
            _ => None,
        }
    }
}

impl From<ParseIntError> for EvalError {
    fn from(e: ParseIntError) -> Self {
        Self::ParseInt(e)
    }
}

impl From<ParseFloatError> for EvalError {
    fn from(e: ParseFloatError) -> Self {
        Self::ParseFloat(e)
    }
}

#[derive(Debug)]
pub(crate) enum Error {
    Lexer(TokenError),
    Parser(ParserError),
    Eval(EvalError),
    IO(std::io::Error),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Lexer(e) => write!(f, "{}", e),
            Error::Parser(e) => write!(f, "{}", e),
            Error::Eval(e) => write!(f, "{}", e),
            Error::IO(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<TokenError> for Error {
    fn from(e: TokenError) -> Self {
        Self::Lexer(e)
    }
}

impl From<ParserError> for Error {
    fn from(e: ParserError) -> Self {
        Self::Parser(e)
    }
}

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
