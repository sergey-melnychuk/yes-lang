use crate::token::Token;
use std::fmt::{Display, Formatter};
use std::num::{ParseIntError, ParseFloatError};
use crate::parser::{Operator, Expression};
use crate::eval::Object;

#[allow(dead_code)]
#[derive(Debug)]
pub(crate) enum TokenError {
    EOF,
    Unexpected(String),
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::EOF => write!(f, "Unexpected EOF"),
            TokenError::Unexpected(c) => write!(f, "Unexpected: '{}'", c),
        }
    }
}

impl std::error::Error for TokenError {}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum ParserError {
    EOF,
    Token(Token, Token),
    Prefix(Token),
    Infix(Token),
    Op(Operator, Expression),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EOF => write!(f, "Unexpected EOF"),
            ParserError::Token(expected, got) => {
                write!(f, "Expected '{:?}' but got '{:?}'", expected, got)
            }
            ParserError::Prefix(token) => write!(f, "No prefix version of '{:?}' exists", token),
            ParserError::Infix(token) => write!(f, "No infix version of '{:?}' exists", token),
            ParserError::Op(op, expr) => write!(f, "Cannot parse {:?} from {:?}", op, expr)
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
    Other(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::ParseInt(e) =>
                write!(f, "Parsing integer failed: {}", e),
            EvalError::ParseFloat(e) =>
                write!(f, "Parsing float failed: {}", e),
            EvalError::NotFound(name) =>
                write!(f, "Not found: '{}'", name),
            EvalError::InfixOp(op, lhs, rhs) =>
                write!(f, "Cannot apply: '{:?}' {:?} '{:?}'", op, lhs, rhs),
            EvalError::PrefixOp(op, obj) =>
                write!(f, "Cannot apply: {:?} '{:?}'", op, obj),
            EvalError::Other(msg) =>
                write!(f, "Evaluation failed: {}", msg)
        }
    }
}

impl std::error::Error for EvalError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            EvalError::ParseInt(e) => Some(e),
            EvalError::ParseFloat(e) => Some(e),
            _ => None
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
    Token(TokenError),
    Parser(ParserError),
    Eval(EvalError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Token(e) => write!(f, "{}", e),
            Error::Parser(e) => write!(f, "{}", e),
            Error::Eval(e) => write!(f, "{}", e),
        }
    }
}

impl std::error::Error for Error {}

impl From<TokenError> for Error {
    fn from(e: TokenError) -> Self {
        Self::Token(e)
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