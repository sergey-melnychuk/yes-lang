use crate::token::Token;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::num::{ParseIntError, ParseFloatError};

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

impl Error for TokenError {}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum ParserError {
    EOF,
    Token(Token, Token),
    Prefix(Token),
    Infix(Token),
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
        }
    }
}

impl Error for ParserError {}

#[derive(Debug)]
pub(crate) enum EvalError {
    ParseInt(ParseIntError),
    ParseFloat(ParseFloatError),
    NotFound(String),
    Other(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::ParseInt(e) => write!(f, "Parsing integer failed: {}", e),
            EvalError::ParseFloat(e) => write!(f, "Parsing float failed: {}", e),
            EvalError::NotFound(name) => write!(f, "Not found: '{}'", name),
            EvalError::Other(msg) => write!(f, "Evaluation failed: {}", msg)
        }
    }
}

impl Error for EvalError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
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
