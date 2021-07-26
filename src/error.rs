use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::token::Token;

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
    Unexpected(String, Token),
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::EOF => write!(f, "Unexpected EOF"),
            ParserError::Unexpected(expected, got) => write!(f, "Expected '{}' but got '{:?}'", expected, got)
        }
    }
}

impl Error for ParserError {}
