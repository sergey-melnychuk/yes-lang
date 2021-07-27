use crate::token::Token;
use std::error::Error;
use std::fmt::{Display, Formatter};

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
