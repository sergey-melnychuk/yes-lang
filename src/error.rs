use std::fmt::{Display, Formatter};
use std::error::Error;

#[derive(Debug)]
pub(crate) enum TokenError {
    EOF,
    Unexpected(String),
}

impl Display for TokenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenError::EOF => write!(f, "Unexpected EOF"),
            TokenError::Unexpected(c) => write!(f, "Unexpected: '{}'", c)
        }
    }
}

impl Error for TokenError {}
