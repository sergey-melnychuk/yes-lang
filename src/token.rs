#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Token {
    Literal(String),
    Identifier(String),
    Keyword(&'static str),
    Operator(&'static str),
    Delimiter(char),
    EOF,
}

impl Token {
    pub(crate) fn len(&self) -> usize {
        match self {
            Token::Literal(lit) => lit.len(),
            Token::Identifier(id) => id.len(),
            Token::Keyword(kw) => kw.len(),
            Token::Operator(op) => op.len(),
            Token::Delimiter(_) => 1,
            _ => 0,
        }
    }
}
