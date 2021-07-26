#[derive(Debug, Eq, PartialEq, Clone)]
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

pub(crate) const LET: &'static str = "let";
pub(crate) const IF: &'static str = "if";
pub(crate) const ELSE: &'static str = "else";
pub(crate) const RETURN: &'static str = "return";
pub(crate) const FN: &'static str = "fn";
pub(crate) const TRUE: &'static str = "true";
pub(crate) const FALSE: &'static str = "false";

pub(crate) const ADD: &'static str = "+";
pub(crate) const SUB: &'static str = "-";
pub(crate) const MUL: &'static str = "*";
pub(crate) const DIV: &'static str = "/";
pub(crate) const MOD: &'static str = "%";
pub(crate) const EQ: &'static str = "==";
pub(crate) const NE: &'static str = "!=";
pub(crate) const NOT: &'static str = "!";
pub(crate) const LTE: &'static str = "<=";
pub(crate) const LT: &'static str = "<";
pub(crate) const GTE: &'static str = ">=";
pub(crate) const GT: &'static str = ">";
pub(crate) const BIND: &'static str = "=";

pub(crate) fn is_keyword(token: &str) -> bool {
    token == LET
        || token == IF
        || token == ELSE
        || token == RETURN
        || token == FN
        || token == TRUE
        || token == FALSE
}

pub(crate) fn is_delimiter(c: char) -> bool {
    c == ';' || c == ',' || c == '{' || c == '}' || c == '(' || c == ')'
}

pub(crate) fn is_operator(c: char) -> bool {
    c == '+'
        || c == '-'
        || c == '*'
        || c == '/'
        || c == '%'
        || c == '='
        || c == '!'
        || c == '>'
        || c == '<'
}

pub(crate) fn is_identifier(token: &str) -> bool {
    let mut chars = token.chars();
    let first_char_is_alphabetic_or_underscore = chars
        .next()
        .map(|c| c.is_ascii_alphabetic() || c == '_')
        .unwrap_or_default();
    first_char_is_alphabetic_or_underscore && chars.all(|c| c.is_ascii_alphanumeric())
}
