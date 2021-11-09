#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum Token {
    Literal(String),
    Identifier(String),
    Keyword(&'static str),
    Operator(&'static str),
    Delimiter(char),
    End,
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

pub(crate) const LET: &str = "let";
pub(crate) const IF: &str = "if";
pub(crate) const ELSE: &str = "else";
pub(crate) const RETURN: &str = "return";
pub(crate) const FN: &str = "fn";
pub(crate) const TRUE: &str = "true";
pub(crate) const FALSE: &str = "false";

pub(crate) const PLUS: &str = "+";
pub(crate) const DASH: &str = "-";
pub(crate) const STAR: &str = "*";
pub(crate) const SLASH: &str = "/";
pub(crate) const PERCENT: &str = "%";
pub(crate) const EQ: &str = "==";
pub(crate) const NE: &str = "!=";
pub(crate) const OR: &str = "||";
pub(crate) const AND: &str = "&&";
pub(crate) const BANG: &str = "!";
pub(crate) const LTE: &str = "<=";
pub(crate) const LT: &str = "<";
pub(crate) const GTE: &str = ">=";
pub(crate) const GT: &str = ">";
pub(crate) const BIND: &str = "=";

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
        || c == '&'
        || c == '|'
}

pub(crate) fn is_identifier(token: &str) -> bool {
    let mut chars = token.chars();
    let first_char_is_alphabetic_or_underscore = chars
        .next()
        .map(|c| c.is_ascii_alphabetic() || c == '_')
        .unwrap_or_default();
    first_char_is_alphabetic_or_underscore && chars.all(|c| c.is_ascii_alphanumeric())
}
