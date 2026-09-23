#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Literal(String),
    Str(String),
    Bytes(Vec<u8>),
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
            Token::Str(s) => s.len(),
            Token::Bytes(b) => b.len(),
            Token::Identifier(id) => id.len(),
            Token::Keyword(kw) => kw.len(),
            Token::Operator(op) => op.len(),
            Token::Delimiter(_) => 1,
            _ => 0,
        }
    }
}

/// Line and column (both starting from 1) in the source.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    /// Position of the char at `offset` (in chars) in `source`.
    pub(crate) fn of(source: &str, offset: usize) -> Self {
        let mut pos = Pos { line: 1, col: 1 };
        for c in source.chars().take(offset) {
            if c == '\n' {
                pos.line += 1;
                pos.col = 1;
            } else {
                pos.col += 1;
            }
        }
        pos
    }
}

impl Pos {
    /// Positions of the chars at `offsets` (in chars, ascending) in `source`, in one pass.
    pub(crate) fn all(source: &str, offsets: &[usize]) -> Vec<Self> {
        let mut positions = Vec::with_capacity(offsets.len());
        let mut pos = Pos { line: 1, col: 1 };
        let mut chars = source.chars();
        let mut at = 0;
        for &offset in offsets {
            while at < offset {
                match chars.next() {
                    Some('\n') => {
                        pos = Pos {
                            line: pos.line + 1,
                            col: 1,
                        }
                    }
                    Some(_) => pos.col += 1,
                    None => break,
                }
                at += 1;
            }
            positions.push(pos);
        }
        positions
    }
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

pub(crate) const LET: &str = "let";
pub(crate) const IF: &str = "if";
pub(crate) const ELSE: &str = "else";
pub(crate) const RETURN: &str = "return";
pub(crate) const FN: &str = "fn";
pub(crate) const TRUE: &str = "true";
pub(crate) const FALSE: &str = "false";
pub(crate) const NULL: &str = "null";
pub(crate) const IN: &str = "in";

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
pub(crate) const RANGE: &str = "..";

pub(crate) fn is_keyword(token: &str) -> bool {
    token == LET
        || token == IF
        || token == ELSE
        || token == RETURN
        || token == FN
        || token == TRUE
        || token == FALSE
        || token == NULL
        || token == IN
}

pub(crate) fn is_delimiter(c: char) -> bool {
    c == ';'
        || c == ','
        || c == ':'
        || c == '{'
        || c == '}'
        || c == '('
        || c == ')'
        || c == '['
        || c == ']'
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

/// Whether the name can be used as a variable name.
pub fn is_identifier(token: &str) -> bool {
    let mut chars = token.chars();
    let first_char_is_alphabetic_or_underscore = chars
        .next()
        .map(|c| c.is_ascii_alphabetic() || c == '_')
        .unwrap_or_default();
    first_char_is_alphabetic_or_underscore && chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}
