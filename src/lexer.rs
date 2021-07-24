use crate::error::TokenError;
use crate::token::Token;

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

fn is_keyword(token: &str) -> bool {
    token == LET
        || token == IF
        || token == ELSE
        || token == RETURN
        || token == FN
        || token == TRUE
        || token == FALSE
}

fn is_delimiter(c: char) -> bool {
    c == ';' || c == ',' || c == '{' || c == '}' || c == '(' || c == ')'
}

fn is_operator(c: char) -> bool {
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

fn is_quoted(token: &str) -> bool {
    token.starts_with('\"') && token.ends_with('\"')
}

fn is_identifier(token: &str) -> bool {
    let mut chars = token.chars();
    let first_char_is_alphabetic_or_underscore = chars
        .next()
        .map(|c| c.is_ascii_alphabetic() || c == '_')
        .unwrap_or_default();
    first_char_is_alphabetic_or_underscore && chars.all(|c| c.is_ascii_alphanumeric())
}

fn match_operator(next: char, peek: Option<char>) -> Result<Token, TokenError> {
    match (next, peek) {
        ('+', _) => Ok(Token::Operator(ADD)),
        ('-', _) => Ok(Token::Operator(SUB)),
        ('*', _) => Ok(Token::Operator(MUL)),
        ('/', _) => Ok(Token::Operator(DIV)),
        ('%', _) => Ok(Token::Operator(MOD)),
        ('>', Some('=')) => Ok(Token::Operator(GTE)),
        ('>', _) => Ok(Token::Operator(GT)),
        ('<', Some('=')) => Ok(Token::Operator(LTE)),
        ('<', _) => Ok(Token::Operator(LT)),
        ('!', Some('=')) => Ok(Token::Operator(NE)),
        ('!', _) => Ok(Token::Operator(NOT)),
        ('=', Some('=')) => Ok(Token::Operator(EQ)),
        ('=', _) => Ok(Token::Operator(BIND)),
        _ => Err(TokenError::Unexpected(format!("{}", next))),
    }
}

fn match_id_or_literal(this: &mut String, tokens: &mut Vec<Token>) {
    if this.is_empty() {
        return;
    }
    if is_identifier(&this) {
        let token = Token::Identifier(this.to_string());
        tokens.push(token);
        this.clear();
    } else if !this.is_empty() {
        let token = Token::Literal(this.to_string());
        tokens.push(token);
        this.clear();
    }
}

pub(crate) trait Iter<T> {
    fn next(&mut self) -> Option<T>;
    fn peek(&self) -> Option<T>;
}

pub(crate) fn tokenize<I: Iter<char>>(buf: &mut I) -> Result<Vec<Token>, TokenError> {
    let mut tokens = Vec::new();

    let mut this = String::with_capacity(16);
    loop {
        let next = {
            if let Some(chr) = buf.next() {
                chr
            } else {
                match_id_or_literal(&mut this, &mut tokens);
                tokens.push(Token::EOF);
                break;
            }
        };

        if next.is_whitespace() || is_delimiter(next) || is_operator(next) {
            if is_keyword(&this) {
                let token = match this.as_ref() {
                    LET => Token::Keyword(LET),
                    IF => Token::Keyword(IF),
                    ELSE => Token::Keyword(ELSE),
                    RETURN => Token::Keyword(RETURN),
                    FN => Token::Keyword(FN),
                    TRUE => Token::Keyword(TRUE),
                    FALSE => Token::Keyword(FALSE),
                    unexpected => return Err(TokenError::Unexpected(unexpected.to_string())),
                };
                tokens.push(token);
                this.clear();
            }

            match_id_or_literal(&mut this, &mut tokens);
        } else {
            this.push(next);
        }

        if is_operator(next) {
            let token = match_operator(next, buf.peek())?;
            if token.len() > 1 {
                let _ = buf.next();
            }
            tokens.push(token);
        }

        if is_delimiter(next) {
            let token = Token::Delimiter(next);
            tokens.push(token);
        }
    }

    Ok(tokens)
}

pub(crate) struct Buf(String, usize);

impl Buf {
    pub(crate) fn new(s: &str) -> Self {
        Self(s.to_string(), 0)
    }
}

impl Iter<char> for Buf {
    fn next(&mut self) -> Option<char> {
        let c = self.0.chars().skip(self.1).next();
        if self.1 < self.0.len() {
            self.1 += 1;
        }
        c
    }

    fn peek(&self) -> Option<char> {
        self.0.chars().skip(self.1).next()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenizer() {
        let tests = vec![
            (
                "let abc = 123;",
                vec![
                    Token::Keyword(LET),
                    Token::Identifier("abc".to_string()),
                    Token::Operator(BIND),
                    Token::Literal("123".to_string()),
                    Token::Delimiter(';'),
                    Token::EOF,
                ],
            ),
            (
                "fn(a, b) {a - b}",
                vec![
                    Token::Keyword(FN),
                    Token::Delimiter('('),
                    Token::Identifier("a".to_string()),
                    Token::Delimiter(','),
                    Token::Identifier("b".to_string()),
                    Token::Delimiter(')'),
                    Token::Delimiter('{'),
                    Token::Identifier("a".to_string()),
                    Token::Operator(SUB),
                    Token::Identifier("b".to_string()),
                    Token::Delimiter('}'),
                    Token::EOF,
                ],
            ),
            (
                "let z = (fn(x, y) { x % y })(10, 3);",
                vec![
                    Token::Keyword(LET),
                    Token::Identifier("z".to_string()),
                    Token::Operator(BIND),
                    Token::Delimiter('('),
                    Token::Keyword(FN),
                    Token::Delimiter('('),
                    Token::Identifier("x".to_string()),
                    Token::Delimiter(','),
                    Token::Identifier("y".to_string()),
                    Token::Delimiter(')'),
                    Token::Delimiter('{'),
                    Token::Identifier("x".to_string()),
                    Token::Operator(MOD),
                    Token::Identifier("y".to_string()),
                    Token::Delimiter('}'),
                    Token::Delimiter(')'),
                    Token::Delimiter('('),
                    Token::Literal("10".to_string()),
                    Token::Delimiter(','),
                    Token::Literal("3".to_string()),
                    Token::Delimiter(')'),
                    Token::Delimiter(';'),
                    Token::EOF,
                ],
            ),
            (
                "fn(x){x*2}",
                vec![
                    Token::Keyword("fn"),
                    Token::Delimiter('('),
                    Token::Identifier("x".to_string()),
                    Token::Delimiter(')'),
                    Token::Delimiter('{'),
                    Token::Identifier("x".to_string()),
                    Token::Operator("*"),
                    Token::Literal("2".to_string()),
                    Token::Delimiter('}'),
                    Token::EOF,
                ],
            ),
            (
                "42;",
                vec![
                    Token::Literal("42".to_string()),
                    Token::Delimiter(';'),
                    Token::EOF,
                ],
            ),
        ];

        for (code, expected) in tests {
            let mut buf = Buf::new(code);
            let tokens = tokenize(&mut buf).unwrap();
            assert_eq!(tokens, expected);
        }
    }
}
