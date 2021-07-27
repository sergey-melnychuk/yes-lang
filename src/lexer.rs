use crate::error::TokenError;
use crate::buffer::Iterable;
use crate::token::*;

fn match_operator(next: char, peek: Option<char>) -> Result<Token, TokenError> {
    match (next, peek) {
        ('+', _) => Ok(Token::Operator(PLUS)),
        ('-', _) => Ok(Token::Operator(DASH)),
        ('*', _) => Ok(Token::Operator(STAR)),
        ('/', _) => Ok(Token::Operator(SLASH)),
        ('%', _) => Ok(Token::Operator(PERCENT)),
        ('>', Some('=')) => Ok(Token::Operator(GTE)),
        ('>', _) => Ok(Token::Operator(GT)),
        ('<', Some('=')) => Ok(Token::Operator(LTE)),
        ('<', _) => Ok(Token::Operator(LT)),
        ('!', Some('=')) => Ok(Token::Operator(NE)),
        ('!', _) => Ok(Token::Operator(BANG)),
        ('&', Some('&')) => Ok(Token::Operator(AND)),
        ('|', Some('|')) => Ok(Token::Operator(OR)),
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

pub(crate) fn tokenize<I: Iterable<char>>(buf: &I) -> Result<Vec<Token>, TokenError> {
    let mut tokens = Vec::new();

    let mut this = String::with_capacity(16);
    loop {
        let next = *{
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
            let token = match_operator(next, buf.peek().cloned())?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;

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
                    Token::Operator(DASH),
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
                    Token::Operator(PERCENT),
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
                    Token::Keyword(FN),
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
            (
                "let answer = \"42\";",
                vec![
                    Token::Keyword(LET),
                    Token::Identifier("answer".to_string()),
                    Token::Operator(BIND),
                    Token::Literal("\"42\"".to_string()),
                    Token::Delimiter(';'),
                    Token::EOF,
                ]
            ),
            (
                "a <= 2",
                vec![
                    Token::Identifier("a".to_string()),
                    Token::Operator(LTE),
                    Token::Literal("2".to_string()),
                    Token::EOF,
                ]
            ),
        ];

        for (code, expected) in tests {
            let mut buf = Buffer::from_string(code);
            let tokens = tokenize(&mut buf).unwrap();
            assert_eq!(tokens, expected);
        }
    }
}
