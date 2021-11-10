use crate::buffer::Iterable;
use crate::error::TokenError;
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

fn match_this(this: &mut String, tokens: &mut Vec<Token>) {
    if this.is_empty() {
        return;
    }
    if is_keyword(this) {
        let token = match this.as_ref() {
            LET => Token::Keyword(LET),
            IF => Token::Keyword(IF),
            ELSE => Token::Keyword(ELSE),
            RETURN => Token::Keyword(RETURN),
            FN => Token::Keyword(FN),
            TRUE => Token::Keyword(TRUE),
            FALSE => Token::Keyword(FALSE),
            _ => unreachable!(),
        };
        tokens.push(token);
        this.clear();
    } else if is_identifier(this) {
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
                match_this(&mut this, &mut tokens);
                tokens.push(Token::End);
                break;
            }
        };

        // match quoted string literals
        if next == '"' {
            this.push(next);
            while let Some(c) = buf.next() {
                this.push(*c);
                if *c == '"' {
                    break;
                }
            }
            continue;
        }

        // match single-line comment from '//' to the line break
        if next == '/' && buf.peek() == Some(&'/') {
            while let Some(c) = buf.next() {
                if *c == '\n' {
                    break;
                }
            }
            continue;
        }

        // match multi-line comment from '//' to the end of line
        if next == '/' && buf.peek() == Some(&'*') {
            while let Some(c) = buf.next() {
                if *c == '*' && buf.peek() == Some(&'/') {
                    let _ = buf.next();
                    break;
                }
            }
            continue;
        }

        if next.is_whitespace() || is_delimiter(next) || is_operator(next) {
            match_this(&mut this, &mut tokens);
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
    fn test_lexer() {
        let tests = vec![
            (
                "let abc = 123;",
                vec![
                    Token::Keyword(LET),
                    Token::Identifier("abc".to_string()),
                    Token::Operator(BIND),
                    Token::Literal("123".to_string()),
                    Token::Delimiter(';'),
                    Token::End,
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
                    Token::End,
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
                    Token::End,
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
                    Token::End,
                ],
            ),
            (
                "42;",
                vec![
                    Token::Literal("42".to_string()),
                    Token::Delimiter(';'),
                    Token::End,
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
                    Token::End,
                ],
            ),
            (
                "a <= 2",
                vec![
                    Token::Identifier("a".to_string()),
                    Token::Operator(LTE),
                    Token::Literal("2".to_string()),
                    Token::End,
                ],
            ),
            (
                "\"hello \"",
                vec![Token::Literal("\"hello \"".to_string()), Token::End],
            ),
            (
                "abc\"def\"",
                vec![Token::Literal("abc\"def\"".to_string()), Token::End],
            ),
            ("true", vec![Token::Keyword(TRUE), Token::End]),
            ("let", vec![Token::Keyword(LET), Token::End]),
        ];

        for (code, expected) in tests {
            let mut buf = Buffer::from_string(code);
            let tokens = tokenize(&mut buf).unwrap();
            assert_eq!(tokens, expected);
        }
    }
}
