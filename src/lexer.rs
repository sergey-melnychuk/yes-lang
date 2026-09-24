use crate::buffer::Iterable;
use crate::error::TokenError;
use crate::hex::decode_hex;
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

// tokens with offsets (in chars) of their first characters in the source
#[derive(Default)]
pub(crate) struct Tokens {
    pub(crate) tokens: Vec<Token>,
    pub(crate) offsets: Vec<usize>,
}

impl Tokens {
    fn push(&mut self, token: Token, offset: usize) {
        self.tokens.push(token);
        self.offsets.push(offset);
    }
}

fn match_this(this: &mut String, start: usize, tokens: &mut Tokens) {
    if this.is_empty() {
        return;
    }
    let token = if is_keyword(this) {
        match this.as_ref() {
            LET => Token::Keyword(LET),
            IF => Token::Keyword(IF),
            ELSE => Token::Keyword(ELSE),
            RETURN => Token::Keyword(RETURN),
            FN => Token::Keyword(FN),
            TRUE => Token::Keyword(TRUE),
            FALSE => Token::Keyword(FALSE),
            NULL => Token::Keyword(NULL),
            IN => Token::Keyword(IN),
            _ => unreachable!(),
        }
    } else if is_identifier(this) {
        Token::Identifier(this.to_string())
    } else {
        Token::Literal(this.to_string())
    };
    tokens.push(token, start);
    this.clear();
}

fn is_digits(s: &str) -> bool {
    !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())
}

// reads the rest of a "..." string literal (opening quote is already consumed)
fn read_string<I: Iterable<char>>(buf: &I) -> Result<String, TokenError> {
    let mut s = String::new();
    loop {
        match buf.next().copied() {
            Some('"') => return Ok(s),
            Some('\\') => match buf.next().copied() {
                Some('"') => s.push('"'),
                Some('\\') => s.push('\\'),
                Some('n') => s.push('\n'),
                Some('t') => s.push('\t'),
                Some('r') => s.push('\r'),
                Some(c) => return Err(TokenError::Unexpected(format!("\\{}", c))),
                None => return Err(TokenError::Unterminated("string")),
            },
            Some(c) => s.push(c),
            None => return Err(TokenError::Unterminated("string")),
        }
    }
}

// reads the rest of a b"..." hex bytes literal (opening quote is already consumed)
fn read_bytes<I: Iterable<char>>(buf: &I) -> Result<Vec<u8>, TokenError> {
    let mut hex = String::new();
    loop {
        match buf.next().copied() {
            Some('"') => break,
            Some(c) => hex.push(c),
            None => return Err(TokenError::Unterminated("bytes")),
        }
    }
    let digits = hex.strip_prefix("0x").unwrap_or(&hex);
    decode_hex(digits).ok_or(TokenError::InvalidBytes(hex))
}

#[cfg(test)]
pub(crate) fn tokenize<I: Iterable<char>>(buf: &I) -> Result<Vec<Token>, TokenError> {
    tokenize_with_offsets(buf)
        .map(|tokens| tokens.tokens)
        .map_err(|(e, _)| e)
}

/// On error, returns the offset of the token that caused it.
pub(crate) fn tokenize_with_offsets<I: Iterable<char>>(
    buf: &I,
) -> Result<Tokens, (TokenError, usize)> {
    let mut tokens = Tokens::default();

    let mut this = String::with_capacity(16);
    let mut start = 0; // offset of the first char of 'this'
    loop {
        let at = buf.pos();
        let next = *{
            if let Some(chr) = buf.next() {
                chr
            } else {
                match_this(&mut this, start, &mut tokens);
                tokens.push(Token::End, at);
                break;
            }
        };

        // match quoted literals: b"..." for raw hex bytes, "..." for strings
        if next == '"' {
            if this == "b" {
                this.clear();
                let bytes = read_bytes(buf).map_err(|e| (e, start))?;
                tokens.push(Token::Bytes(bytes), start);
            } else {
                match_this(&mut this, start, &mut tokens);
                let string = read_string(buf).map_err(|e| (e, at))?;
                tokens.push(Token::Str(string), at);
            }
            continue;
        }

        // match single-line comment from '//' to the line break
        if next == '/' && buf.peek() == Some(&'/') {
            match_this(&mut this, start, &mut tokens);
            while let Some(c) = buf.next() {
                if *c == '\n' {
                    break;
                }
            }
            continue;
        }

        // match multi-line comment from '/*' to '*/'
        if next == '/' && buf.peek() == Some(&'*') {
            match_this(&mut this, start, &mut tokens);
            let _ = buf.next(); // skip opening '*', so that '/*/' does not close the comment
            loop {
                match buf.next().copied() {
                    Some('*') if buf.peek() == Some(&'/') => {
                        let _ = buf.next();
                        break;
                    }
                    Some(_) => continue,
                    None => return Err((TokenError::Unterminated("comment"), at)),
                }
            }
            continue;
        }

        // '.' is either a part of a float literal, a range operator '..' or a field access
        if next == '.' {
            if buf.peek() == Some(&'.') {
                match_this(&mut this, start, &mut tokens);
                let _ = buf.next();
                tokens.push(Token::Operator(RANGE), at);
            } else if is_digits(&this) && buf.peek().is_some_and(|c| c.is_ascii_digit()) {
                this.push(next);
            } else {
                match_this(&mut this, start, &mut tokens);
                tokens.push(Token::Delimiter('.'), at);
            }
            continue;
        }

        if next.is_whitespace() || is_delimiter(next) || is_operator(next) {
            match_this(&mut this, start, &mut tokens);
        } else {
            if this.is_empty() {
                start = at;
            }
            this.push(next);
        }

        if is_operator(next) {
            let token = match_operator(next, buf.peek().cloned()).map_err(|e| (e, at))?;
            if token.len() > 1 {
                let _ = buf.next();
            }
            tokens.push(token, at);
        }

        if is_delimiter(next) {
            tokens.push(Token::Delimiter(next), at);
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
                    Token::Str("42".to_string()),
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
                vec![Token::Str("hello ".to_string()), Token::End],
            ),
            (
                "abc\"def\"",
                vec![
                    Token::Identifier("abc".to_string()),
                    Token::Str("def".to_string()),
                    Token::End,
                ],
            ),
            ("true", vec![Token::Keyword(TRUE), Token::End]),
            ("let", vec![Token::Keyword(LET), Token::End]),
            // identifiers may contain '_' anywhere
            (
                "ab_c a1_b2 _x",
                vec![
                    Token::Identifier("ab_c".to_string()),
                    Token::Identifier("a1_b2".to_string()),
                    Token::Identifier("_x".to_string()),
                    Token::End,
                ],
            ),
            // a comment terminates the preceding token
            (
                "a// comment\nb",
                vec![
                    Token::Identifier("a".to_string()),
                    Token::Identifier("b".to_string()),
                    Token::End,
                ],
            ),
            (
                "a/* comment */b",
                vec![
                    Token::Identifier("a".to_string()),
                    Token::Identifier("b".to_string()),
                    Token::End,
                ],
            ),
            // '/*/' does not close a multi-line comment
            ("/*/ x */", vec![Token::End]),
            (
                r#""a\"b\\c\nd""#,
                vec![Token::Str("a\"b\\c\nd".to_string()), Token::End],
            ),
            (
                "b\"a9059cbb\" b\"0xA9\" b\"\"",
                vec![
                    Token::Bytes(vec![0xa9, 0x05, 0x9c, 0xbb]),
                    Token::Bytes(vec![0xa9]),
                    Token::Bytes(vec![]),
                    Token::End,
                ],
            ),
            (
                "1.5 x.y a[0..4]",
                vec![
                    Token::Literal("1.5".to_string()),
                    Token::Identifier("x".to_string()),
                    Token::Delimiter('.'),
                    Token::Identifier("y".to_string()),
                    Token::Identifier("a".to_string()),
                    Token::Delimiter('['),
                    Token::Literal("0".to_string()),
                    Token::Operator(RANGE),
                    Token::Literal("4".to_string()),
                    Token::Delimiter(']'),
                    Token::End,
                ],
            ),
        ];

        for (code, expected) in tests {
            let buf = Buffer::from_string(code);
            let tokens = tokenize(&buf).unwrap();
            assert_eq!(tokens, expected, "{}", code);
        }
    }

    #[test]
    fn test_lexer_errors() {
        let tests = vec![
            ("\"abc", TokenError::Unterminated("string")),
            ("b\"abc", TokenError::Unterminated("bytes")),
            ("/* abc", TokenError::Unterminated("comment")),
            ("\"\\q\"", TokenError::Unexpected("\\q".to_string())),
            ("b\"abc\"", TokenError::InvalidBytes("abc".to_string())),
            ("b\"zz\"", TokenError::InvalidBytes("zz".to_string())),
            ("a & b", TokenError::Unexpected("&".to_string())),
        ];

        for (code, expected) in tests {
            let buf = Buffer::from_string(code);
            let result = tokenize(&buf);
            assert_eq!(result, Err(expected), "{}", code);
        }
    }
}
