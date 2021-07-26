use crate::buffer::{Buffer, Iterable};
use crate::error::ParserError;
use crate::token::*;

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    Not,
    Bind,
}

impl Operator {
    pub(crate) fn rank(&self) -> usize {
        match self {
            Operator::Not => 5,
            Operator::Mul
            | Operator::Div
            | Operator::Mod => 4,
            Operator::Add
            | Operator::Sub => 3,
            Operator::Eq
            | Operator::Ne
            | Operator::Lt
            | Operator::Lte
            | Operator::Gt
            | Operator::Gte => 2,
            Operator::Bind => 1,
        }
    }

    pub(crate) fn from_token(token: &Token) -> Result<Operator, ParserError> {
        match token {
            Token::Operator(ADD) => Ok(Operator::Add),
            Token::Operator(SUB) => Ok(Operator::Sub),
            Token::Operator(MUL) => Ok(Operator::Mul),
            Token::Operator(DIV) => Ok(Operator::Div),
            Token::Operator(MOD) => Ok(Operator::Mod),
            Token::Operator(GTE) => Ok(Operator::Gte),
            Token::Operator(GT) => Ok(Operator::Gt),
            Token::Operator(LTE) => Ok(Operator::Lte),
            Token::Operator(LT) => Ok(Operator::Lt),
            Token::Operator(NE) => Ok(Operator::Ne),
            Token::Operator(EQ) => Ok(Operator::Eq),
            Token::Operator(NOT) => Ok(Operator::Not),
            Token::Operator(BIND) => Ok(Operator::Bind),
            _ => Err(ParserError::Unexpected("operator".to_string(), token.clone()))
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Statement {
    Let(String, Expression),
    Ret(Expression),
}

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum Expression {
    Const(String),
    Prefix(Operator, Box<Expression>),
    Infix(Box<Expression>, Operator, Box<Expression>),
}

pub(crate) fn parse(tokens: Vec<Token>) -> Result<Vec<Statement>, ParserError> {
    let mut buffer = Buffer::new(tokens);
    let mut result = Vec::new();

    loop {
        let next = if let Some(next) = buffer.next() {
            if next == &Token::EOF {
                return Ok(result);
            } else {
                next
            }
        } else {
            return Err(ParserError::EOF);
        };

        if next == &Token::Keyword(LET) {
            let stmt = parse_let_statement(&mut buffer)?;
            result.push(stmt);
        }
    }
}

fn next(buffer: &Buffer<Token>) -> Result<&Token, ParserError> {
    if let Some(token) = buffer.next() {
        Ok(token)
    } else {
        Err(ParserError::EOF)
    }
}

fn peek(buffer: &Buffer<Token>) -> Result<&Token, ParserError> {
    if let Some(token) = buffer.peek() {
        Ok(token)
    } else {
        Err(ParserError::EOF)
    }
}

fn is_literal(token: &Token) -> bool {
    match token {
        Token::Literal(_) => true,
        _ => false
    }
}

fn is_operator(token: &Token) -> bool {
    match token {
        Token::Operator(_) => true,
        _ => false
    }
}

fn is_delimiter(token: &Token, chr: char) -> bool {
    match token {
        Token::Delimiter(d) => *d == chr,
        _ => false
    }
}


fn parse_operator(op: Operator) -> Result<Expression, ParserError> {
    todo!()
}

fn parse_expression(buffer: &mut Buffer<Token>) -> Result<Expression, ParserError> {
    let lhs = next(buffer)?;
    if lhs == &Token::EOF {
        return Err(ParserError::Unexpected("Expression".to_string(), Token::EOF));
    }

    if let Token::Literal(lit) = lhs {
        let peek = peek(buffer)?;
        if let Token::Delimiter(';') = peek {
            return Ok(Expression::Const(lit.to_owned()));
        }
    }

    Ok(Expression::Const("not yet implemented".to_string()))
}

fn parse_let_statement(buffer: &mut Buffer<Token>) -> Result<Statement, ParserError> {
    let token = next(buffer)?;
    let id = if let Token::Identifier(id) = token {
        id.to_owned()
    } else {
        return Err(ParserError::Unexpected("Identifier".to_string(), token.clone()));
    };

    let eq = next(buffer)?;
    if eq != &Token::Operator(BIND) {
        return Err(ParserError::Unexpected("=".to_string(), token.clone()));
    }

    let expr = parse_expression(buffer)?;
    Ok(Statement::Let(id, expr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn test_parse_statement() {
        let tests = vec![
            (
                "let ;",
                Err(ParserError::Unexpected("Identifier".to_string(), Token::Delimiter(';')))
            ),
            (
                "let 123",
                Err(ParserError::Unexpected("Identifier".to_string(), Token::Literal("123".to_string())))
            ),
            (
                "let abc;",
                Err(ParserError::Unexpected("=".to_string(), Token::Identifier("abc".to_string())))
            ),
            (
                "let x =",
                Err(ParserError::Unexpected("Expression".to_string(), Token::EOF))
            ),
            (
                "let answer = 42;",
                Ok(vec![
                    Statement::Let("answer".to_string(), Expression::Const("42".to_string()))
                ])
            ),
        ];

        for (src, expected) in tests {
            let tokens = tokenize(&mut Buffer::from_string(src)).unwrap();
            let parsed = parse(tokens);

            assert_eq!(parsed, expected);
        }
    }

    #[test]
    #[ignore]
    fn test_parse_expression() {
        let tests = vec![
            (
                "1 + 2;",
                Ok(
                    Expression::Infix(
                        Box::new(Expression::Const("1".to_string())),
                        Operator::Add,
                        Box::new(Expression::Const("2".to_string()))
                    )
                )
            ),
        ];

        for (src, expected) in tests {
            let tokens = tokenize(&mut Buffer::from_string(src)).unwrap();
            let mut buffer = Buffer::new(tokens);
            let parsed = parse_expression(&mut buffer);

            assert_eq!(parsed, expected);
        }
    }
}
