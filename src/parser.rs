use crate::buffer::{Buffer, Iterable};
use crate::error::ParserError;
use crate::token::*;

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq, Clone)]
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
    Neg,
    Not,
    Or,
    And,
    Bind,
}

impl Operator {
    pub(crate) const MIN_RANK: usize = 0;

    pub(crate) fn rank(&self) -> usize {
        match self {
            Operator::Not | Operator::Neg => 7,
            Operator::Mul | Operator::Div | Operator::Mod => 6,
            Operator::Add | Operator::Sub => 5,
            Operator::Lt | Operator::Lte | Operator::Gt | Operator::Gte => 4,
            Operator::Eq | Operator::Ne => 3,
            Operator::Or | Operator::And => 2,
            Operator::Bind => 1,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum Statement {
    Let(String, Expression),
    Ret(Expression),
    If(Expression, Vec<Statement>, Vec<Statement>),
    Fn(String, Vec<String>, Vec<Statement>),
    Expr(Expression),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) enum Expression {
    Var(String),
    Lit(String),
    Fn(Vec<String>, Vec<Statement>),
    Apply(Box<Expression>, Vec<Expression>),
    Prefix(Operator, Box<Expression>),
    Infix(Box<Expression>, Operator, Box<Expression>),
}

pub(crate) fn parse(buffer: &Buffer<Token>) -> Result<Vec<Statement>, ParserError> {
    let mut result = Vec::new();

    loop {
        let token = if let Some(next) = buffer.peek() {
            if next == &Token::End {
                return Ok(result);
            } else {
                let _ = buffer.next();
                next
            }
        } else {
            return Err(ParserError::Unexpected(Token::End));
        };

        if token == &Token::Keyword(LET) {
            let stmt = parse_let_statement(buffer)?;
            expect(buffer, &Token::Delimiter(';'))?;
            result.push(stmt);
        } else if token == &Token::Keyword(RETURN) {
            let expr = parse_expression(buffer, Operator::MIN_RANK)?;
            expect(buffer, &Token::Delimiter(';'))?;
            let stmt = Statement::Ret(expr);
            result.push(stmt);
        } else if token == &Token::Keyword(IF) {
            let stmt = parse_if_statement(buffer)?;
            result.push(stmt);
        } else if token == &Token::Keyword(FN) {
            if let &Token::Identifier(_) = peek(buffer)? {
                let stmt = parse_fn_definition(buffer)?;
                result.push(stmt);
            } else {
                let expr = parse_fn_expression(buffer)?;
                let stmt = Statement::Expr(expr);
                result.push(stmt);
            }
        } else {
            buffer.back();
            let pos = buffer.pos();
            if let Ok(expr) = parse_expression(buffer, Operator::MIN_RANK) {
                let stmt = Statement::Expr(expr);
                result.push(stmt);
            } else {
                buffer.set(pos);
            }
            return Ok(result);
        }
    }
}

fn next(buffer: &Buffer<Token>) -> Result<&Token, ParserError> {
    if let Some(token) = buffer.next() {
        Ok(token)
    } else {
        Err(ParserError::Unexpected(Token::End))
    }
}

fn peek(buffer: &Buffer<Token>) -> Result<&Token, ParserError> {
    if let Some(token) = buffer.peek() {
        Ok(token)
    } else {
        Err(ParserError::Unexpected(Token::End))
    }
}

fn expect(buffer: &Buffer<Token>, expected: &Token) -> Result<(), ParserError> {
    if let Some(token) = buffer.next() {
        if token != expected {
            Err(ParserError::Mismatch(expected.clone(), token.clone()))
        } else {
            Ok(())
        }
    } else {
        Err(ParserError::Unexpected(Token::End))
    }
}

fn is_identifier(token: &Token) -> bool {
    matches!(token, Token::Identifier(_))
}

trait PrefixParser {
    fn parse(&mut self, token: &Token, buffer: &Buffer<Token>) -> Result<Expression, ParserError>;
}

impl PrefixParser for Operator {
    fn parse(&mut self, token: &Token, buffer: &Buffer<Token>) -> Result<Expression, ParserError> {
        match self {
            Operator::Not | Operator::Neg => {
                let rhs = parse_expression(buffer, self.rank())?;
                Ok(Expression::Prefix(self.clone(), Box::new(rhs)))
            }
            _ => Err(ParserError::Prefix(token.clone())),
        }
    }
}

fn get_prefix_parser(token: &Token) -> Option<impl PrefixParser> {
    match token {
        Token::Operator(DASH) => Some(Operator::Neg),
        Token::Operator(BANG) => Some(Operator::Not),
        _ => None,
    }
}

trait InfixParser {
    fn rank(&self) -> usize;
    fn parse(
        &mut self,
        lhs: Expression,
        token: &Token,
        buffer: &Buffer<Token>,
    ) -> Result<Expression, ParserError>;
}

impl InfixParser for Operator {
    fn rank(&self) -> usize {
        self.rank()
    }

    fn parse(
        &mut self,
        lhs: Expression,
        token: &Token,
        buffer: &Buffer<Token>,
    ) -> Result<Expression, ParserError> {
        match self {
            Operator::Add
            | Operator::Sub
            | Operator::Mul
            | Operator::Mod
            | Operator::Div
            | Operator::And
            | Operator::Or
            | Operator::Lt
            | Operator::Lte
            | Operator::Gt
            | Operator::Gte
            | Operator::Eq
            | Operator::Ne
            | Operator::Bind => {
                let rhs = parse_expression(buffer, self.rank())?;
                Ok(Expression::Infix(
                    Box::new(lhs),
                    self.clone(),
                    Box::new(rhs),
                ))
            }
            _ => Err(ParserError::Infix(token.clone())),
        }
    }
}

fn get_infix_parser(token: &Token) -> Option<impl InfixParser> {
    match token {
        Token::Operator(PLUS) => Some(Operator::Add),
        Token::Operator(DASH) => Some(Operator::Sub),
        Token::Operator(STAR) => Some(Operator::Mul),
        Token::Operator(SLASH) => Some(Operator::Div),
        Token::Operator(PERCENT) => Some(Operator::Mod),
        Token::Operator(LT) => Some(Operator::Lt),
        Token::Operator(LTE) => Some(Operator::Lte),
        Token::Operator(GT) => Some(Operator::Gt),
        Token::Operator(GTE) => Some(Operator::Gte),
        Token::Operator(EQ) => Some(Operator::Eq),
        Token::Operator(NE) => Some(Operator::Ne),
        Token::Operator(AND) => Some(Operator::And),
        Token::Operator(OR) => Some(Operator::Or),
        Token::Operator(BIND) => Some(Operator::Bind),
        _ => None,
    }
}

fn parse_expression(buffer: &Buffer<Token>, rank: usize) -> Result<Expression, ParserError> {
    let token = next(buffer)?;
    let mut expr = if let Token::Operator(_) = token {
        if let Some(mut op) = get_prefix_parser(token) {
            op.parse(token, buffer)?
        } else {
            return Err(ParserError::Prefix(token.clone()));
        }
    } else if let Token::Literal(lit) = token {
        Expression::Lit(lit.to_owned())
    } else if let Token::Identifier(id) = token {
        if let Token::Delimiter('(') = peek(buffer)? {
            let name = Expression::Var(id.to_owned());
            parse_fn_application(name, buffer)?
        } else {
            Expression::Var(id.to_owned())
        }
    } else if let Token::Keyword(FN) = token {
        parse_fn_expression(buffer)?
    } else if let Token::Delimiter('(') = token {
        let is_fn = peek(buffer)? == &Token::Keyword(FN);
        let expr = parse_expression(buffer, Operator::MIN_RANK)?;
        expect(buffer, &Token::Delimiter(')'))?;
        if is_fn {
            parse_fn_application(expr, buffer)?
        } else {
            expr
        }
    } else if let Token::Keyword(TRUE) = token {
        Expression::Lit(TRUE.to_string())
    } else if let Token::Keyword(FALSE) = token {
        Expression::Lit(FALSE.to_string())
    } else {
        return Err(ParserError::Unexpected(token.clone()));
    };

    while let Token::Operator(_) = peek(buffer)? {
        let token = peek(buffer)?;
        if let Some(mut op) = get_infix_parser(token) {
            if rank < op.rank() {
                let _ = next(buffer)?;
                expr = op.parse(expr, token, buffer)?;
            } else {
                break;
            }
        }
    }

    Ok(expr)
}

fn parse_let_statement(buffer: &Buffer<Token>) -> Result<Statement, ParserError> {
    let token = peek(buffer)?;
    if !is_identifier(token) {
        return Err(ParserError::Mismatch(
            Token::Identifier("*".to_string()),
            token.clone(),
        ));
    }
    match parse_expression(buffer, Operator::MIN_RANK)? {
        Expression::Infix(lhs, Operator::Bind, rhs) => match *lhs {
            Expression::Var(name) => Ok(Statement::Let(name, *rhs)),
            lhs => Err(ParserError::Op(Operator::Bind, lhs)),
        },
        expr => Err(ParserError::Op(Operator::Bind, expr)),
    }
}

fn parse_if_statement(buffer: &Buffer<Token>) -> Result<Statement, ParserError> {
    let cond = parse_expression(buffer, Operator::MIN_RANK)?;
    expect(buffer, &Token::Delimiter('{'))?;
    let if_clause = parse(buffer)?;
    expect(buffer, &Token::Delimiter('}'))?;
    let else_clause = if peek(buffer)? == &Token::Keyword(ELSE) {
        let _ = next(buffer)?;
        expect(buffer, &Token::Delimiter('{'))?;
        let block = parse(buffer)?;
        expect(buffer, &Token::Delimiter('}'))?;
        block
    } else {
        vec![]
    };
    Ok(Statement::If(cond, if_clause, else_clause))
}

fn parse_fn_definition(buffer: &Buffer<Token>) -> Result<Statement, ParserError> {
    let token = next(buffer)?;
    let name = if let Token::Identifier(id) = token {
        id.to_owned()
    } else {
        return Err(ParserError::Mismatch(
            Token::Identifier("*".to_string()),
            token.clone(),
        ));
    };

    let mut args = Vec::new();
    expect(buffer, &Token::Delimiter('('))?;
    loop {
        let token = peek(buffer)?;
        if let Token::Identifier(arg) = token {
            args.push(arg.to_owned());
            let _ = next(buffer)?;
        }
        let token = next(buffer)?;
        if token == &Token::Delimiter(')') {
            break;
        }
        if token == &Token::Delimiter(',') {
            continue;
        }
        return Err(ParserError::Mismatch(Token::Delimiter(')'), token.clone()));
    }

    expect(buffer, &Token::Delimiter('{'))?;
    let statements = parse(buffer)?;
    expect(buffer, &Token::Delimiter('}'))?;

    Ok(Statement::Fn(name, args, statements))
}

fn parse_fn_expression(buffer: &Buffer<Token>) -> Result<Expression, ParserError> {
    let mut args = Vec::new();
    expect(buffer, &Token::Delimiter('('))?;
    loop {
        let token = peek(buffer)?;
        if let Token::Identifier(arg) = token {
            args.push(arg.to_owned());
            let _ = next(buffer)?;
        }
        let token = next(buffer)?;
        if token == &Token::Delimiter(')') {
            break;
        }
        if token == &Token::Delimiter(',') {
            continue;
        }
        return Err(ParserError::Mismatch(Token::Delimiter(')'), token.clone()));
    }

    expect(buffer, &Token::Delimiter('{'))?;
    let statements = parse(buffer)?;
    expect(buffer, &Token::Delimiter('}'))?;

    Ok(Expression::Fn(args, statements))
}

fn parse_fn_application(
    func: Expression,
    buffer: &Buffer<Token>,
) -> Result<Expression, ParserError> {
    let mut expr = func;
    while let &Token::Delimiter('(') = peek(buffer)? {
        expect(buffer, &Token::Delimiter('('))?;
        let mut args = Vec::new();
        loop {
            let token = peek(buffer)?;
            if token == &Token::Delimiter(')') {
                break;
            }
            let arg = parse_expression(buffer, Operator::MIN_RANK)?;
            args.push(arg);
            let token = peek(buffer)?;
            if token == &Token::Delimiter(',') {
                let _ = next(buffer)?;
                continue;
            }
            if token == &Token::Delimiter(')') {
                break;
            }
        }
        expect(buffer, &Token::Delimiter(')'))?;
        expr = Expression::Apply(Box::new(expr), args);
    }
    Ok(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;

    #[test]
    fn test_stmt() {
        let tests = vec![
            (
                "let ;",
                Err(ParserError::Mismatch(
                    Token::Identifier("*".to_string()),
                    Token::Delimiter(';'),
                )),
            ),
            (
                "let 123",
                Err(ParserError::Mismatch(
                    Token::Identifier("*".to_string()),
                    Token::Literal("123".to_string()),
                )),
            ),
            (
                "let abc;",
                Err(ParserError::Op(
                    Operator::Bind,
                    Expression::Var("abc".to_string()),
                )),
            ),
            ("let x =", Err(ParserError::Unexpected(Token::End))),
            ("{", Ok(vec![])),
            ("[", Ok(vec![])),
            (
                "let answer = 42;",
                Ok(vec![Statement::Let(
                    "answer".to_string(),
                    Expression::Lit("42".to_string()),
                )]),
            ),
            (
                "let result = x + 1;",
                Ok(vec![Statement::Let(
                    "result".to_string(),
                    Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit("1".to_string())),
                    ),
                )]),
            ),
            (
                "let flag = x >= 42;",
                Ok(vec![Statement::Let(
                    "flag".to_string(),
                    Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Gte,
                        Box::new(Expression::Lit("42".to_string())),
                    ),
                )]),
            ),
            (
                "return (a + 16) * k + 4;",
                Ok(vec![Statement::Ret(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Infix(
                            Box::new(Expression::Var("a".to_string())),
                            Operator::Add,
                            Box::new(Expression::Lit("16".to_string())),
                        )),
                        Operator::Mul,
                        Box::new(Expression::Var("k".to_string())),
                    )),
                    Operator::Add,
                    Box::new(Expression::Lit("4".to_string())),
                ))]),
            ),
            (
                "fn add(a, b) { return a + b; }",
                Ok(vec![Statement::Fn(
                    "add".to_string(),
                    vec!["a".to_string(), "b".to_string()],
                    vec![Statement::Ret(Expression::Infix(
                        Box::new(Expression::Var("a".to_string())),
                        Operator::Add,
                        Box::new(Expression::Var("b".to_string())),
                    ))],
                )]),
            ),
            (
                "let x = fn(a) { return a; };",
                Ok(vec![Statement::Let(
                    "x".to_string(),
                    Expression::Fn(
                        vec!["a".to_string()],
                        vec![Statement::Ret(Expression::Var("a".to_string()))],
                    ),
                )]),
            ),
            (
                "return f(x);",
                Ok(vec![Statement::Ret(Expression::Apply(
                    Box::new(Expression::Var("f".to_string())),
                    vec![Expression::Var("x".to_string())],
                ))]),
            ),
            (
                "fn sum(a, b) { return a + b; }",
                Ok(vec![Statement::Fn(
                    "sum".to_string(),
                    vec!["a".to_string(), "b".to_string()],
                    vec![Statement::Ret(Expression::Infix(
                        Box::new(Expression::Var("a".to_string())),
                        Operator::Add,
                        Box::new(Expression::Var("b".to_string())),
                    ))],
                )]),
            ),
            (
                "let f = fn(a, b) { return (a + b) / 2; };",
                Ok(vec![Statement::Let(
                    "f".to_string(),
                    Expression::Fn(
                        vec!["a".to_string(), "b".to_string()],
                        vec![Statement::Ret(Expression::Infix(
                            Box::new(Expression::Infix(
                                Box::new(Expression::Var("a".to_string())),
                                Operator::Add,
                                Box::new(Expression::Var("b".to_string())),
                            )),
                            Operator::Div,
                            Box::new(Expression::Lit("2".to_string())),
                        ))],
                    ),
                )]),
            ),
            (
                "let x = (fn(a,b) {return a+b;})(1,2);",
                Ok(vec![Statement::Let(
                    "x".to_string(),
                    Expression::Apply(
                        Box::new(Expression::Fn(
                            vec!["a".to_string(), "b".to_string()],
                            vec![Statement::Ret(Expression::Infix(
                                Box::new(Expression::Var("a".to_string())),
                                Operator::Add,
                                Box::new(Expression::Var("b".to_string())),
                            ))],
                        )),
                        vec![
                            Expression::Lit("1".to_string()),
                            Expression::Lit("2".to_string()),
                        ],
                    ),
                )]),
            ),
            (
                "if a == 0 { x + 1 } else { y - 2 }",
                Ok(vec![Statement::If(
                    Expression::Infix(
                        Box::new(Expression::Var("a".to_string())),
                        Operator::Eq,
                        Box::new(Expression::Lit("0".to_string())),
                    ),
                    vec![Statement::Expr(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit("1".to_string())),
                    ))],
                    vec![Statement::Expr(Expression::Infix(
                        Box::new(Expression::Var("y".to_string())),
                        Operator::Sub,
                        Box::new(Expression::Lit("2".to_string())),
                    ))],
                )]),
            ),
            (
                "if a { f(x) } else { g(y) }",
                Ok(vec![Statement::If(
                    Expression::Var("a".to_string()),
                    vec![Statement::Expr(Expression::Apply(
                        Box::new(Expression::Var("f".to_string())),
                        vec![Expression::Var("x".to_string())],
                    ))],
                    vec![Statement::Expr(Expression::Apply(
                        Box::new(Expression::Var("g".to_string())),
                        vec![Expression::Var("y".to_string())],
                    ))],
                )]),
            ),
            (
                "if x == 0 { let y = x * 2; return y; }",
                Ok(vec![Statement::If(
                    Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Eq,
                        Box::new(Expression::Lit("0".to_string())),
                    ),
                    vec![
                        Statement::Let(
                            "y".to_string(),
                            Expression::Infix(
                                Box::new(Expression::Var("x".to_string())),
                                Operator::Mul,
                                Box::new(Expression::Lit("2".to_string())),
                            ),
                        ),
                        Statement::Ret(Expression::Var("y".to_string())),
                    ],
                    vec![],
                )]),
            ),
            (
                "(fn(a,b) {return a+b;})(1,2);",
                Ok(vec![Statement::Expr(Expression::Apply(
                    Box::new(Expression::Fn(
                        vec!["a".to_string(), "b".to_string()],
                        vec![Statement::Ret(Expression::Infix(
                            Box::new(Expression::Var("a".to_string())),
                            Operator::Add,
                            Box::new(Expression::Var("b".to_string())),
                        ))],
                    )),
                    vec![
                        Expression::Lit("1".to_string()),
                        Expression::Lit("2".to_string()),
                    ],
                ))]),
            ),
            (
                "x + y",
                Ok(vec![Statement::Expr(Expression::Infix(
                    Box::new(Expression::Var("x".to_string())),
                    Operator::Add,
                    Box::new(Expression::Var("y".to_string())),
                ))]),
            ),
            (
                "let f = fn(a, b) { a + b }",
                Err(ParserError::Mismatch(Token::Delimiter(';'), Token::End)),
            ),
            (
                "fn g(a) { return fn(b) { f(a,b) }; }",
                Ok(vec![Statement::Fn(
                    "g".to_string(),
                    vec!["a".to_string()],
                    vec![Statement::Ret(Expression::Fn(
                        vec!["b".to_string()],
                        vec![Statement::Expr(Expression::Apply(
                            Box::new(Expression::Var("f".to_string())),
                            vec![
                                Expression::Var("a".to_string()),
                                Expression::Var("b".to_string()),
                            ],
                        ))],
                    ))],
                )]),
            ),
            (
                "1 + (2)",
                Ok(vec![Statement::Expr(Expression::Infix(
                    Box::new(Expression::Lit("1".to_string())),
                    Operator::Add,
                    Box::new(Expression::Lit("2".to_string())),
                ))]),
            ),
            (
                "(1) + 2",
                Ok(vec![Statement::Expr(Expression::Infix(
                    Box::new(Expression::Lit("1".to_string())),
                    Operator::Add,
                    Box::new(Expression::Lit("2".to_string())),
                ))]),
            ),
        ];

        for (src, expected) in tests {
            let tokens = tokenize(&mut Buffer::from_string(src)).unwrap();
            let buf = Buffer::new(tokens);
            let parsed = parse(&buf);

            assert_eq!(parsed, expected, "src=\"{}\"", src);
        }
    }

    #[test]
    fn test_expr() {
        let tests = vec![
            (
                "-x",
                Ok(Expression::Prefix(
                    Operator::Neg,
                    Box::new(Expression::Var("x".to_string())),
                )),
            ),
            (
                "!abc",
                Ok(Expression::Prefix(
                    Operator::Not,
                    Box::new(Expression::Var("abc".to_string())),
                )),
            ),
            (
                "1 + 2",
                Ok(Expression::Infix(
                    Box::new(Expression::Lit("1".to_string())),
                    Operator::Add,
                    Box::new(Expression::Lit("2".to_string())),
                )),
            ),
            (
                "z != 42",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("z".to_string())),
                    Operator::Ne,
                    Box::new(Expression::Lit("42".to_string())),
                )),
            ),
            (
                "a <= 2",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("a".to_string())),
                    Operator::Lte,
                    Box::new(Expression::Lit("2".to_string())),
                )),
            ),
            (
                "n % 42",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("n".to_string())),
                    Operator::Mod,
                    Box::new(Expression::Lit("42".to_string())),
                )),
            ),
            (
                "x + 1 / 2",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("x".to_string())),
                    Operator::Add,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Lit("1".to_string())),
                        Operator::Div,
                        Box::new(Expression::Lit("2".to_string())),
                    )),
                )),
            ),
            (
                "a * 2 + 1",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("a".to_string())),
                        Operator::Mul,
                        Box::new(Expression::Lit("2".to_string())),
                    )),
                    Operator::Add,
                    Box::new(Expression::Lit("1".to_string())),
                )),
            ),
            (
                "(abc + 3) * 4",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("abc".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit("3".to_string())),
                    )),
                    Operator::Mul,
                    Box::new(Expression::Lit("4".to_string())),
                )),
            ),
            ("(((42)))", Ok(Expression::Lit("42".to_string()))),
            (
                "2 * (x + 3)",
                Ok(Expression::Infix(
                    Box::new(Expression::Lit("2".to_string())),
                    Operator::Mul,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit("3".to_string())),
                    )),
                )),
            ),
            (
                "(a + b) * (c - d)",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("a".to_string())),
                        Operator::Add,
                        Box::new(Expression::Var("b".to_string())),
                    )),
                    Operator::Mul,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("c".to_string())),
                        Operator::Sub,
                        Box::new(Expression::Var("d".to_string())),
                    )),
                )),
            ),
            (
                "a >= 42 && x != 1",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("a".to_string())),
                        Operator::Gte,
                        Box::new(Expression::Lit("42".to_string())),
                    )),
                    Operator::And,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Ne,
                        Box::new(Expression::Lit("1".to_string())),
                    )),
                )),
            ),
            (
                "x == 0 || y <= 42",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Eq,
                        Box::new(Expression::Lit("0".to_string())),
                    )),
                    Operator::Or,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("y".to_string())),
                        Operator::Lte,
                        Box::new(Expression::Lit("42".to_string())),
                    )),
                )),
            ),
            (
                "(fn(a,b) {return a+b;})(1,2)",
                Ok(Expression::Apply(
                    Box::new(Expression::Fn(
                        vec!["a".to_string(), "b".to_string()],
                        vec![Statement::Ret(Expression::Infix(
                            Box::new(Expression::Var("a".to_string())),
                            Operator::Add,
                            Box::new(Expression::Var("b".to_string())),
                        ))],
                    )),
                    vec![
                        Expression::Lit("1".to_string()),
                        Expression::Lit("2".to_string()),
                    ],
                )),
            ),
            (
                "a = b",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("a".to_string())),
                    Operator::Bind,
                    Box::new(Expression::Var("b".to_string())),
                )),
            ),
            (
                "x == true",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("x".to_string())),
                    Operator::Eq,
                    Box::new(Expression::Lit("true".to_string())),
                )),
            ),
            (
                "x == false",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("x".to_string())),
                    Operator::Eq,
                    Box::new(Expression::Lit("false".to_string())),
                )),
            ),
        ];

        for (src, expected) in tests {
            let tokens = tokenize(&mut Buffer::from_string(src)).unwrap();
            let buffer = Buffer::new(tokens);
            let parsed = parse_expression(&buffer, Operator::MIN_RANK);

            assert_eq!(parsed, expected, "{}", src);
        }
    }
}
