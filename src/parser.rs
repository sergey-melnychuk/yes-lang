use num_bigint::BigInt;

use std::cell::Cell;
use std::ops::Deref;

use crate::buffer::{Buffer, Iterable};
use crate::error::ParserError;
use crate::token::*;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Operator {
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
    In,
}

impl Operator {
    pub(crate) const MIN_RANK: usize = 0;

    pub(crate) fn rank(&self) -> usize {
        match self {
            Operator::Not | Operator::Neg => 7,
            Operator::Mul | Operator::Div | Operator::Mod => 6,
            Operator::Add | Operator::Sub => 5,
            Operator::Lt | Operator::Lte | Operator::Gt | Operator::Gte | Operator::In => 4,
            Operator::Eq | Operator::Ne => 3,
            Operator::And => 2,
            Operator::Or => 1,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Statement {
    Let(String, Expression),
    Ret(Expression),
    If(Expression, Vec<Statement>, Vec<Statement>),
    Fn(String, Vec<String>, Vec<Statement>),
    Expr(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Literal {
    Null,
    Int(BigInt),
    Float(f64),
    Bool(bool),
    Str(String),
    Bytes(Vec<u8>),
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expression {
    Var(String),
    Lit(Literal),
    Fn(Vec<String>, Vec<Statement>),
    Apply(Box<Expression>, Vec<Expression>),
    Prefix(Operator, Box<Expression>),
    Infix(Box<Expression>, Operator, Box<Expression>),
    Index(Box<Expression>, Box<Expression>),
    Slice(
        Box<Expression>,
        Option<Box<Expression>>,
        Option<Box<Expression>>,
    ),
    Field(Box<Expression>, String),
    List(Vec<Expression>),
    Record(Vec<(String, Expression)>),
    // position of an expression that can fail at runtime, to report errors
    At(Pos, Box<Expression>),
}

impl Expression {
    pub(crate) fn position(&self) -> Option<Pos> {
        match self {
            Expression::At(pos, _) => Some(*pos),
            _ => None,
        }
    }

    // the expression without its position
    pub(crate) fn inner(&self) -> &Expression {
        match self {
            Expression::At(_, expr) => expr.inner(),
            expr => expr,
        }
    }
}

/// The scope a block needs to run in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BlockScope {
    /// No scope of its own: the block binds nothing ('let', 'fn').
    None,
    /// A scope no closure can capture, so it cannot be a part of a cycle.
    Untracked,
    /// A scope that closures created in the block can capture (see 'eval::track').
    Tracked,
}

impl BlockScope {
    pub(crate) fn of(statements: &[Statement]) -> Self {
        if defines_functions(statements) {
            BlockScope::Tracked
        } else if statements
            .iter()
            .any(|stmt| matches!(stmt, Statement::Let(..) | Statement::Fn(..)))
        {
            BlockScope::Untracked
        } else {
            BlockScope::None
        }
    }
}

/// Whether the statements create functions (at any depth): only then a scope in which they
/// run can be captured by a closure.
pub(crate) fn defines_functions(statements: &[Statement]) -> bool {
    statements.iter().any(|stmt| match stmt {
        Statement::Fn(..) => true,
        Statement::Let(_, expr) | Statement::Ret(expr) | Statement::Expr(expr) => {
            creates_functions(expr)
        }
        Statement::If(cond, a, b) => {
            creates_functions(cond) || defines_functions(a) || defines_functions(b)
        }
    })
}

fn creates_functions(expr: &Expression) -> bool {
    let any = |exprs: &[Expression]| exprs.iter().any(creates_functions);
    match expr {
        Expression::Fn(..) => true,
        Expression::Var(_) | Expression::Lit(_) => false,
        Expression::At(_, expr) | Expression::Prefix(_, expr) | Expression::Field(expr, _) => {
            creates_functions(expr)
        }
        Expression::Infix(a, _, b) | Expression::Index(a, b) => {
            creates_functions(a) || creates_functions(b)
        }
        Expression::Apply(f, args) => creates_functions(f) || any(args),
        Expression::Slice(target, start, end) => {
            creates_functions(target)
                || start.as_deref().is_some_and(creates_functions)
                || end.as_deref().is_some_and(creates_functions)
        }
        Expression::List(items) => any(items),
        Expression::Record(fields) => fields.iter().any(|(_, expr)| creates_functions(expr)),
    }
}

fn at(pos: Pos, expr: Expression) -> Expression {
    Expression::At(pos, Box::new(expr))
}

/// Maximum nesting of expressions and blocks, like '((((...' or '1+1+...+1' (each '+' is
/// nested in the next one). Parsing and running grow the stack as needed, but dropping
/// a syntax tree recurses as deep as it is nested, and that cannot grow the stack.
pub(crate) const MAX_NESTING: usize = 200;

/// Tokens with their positions in the source.
pub(crate) struct Input {
    tokens: Buffer<Token>,
    positions: Vec<Pos>,
    nesting: Cell<usize>,
}

impl Input {
    pub(crate) fn new(tokens: Vec<Token>, positions: Vec<Pos>) -> Self {
        Input {
            tokens: Buffer::new(tokens),
            positions,
            nesting: Cell::new(0),
        }
    }

    // one more level of nesting
    fn nest(&self) -> Result<(), ParserError> {
        let nesting = self.nesting.get() + 1;
        if nesting > MAX_NESTING {
            return Err(ParserError::TooDeep(MAX_NESTING));
        }
        self.nesting.set(nesting);
        Ok(())
    }

    // runs 'parse', the nesting it adds is removed afterwards
    fn nested<T>(&self, parse: impl FnOnce() -> Result<T, ParserError>) -> Result<T, ParserError> {
        let nesting = self.nesting.get();
        let result = crate::grow(parse);
        self.nesting.set(nesting);
        result
    }

    /// Position of the token returned by the latest `next` or `peek`.
    pub(crate) fn pos(&self) -> Pos {
        let idx = self
            .tokens
            .last()
            .min(self.positions.len().saturating_sub(1));
        self.positions
            .get(idx)
            .copied()
            .unwrap_or(Pos { line: 1, col: 1 })
    }
}

impl Deref for Input {
    type Target = Buffer<Token>;

    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

pub(crate) fn parse(buffer: &Input) -> Result<Vec<Statement>, ParserError> {
    let result = parse_block(buffer)?;
    expect(buffer, &Token::End)?;
    Ok(result)
}

// parses statements up to (but not including) the closing '}' or the end of input
fn parse_block(buffer: &Input) -> Result<Vec<Statement>, ParserError> {
    let mut result = Vec::new();
    loop {
        match peek(buffer)? {
            Token::End | Token::Delimiter('}') => return Ok(result),
            Token::Delimiter(';') => {
                let _ = next(buffer)?;
            }
            _ => result.push(parse_statement(buffer)?),
        }
    }
}

fn parse_statement(buffer: &Input) -> Result<Statement, ParserError> {
    let token = next(buffer)?;
    if token == &Token::Keyword(LET) {
        let stmt = parse_let_statement(buffer)?;
        end_of_statement(buffer)?;
        Ok(stmt)
    } else if token == &Token::Keyword(RETURN) {
        let expr = parse_expression(buffer, Operator::MIN_RANK)?;
        end_of_statement(buffer)?;
        Ok(Statement::Ret(expr))
    } else if token == &Token::Keyword(IF) {
        parse_if_statement(buffer)
    } else if token == &Token::Keyword(FN) && is_identifier(peek(buffer)?) {
        parse_fn_definition(buffer)
    } else {
        buffer.back();
        let expr = parse_expression(buffer, Operator::MIN_RANK)?;
        end_of_statement(buffer)?;
        Ok(Statement::Expr(expr))
    }
}

// a statement is terminated by ';', or it is the last one in a block
fn end_of_statement(buffer: &Input) -> Result<(), ParserError> {
    match peek(buffer)? {
        Token::Delimiter(';') => {
            let _ = next(buffer)?;
            Ok(())
        }
        Token::Delimiter('}') | Token::End => Ok(()),
        token => Err(ParserError::Unexpected(token.clone())),
    }
}

fn next(buffer: &Input) -> Result<&Token, ParserError> {
    if let Some(token) = buffer.next() {
        Ok(token)
    } else {
        Err(ParserError::Unexpected(Token::End))
    }
}

fn peek(buffer: &Input) -> Result<&Token, ParserError> {
    if let Some(token) = buffer.peek() {
        Ok(token)
    } else {
        Err(ParserError::Unexpected(Token::End))
    }
}

fn expect(buffer: &Input, expected: &Token) -> Result<(), ParserError> {
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

fn expect_identifier(buffer: &Input) -> Result<String, ParserError> {
    match next(buffer)? {
        Token::Identifier(id) => Ok(id.to_owned()),
        token => Err(ParserError::Mismatch(
            Token::Identifier("*".to_string()),
            token.clone(),
        )),
    }
}

fn is_identifier(token: &Token) -> bool {
    matches!(token, Token::Identifier(_))
}

fn is_float(s: &str) -> bool {
    let dots = s.chars().filter(|c| *c == '.').count();
    dots == 1 && s.chars().filter(|c| *c != '.').all(|c| c.is_ascii_digit())
}

fn parse_literal(lit: &str) -> Result<Literal, ParserError> {
    let invalid = || ParserError::InvalidLiteral(lit.to_string());
    if lit.chars().all(|c| c.is_ascii_digit()) {
        lit.parse().map(Literal::Int).map_err(|_| invalid())
    } else if is_float(lit) {
        lit.parse().map(Literal::Float).map_err(|_| invalid())
    } else {
        Err(invalid())
    }
}

trait PrefixParser {
    fn parse(&mut self, token: &Token, buffer: &Input) -> Result<Expression, ParserError>;
}

impl PrefixParser for Operator {
    fn parse(&mut self, token: &Token, buffer: &Input) -> Result<Expression, ParserError> {
        match self {
            Operator::Not | Operator::Neg => {
                let rhs = parse_expression(buffer, self.rank())?;
                Ok(Expression::Prefix(self.clone(), Box::new(rhs)))
            }
            _ => Err(ParserError::Prefix(token.clone())),
        }
    }
}

fn get_prefix_parser(token: &Token) -> Option<impl PrefixParser + use<>> {
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
        buffer: &Input,
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
        buffer: &Input,
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
            | Operator::In => {
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

fn get_infix_parser(token: &Token) -> Option<impl InfixParser + use<>> {
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
        Token::Keyword(IN) => Some(Operator::In),
        _ => None,
    }
}

fn is_postfix(token: &Token) -> bool {
    matches!(
        token,
        Token::Delimiter('(') | Token::Delimiter('[') | Token::Delimiter('.')
    )
}

fn parse_expression(buffer: &Input, rank: usize) -> Result<Expression, ParserError> {
    buffer.nested(|| parse_nested_expression(buffer, rank))
}

fn parse_nested_expression(buffer: &Input, rank: usize) -> Result<Expression, ParserError> {
    buffer.nest()?;
    let token = next(buffer)?;
    let pos = buffer.pos();
    let mut expr = match token {
        Token::Operator(_) => match get_prefix_parser(token) {
            Some(mut op) => at(pos, op.parse(token, buffer)?),
            None => return Err(ParserError::Prefix(token.clone())),
        },
        Token::Literal(lit) => Expression::Lit(parse_literal(lit)?),
        Token::Str(s) => Expression::Lit(Literal::Str(s.to_owned())),
        Token::Bytes(b) => Expression::Lit(Literal::Bytes(b.to_owned())),
        Token::Identifier(id) => at(pos, Expression::Var(id.to_owned())),
        Token::Keyword(FN) => parse_fn_expression(buffer)?,
        Token::Keyword(TRUE) => Expression::Lit(Literal::Bool(true)),
        Token::Keyword(FALSE) => Expression::Lit(Literal::Bool(false)),
        Token::Keyword(NULL) => Expression::Lit(Literal::Null),
        Token::Delimiter('(') => {
            let expr = parse_expression(buffer, Operator::MIN_RANK)?;
            expect(buffer, &Token::Delimiter(')'))?;
            expr
        }
        Token::Delimiter('[') => Expression::List(parse_list(buffer, ']')?),
        Token::Delimiter('{') => Expression::Record(parse_record(buffer)?),
        _ => return Err(ParserError::Unexpected(token.clone())),
    };

    loop {
        let token = peek(buffer)?;
        // calls, indexing, slicing and field access bind tighter than any operator
        if is_postfix(token) {
            buffer.nest()?; // 'expr' becomes nested in the postfix expression
            expr = parse_postfix(expr, buffer)?;
            continue;
        }
        match get_infix_parser(token) {
            Some(mut op) if rank < op.rank() => {
                buffer.nest()?; // 'expr' becomes nested in the infix expression
                let _ = next(buffer)?;
                let pos = buffer.pos();
                expr = at(pos, op.parse(expr, token, buffer)?);
            }
            _ => break,
        }
    }

    Ok(expr)
}

// parses one of: call 'f(a, b)', index 'x[i]', slice 'x[a..b]' or field access 'x.name'
fn parse_postfix(expr: Expression, buffer: &Input) -> Result<Expression, ParserError> {
    let token = next(buffer)?;
    let pos = buffer.pos();
    parse_postfix_op(expr, token, buffer).map(|expr| at(pos, expr))
}

fn parse_postfix_op(
    expr: Expression,
    token: &Token,
    buffer: &Input,
) -> Result<Expression, ParserError> {
    match token {
        Token::Delimiter('(') => {
            let args = parse_list(buffer, ')')?;
            Ok(Expression::Apply(Box::new(expr), args))
        }
        Token::Delimiter('[') => {
            let start = if peek(buffer)? == &Token::Operator(RANGE) {
                None
            } else {
                Some(Box::new(parse_expression(buffer, Operator::MIN_RANK)?))
            };
            if peek(buffer)? != &Token::Operator(RANGE) {
                expect(buffer, &Token::Delimiter(']'))?;
                // 'start' is always present here: '[..' would have been a range
                return Ok(Expression::Index(Box::new(expr), start.unwrap()));
            }
            let _ = next(buffer)?;
            let end = if peek(buffer)? == &Token::Delimiter(']') {
                None
            } else {
                Some(Box::new(parse_expression(buffer, Operator::MIN_RANK)?))
            };
            expect(buffer, &Token::Delimiter(']'))?;
            Ok(Expression::Slice(Box::new(expr), start, end))
        }
        Token::Delimiter('.') => {
            let name = expect_identifier(buffer)?;
            Ok(Expression::Field(Box::new(expr), name))
        }
        token => Err(ParserError::Unexpected(token.clone())),
    }
}

fn parse_let_statement(buffer: &Input) -> Result<Statement, ParserError> {
    let name = expect_identifier(buffer)?;
    expect(buffer, &Token::Operator(BIND))?;
    let expr = parse_expression(buffer, Operator::MIN_RANK)?;
    Ok(Statement::Let(name, expr))
}

fn parse_if_statement(buffer: &Input) -> Result<Statement, ParserError> {
    // the condition is at the position of 'if', for an error when it is not a boolean
    let pos = buffer.pos();
    let cond = at(pos, parse_expression(buffer, Operator::MIN_RANK)?);
    let if_clause = parse_braced_block(buffer)?;
    let else_clause = if peek(buffer)? == &Token::Keyword(ELSE) {
        let _ = next(buffer)?;
        if peek(buffer)? == &Token::Keyword(IF) {
            // 'else if' is a shorthand for 'else { if ... }'
            let _ = next(buffer)?;
            vec![parse_if_statement(buffer)?]
        } else {
            parse_braced_block(buffer)?
        }
    } else {
        vec![]
    };
    Ok(Statement::If(cond, if_clause, else_clause))
}

fn parse_braced_block(buffer: &Input) -> Result<Vec<Statement>, ParserError> {
    buffer.nested(|| {
        buffer.nest()?;
        expect(buffer, &Token::Delimiter('{'))?;
        let block = parse_block(buffer)?;
        expect(buffer, &Token::Delimiter('}'))?;
        Ok(block)
    })
}

fn parse_fn_params(buffer: &Input) -> Result<Vec<String>, ParserError> {
    let mut params = Vec::new();
    expect(buffer, &Token::Delimiter('('))?;
    if peek(buffer)? == &Token::Delimiter(')') {
        let _ = next(buffer)?;
        return Ok(params);
    }
    loop {
        params.push(expect_identifier(buffer)?);
        match next(buffer)? {
            Token::Delimiter(')') => return Ok(params),
            Token::Delimiter(',') => continue,
            token => return Err(ParserError::Mismatch(Token::Delimiter(')'), token.clone())),
        }
    }
}

// parses comma-separated expressions after the opening bracket up to and including
// the closing one, a trailing comma is allowed: call arguments 'f(a, b)' or a list '[a, b,]'
fn parse_list(buffer: &Input, closing: char) -> Result<Vec<Expression>, ParserError> {
    let mut items = Vec::new();
    loop {
        if peek(buffer)? == &Token::Delimiter(closing) {
            let _ = next(buffer)?;
            return Ok(items);
        }
        items.push(parse_expression(buffer, Operator::MIN_RANK)?);
        match next(buffer)? {
            Token::Delimiter(c) if *c == closing => return Ok(items),
            Token::Delimiter(',') => continue,
            token => {
                return Err(ParserError::Mismatch(
                    Token::Delimiter(closing),
                    token.clone(),
                ));
            }
        }
    }
}

// parses 'name: expr' or '"any key": expr' pairs after the opening '{' up to and including
// the closing '}', a trailing comma is allowed
fn parse_record(buffer: &Input) -> Result<Vec<(String, Expression)>, ParserError> {
    let mut fields: Vec<(String, Expression)> = Vec::new();
    loop {
        let key = match next(buffer)? {
            Token::Delimiter('}') => return Ok(fields),
            Token::Identifier(key) | Token::Str(key) => key.clone(),
            token => {
                return Err(ParserError::Mismatch(
                    Token::Identifier("*".to_string()),
                    token.clone(),
                ));
            }
        };
        if fields.iter().any(|(k, _)| *k == key) {
            return Err(ParserError::DuplicateKey(key));
        }
        expect(buffer, &Token::Delimiter(':'))?;
        fields.push((key, parse_expression(buffer, Operator::MIN_RANK)?));
        match next(buffer)? {
            Token::Delimiter('}') => return Ok(fields),
            Token::Delimiter(',') => continue,
            token => return Err(ParserError::Mismatch(Token::Delimiter('}'), token.clone())),
        }
    }
}

fn parse_fn_definition(buffer: &Input) -> Result<Statement, ParserError> {
    let name = expect_identifier(buffer)?;
    let params = parse_fn_params(buffer)?;
    let body = parse_braced_block(buffer)?;
    Ok(Statement::Fn(name, params, body))
}

fn parse_fn_expression(buffer: &Input) -> Result<Expression, ParserError> {
    let params = parse_fn_params(buffer)?;
    let body = parse_braced_block(buffer)?;
    Ok(Expression::Fn(params, body))
}

// removes the positions, so that tests can compare the tree
#[cfg(test)]
pub(crate) fn strip(statements: Vec<Statement>) -> Vec<Statement> {
    statements
        .into_iter()
        .map(|stmt| match stmt {
            Statement::Let(name, expr) => Statement::Let(name, strip_expr(expr)),
            Statement::Ret(expr) => Statement::Ret(strip_expr(expr)),
            Statement::If(cond, a, b) => Statement::If(strip_expr(cond), strip(a), strip(b)),
            Statement::Fn(name, params, body) => Statement::Fn(name, params, strip(body)),
            Statement::Expr(expr) => Statement::Expr(strip_expr(expr)),
        })
        .collect()
}

#[cfg(test)]
pub(crate) fn strip_expr(expr: Expression) -> Expression {
    let bx = |expr: Box<Expression>| Box::new(strip_expr(*expr));
    let all = |exprs: Vec<Expression>| exprs.into_iter().map(strip_expr).collect();
    match expr {
        Expression::At(_, expr) => strip_expr(*expr),
        Expression::Var(_) | Expression::Lit(_) => expr,
        Expression::Fn(params, body) => Expression::Fn(params, strip(body)),
        Expression::Apply(f, args) => Expression::Apply(bx(f), all(args)),
        Expression::Prefix(op, rhs) => Expression::Prefix(op, bx(rhs)),
        Expression::Infix(lhs, op, rhs) => Expression::Infix(bx(lhs), op, bx(rhs)),
        Expression::Index(target, index) => Expression::Index(bx(target), bx(index)),
        Expression::Slice(target, start, end) => {
            Expression::Slice(bx(target), start.map(bx), end.map(bx))
        }
        Expression::Field(target, name) => Expression::Field(bx(target), name),
        Expression::List(items) => Expression::List(all(items)),
        Expression::Record(fields) => Expression::Record(
            fields
                .into_iter()
                .map(|(name, expr)| (name, strip_expr(expr)))
                .collect(),
        ),
    }
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
                Err(ParserError::Mismatch(
                    Token::Operator(BIND),
                    Token::Delimiter(';'),
                )),
            ),
            ("let x =", Err(ParserError::Unexpected(Token::End))),
            (
                "{",
                Err(ParserError::Mismatch(
                    Token::Identifier("*".to_string()),
                    Token::End,
                )),
            ),
            (
                "{}; {a: 1, \"b c\": x,}",
                Ok(vec![
                    Statement::Expr(Expression::Record(vec![])),
                    Statement::Expr(Expression::Record(vec![
                        ("a".to_string(), Expression::Lit(Literal::Int(1.into()))),
                        ("b c".to_string(), Expression::Var("x".to_string())),
                    ])),
                ]),
            ),
            (
                "{a: 1, a: 2}",
                Err(ParserError::DuplicateKey("a".to_string())),
            ),
            (
                "{1: 2}",
                Err(ParserError::Mismatch(
                    Token::Identifier("*".to_string()),
                    Token::Literal("1".to_string()),
                )),
            ),
            (
                "{a 1}",
                Err(ParserError::Mismatch(
                    Token::Delimiter(':'),
                    Token::Literal("1".to_string()),
                )),
            ),
            (
                "{a: 1 b: 2}",
                Err(ParserError::Mismatch(
                    Token::Delimiter('}'),
                    Token::Identifier("b".to_string()),
                )),
            ),
            // a record in a condition
            (
                "if x == {} { 1 }",
                Ok(vec![Statement::If(
                    Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Eq,
                        Box::new(Expression::Record(vec![])),
                    ),
                    vec![Statement::Expr(Expression::Lit(Literal::Int(1.into())))],
                    vec![],
                )]),
            ),
            ("[", Err(ParserError::Unexpected(Token::End))),
            (
                "[1 2]",
                Err(ParserError::Mismatch(
                    Token::Delimiter(']'),
                    Token::Literal("2".to_string()),
                )),
            ),
            (
                "if a { 1 } else if b { 2 } else { 3 }",
                Ok(vec![Statement::If(
                    Expression::Var("a".to_string()),
                    vec![Statement::Expr(Expression::Lit(Literal::Int(1.into())))],
                    vec![Statement::If(
                        Expression::Var("b".to_string()),
                        vec![Statement::Expr(Expression::Lit(Literal::Int(2.into())))],
                        vec![Statement::Expr(Expression::Lit(Literal::Int(3.into())))],
                    )],
                )]),
            ),
            (
                "}",
                Err(ParserError::Mismatch(Token::End, Token::Delimiter('}'))),
            ),
            // input must not be silently dropped after an expression
            (
                "1; 2",
                Ok(vec![
                    Statement::Expr(Expression::Lit(Literal::Int(1.into()))),
                    Statement::Expr(Expression::Lit(Literal::Int(2.into()))),
                ]),
            ),
            (
                "1 2",
                Err(ParserError::Unexpected(Token::Literal("2".to_string()))),
            ),
            (
                "let x = 1; x +; let y = 2; y",
                Err(ParserError::Unexpected(Token::Delimiter(';'))),
            ),
            (
                "f(1 2)",
                Err(ParserError::Mismatch(
                    Token::Delimiter(')'),
                    Token::Literal("2".to_string()),
                )),
            ),
            (
                "fn(,) {}",
                Err(ParserError::Mismatch(
                    Token::Identifier("*".to_string()),
                    Token::Delimiter(','),
                )),
            ),
            (
                "if x { 1",
                Err(ParserError::Mismatch(Token::Delimiter('}'), Token::End)),
            ),
            // an operator without infix form must not hang the parser
            ("1 ! 2", Err(ParserError::Unexpected(Token::Operator(BANG)))),
            ("a = b", Err(ParserError::Unexpected(Token::Operator(BIND)))),
            // typed literals are validated at parse time
            // integers have arbitrary precision
            (
                "99999999999999999999",
                Ok(vec![Statement::Expr(Expression::Lit(Literal::Int(
                    "99999999999999999999".parse().unwrap(),
                )))]),
            ),
            ("1abc", Err(ParserError::InvalidLiteral("1abc".to_string()))),
            (
                "abc\"def\"",
                Err(ParserError::Unexpected(Token::Str("def".to_string()))),
            ),
            (
                "let x = 1.5; \"s\"; b\"ff\"",
                Ok(vec![
                    Statement::Let("x".to_string(), Expression::Lit(Literal::Float(1.5))),
                    Statement::Expr(Expression::Lit(Literal::Str("s".to_string()))),
                    Statement::Expr(Expression::Lit(Literal::Bytes(vec![0xff]))),
                ]),
            ),
            (
                "if a { return 1 } else { 2 }",
                Ok(vec![Statement::If(
                    Expression::Var("a".to_string()),
                    vec![Statement::Ret(Expression::Lit(Literal::Int(1.into())))],
                    vec![Statement::Expr(Expression::Lit(Literal::Int(2.into())))],
                )]),
            ),
            (
                "let answer = 42;",
                Ok(vec![Statement::Let(
                    "answer".to_string(),
                    Expression::Lit(Literal::Int(42.into())),
                )]),
            ),
            (
                "let result = x + 1;",
                Ok(vec![Statement::Let(
                    "result".to_string(),
                    Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit(Literal::Int(1.into()))),
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
                        Box::new(Expression::Lit(Literal::Int(42.into()))),
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
                            Box::new(Expression::Lit(Literal::Int(16.into()))),
                        )),
                        Operator::Mul,
                        Box::new(Expression::Var("k".to_string())),
                    )),
                    Operator::Add,
                    Box::new(Expression::Lit(Literal::Int(4.into()))),
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
                            Box::new(Expression::Lit(Literal::Int(2.into()))),
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
                            Expression::Lit(Literal::Int(1.into())),
                            Expression::Lit(Literal::Int(2.into())),
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
                        Box::new(Expression::Lit(Literal::Int(0.into()))),
                    ),
                    vec![Statement::Expr(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit(Literal::Int(1.into()))),
                    ))],
                    vec![Statement::Expr(Expression::Infix(
                        Box::new(Expression::Var("y".to_string())),
                        Operator::Sub,
                        Box::new(Expression::Lit(Literal::Int(2.into()))),
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
                        Box::new(Expression::Lit(Literal::Int(0.into()))),
                    ),
                    vec![
                        Statement::Let(
                            "y".to_string(),
                            Expression::Infix(
                                Box::new(Expression::Var("x".to_string())),
                                Operator::Mul,
                                Box::new(Expression::Lit(Literal::Int(2.into()))),
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
                        Expression::Lit(Literal::Int(1.into())),
                        Expression::Lit(Literal::Int(2.into())),
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
                Ok(vec![Statement::Let(
                    "f".to_string(),
                    Expression::Fn(
                        vec!["a".to_string(), "b".to_string()],
                        vec![Statement::Expr(Expression::Infix(
                            Box::new(Expression::Var("a".to_string())),
                            Operator::Add,
                            Box::new(Expression::Var("b".to_string())),
                        ))],
                    ),
                )]),
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
                    Box::new(Expression::Lit(Literal::Int(1.into()))),
                    Operator::Add,
                    Box::new(Expression::Lit(Literal::Int(2.into()))),
                ))]),
            ),
            (
                "(1) + 2",
                Ok(vec![Statement::Expr(Expression::Infix(
                    Box::new(Expression::Lit(Literal::Int(1.into()))),
                    Operator::Add,
                    Box::new(Expression::Lit(Literal::Int(2.into()))),
                ))]),
            ),
        ];

        for (src, expected) in tests {
            let tokens = tokenize(&Buffer::from_string(src)).unwrap();
            let parsed = parse(&Input::new(tokens, vec![])).map(strip);

            assert_eq!(parsed, expected, "src=\"{}\"", src);
        }
    }

    #[test]
    fn test_block_scope() {
        let scope = |src: &str| {
            let tokens = tokenize(&Buffer::from_string(src)).unwrap();
            BlockScope::of(&parse(&Input::new(tokens, vec![])).unwrap())
        };
        assert_eq!(scope("1 + 2; f(x)[0].y"), BlockScope::None);
        assert_eq!(
            scope("if a { let x = 1; } else { fn() { 1 } }"),
            BlockScope::Tracked
        );
        assert_eq!(scope("if a { let x = 1; }"), BlockScope::None);
        assert_eq!(scope("let x = 1;"), BlockScope::Untracked);
        assert_eq!(scope("fn f() {}"), BlockScope::Tracked);
        // a function anywhere in an expression
        for src in [
            "let x = [1, fn() {}];",
            "let x = {a: fn() {}};",
            "x[..fn() {}()]",
            "f(g(fn() {}))",
            "-(fn() { 1 })()",
            "if fn() { true }() {}",
            "return x.y == fn() {};",
        ] {
            assert_eq!(scope(src), BlockScope::Tracked, "{}", src);
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
                    Box::new(Expression::Lit(Literal::Int(1.into()))),
                    Operator::Add,
                    Box::new(Expression::Lit(Literal::Int(2.into()))),
                )),
            ),
            (
                "z != 42",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("z".to_string())),
                    Operator::Ne,
                    Box::new(Expression::Lit(Literal::Int(42.into()))),
                )),
            ),
            (
                "a <= 2",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("a".to_string())),
                    Operator::Lte,
                    Box::new(Expression::Lit(Literal::Int(2.into()))),
                )),
            ),
            (
                "n % 42",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("n".to_string())),
                    Operator::Mod,
                    Box::new(Expression::Lit(Literal::Int(42.into()))),
                )),
            ),
            (
                "x + 1 / 2",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("x".to_string())),
                    Operator::Add,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Lit(Literal::Int(1.into()))),
                        Operator::Div,
                        Box::new(Expression::Lit(Literal::Int(2.into()))),
                    )),
                )),
            ),
            (
                "a * 2 + 1",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("a".to_string())),
                        Operator::Mul,
                        Box::new(Expression::Lit(Literal::Int(2.into()))),
                    )),
                    Operator::Add,
                    Box::new(Expression::Lit(Literal::Int(1.into()))),
                )),
            ),
            (
                "(abc + 3) * 4",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("abc".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit(Literal::Int(3.into()))),
                    )),
                    Operator::Mul,
                    Box::new(Expression::Lit(Literal::Int(4.into()))),
                )),
            ),
            ("(((42)))", Ok(Expression::Lit(Literal::Int(42.into())))),
            (
                "2 * (x + 3)",
                Ok(Expression::Infix(
                    Box::new(Expression::Lit(Literal::Int(2.into()))),
                    Operator::Mul,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Add,
                        Box::new(Expression::Lit(Literal::Int(3.into()))),
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
                        Box::new(Expression::Lit(Literal::Int(42.into()))),
                    )),
                    Operator::And,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Ne,
                        Box::new(Expression::Lit(Literal::Int(1.into()))),
                    )),
                )),
            ),
            (
                "x == 0 || y <= 42",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("x".to_string())),
                        Operator::Eq,
                        Box::new(Expression::Lit(Literal::Int(0.into()))),
                    )),
                    Operator::Or,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("y".to_string())),
                        Operator::Lte,
                        Box::new(Expression::Lit(Literal::Int(42.into()))),
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
                        Expression::Lit(Literal::Int(1.into())),
                        Expression::Lit(Literal::Int(2.into())),
                    ],
                )),
            ),
            // '&&' binds tighter than '||'
            (
                "a || b && c",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("a".to_string())),
                    Operator::Or,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Var("b".to_string())),
                        Operator::And,
                        Box::new(Expression::Var("c".to_string())),
                    )),
                )),
            ),
            // any expression can be called, not only names and 'fn' literals
            (
                "(f)(1)",
                Ok(Expression::Apply(
                    Box::new(Expression::Var("f".to_string())),
                    vec![Expression::Lit(Literal::Int(1.into()))],
                )),
            ),
            (
                "f()(2)",
                Ok(Expression::Apply(
                    Box::new(Expression::Apply(
                        Box::new(Expression::Var("f".to_string())),
                        vec![],
                    )),
                    vec![Expression::Lit(Literal::Int(2.into()))],
                )),
            ),
            (
                "tx.data[0..4] == b\"a9059cbb\"",
                Ok(Expression::Infix(
                    Box::new(Expression::Slice(
                        Box::new(Expression::Field(
                            Box::new(Expression::Var("tx".to_string())),
                            "data".to_string(),
                        )),
                        Some(Box::new(Expression::Lit(Literal::Int(0.into())))),
                        Some(Box::new(Expression::Lit(Literal::Int(4.into())))),
                    )),
                    Operator::Eq,
                    Box::new(Expression::Lit(Literal::Bytes(vec![
                        0xa9, 0x05, 0x9c, 0xbb,
                    ]))),
                )),
            ),
            (
                "x[1..] + x[..n - 1] + x[..]",
                Ok(Expression::Infix(
                    Box::new(Expression::Infix(
                        Box::new(Expression::Slice(
                            Box::new(Expression::Var("x".to_string())),
                            Some(Box::new(Expression::Lit(Literal::Int(1.into())))),
                            None,
                        )),
                        Operator::Add,
                        Box::new(Expression::Slice(
                            Box::new(Expression::Var("x".to_string())),
                            None,
                            Some(Box::new(Expression::Infix(
                                Box::new(Expression::Var("n".to_string())),
                                Operator::Sub,
                                Box::new(Expression::Lit(Literal::Int(1.into()))),
                            ))),
                        )),
                    )),
                    Operator::Add,
                    Box::new(Expression::Slice(
                        Box::new(Expression::Var("x".to_string())),
                        None,
                        None,
                    )),
                )),
            ),
            ("[]", Ok(Expression::List(vec![]))),
            (
                "[1, x,]",
                Ok(Expression::List(vec![
                    Expression::Lit(Literal::Int(1.into())),
                    Expression::Var("x".to_string()),
                ])),
            ),
            (
                "f(1,)",
                Ok(Expression::Apply(
                    Box::new(Expression::Var("f".to_string())),
                    vec![Expression::Lit(Literal::Int(1.into()))],
                )),
            ),
            // 'in' binds like comparisons: tighter than '==', looser than '+'
            (
                "a == b + 1 in c",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("a".to_string())),
                    Operator::Eq,
                    Box::new(Expression::Infix(
                        Box::new(Expression::Infix(
                            Box::new(Expression::Var("b".to_string())),
                            Operator::Add,
                            Box::new(Expression::Lit(Literal::Int(1.into()))),
                        )),
                        Operator::In,
                        Box::new(Expression::Var("c".to_string())),
                    )),
                )),
            ),
            (
                "[1, 2][0]",
                Ok(Expression::Index(
                    Box::new(Expression::List(vec![
                        Expression::Lit(Literal::Int(1.into())),
                        Expression::Lit(Literal::Int(2.into())),
                    ])),
                    Box::new(Expression::Lit(Literal::Int(0.into()))),
                )),
            ),
            // postfix binds tighter than prefix
            (
                "-a.b[\"c\"](1)",
                Ok(Expression::Prefix(
                    Operator::Neg,
                    Box::new(Expression::Apply(
                        Box::new(Expression::Index(
                            Box::new(Expression::Field(
                                Box::new(Expression::Var("a".to_string())),
                                "b".to_string(),
                            )),
                            Box::new(Expression::Lit(Literal::Str("c".to_string()))),
                        )),
                        vec![Expression::Lit(Literal::Int(1.into()))],
                    )),
                )),
            ),
            (
                "a.1",
                Err(ParserError::Mismatch(
                    Token::Identifier("*".to_string()),
                    Token::Literal("1".to_string()),
                )),
            ),
            ("a[]", Err(ParserError::Unexpected(Token::Delimiter(']')))),
            (
                "a[1",
                Err(ParserError::Mismatch(Token::Delimiter(']'), Token::End)),
            ),
            (
                "x == true",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("x".to_string())),
                    Operator::Eq,
                    Box::new(Expression::Lit(Literal::Bool(true))),
                )),
            ),
            (
                "x == false",
                Ok(Expression::Infix(
                    Box::new(Expression::Var("x".to_string())),
                    Operator::Eq,
                    Box::new(Expression::Lit(Literal::Bool(false))),
                )),
            ),
        ];

        for (src, expected) in tests {
            let tokens = tokenize(&Buffer::from_string(src)).unwrap();
            let parsed =
                parse_expression(&Input::new(tokens, vec![]), Operator::MIN_RANK).map(strip_expr);

            assert_eq!(parsed, expected, "{}", src);
        }
    }
}
