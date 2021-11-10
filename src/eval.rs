use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::error::EvalError;
use crate::parser::{Expression, Operator, Statement};
use crate::token::*;

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Object {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Func(Vec<String>, Vec<Statement>, Snapshot),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Unit => write!(f, "()"),
            Object::Int(x) => write!(f, "{}", x),
            Object::Float(x) => write!(f, "{}", x),
            Object::Bool(true) => write!(f, "true"),
            Object::Bool(false) => write!(f, "false"),
            Object::Str(x) => write!(f, "\"{}\"", x),
            Object::Func(_, _, _) => write!(f, "<function>"),
        }
    }
}

fn is_quoted(s: &str) -> bool {
    s.starts_with('\"') && s.ends_with('\"')
}

fn unquote(s: &str) -> String {
    if is_quoted(s) {
        let n = s.len();
        s.chars().skip(1).take(n - 2).collect()
    } else {
        s.to_owned()
    }
}

fn is_integer(s: &str) -> bool {
    s.chars().all(|c| c.is_numeric())
}

fn is_float(s: &str) -> bool {
    let dots = s.chars().filter(|c| *c == '.').count();
    dots == 1 && s.chars().filter(|c| *c != '.').all(|c| c.is_numeric())
}

#[derive(Default, Debug, Clone)]
pub(crate) struct Context<'a> {
    objects: HashMap<String, Object>,
    parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    fn fork(&'a self) -> Self {
        Self {
            parent: Some(self),
            objects: Default::default(),
        }
    }

    fn put(&mut self, name: String, object: Object) {
        self.objects.insert(name, object);
    }

    fn get(&self, name: &str) -> Option<&Object> {
        self.objects.get(name).or_else(|| match self.parent {
            Some(p) => p.get(name),
            None => None,
        })
    }

    fn has(&self, name: &str) -> bool {
        self.objects.contains_key(name) || self.parent.map(|p| p.has(name)).unwrap_or_default()
    }

    fn snapshot(&self) -> Snapshot {
        if self.objects.is_empty() {
            self.parent.map(|p| p.snapshot()).unwrap_or_default()
        } else {
            Snapshot {
                objects: self.objects.clone(),
            }
        }
    }

    fn extend(&'a self, snapshot: &Snapshot) -> Self {
        let mut ctx = self.fork();
        for (k, v) in snapshot.objects.iter() {
            ctx.put(k.clone(), v.clone());
        }
        ctx
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub(crate) struct Snapshot {
    objects: HashMap<String, Object>,
}

pub(crate) fn eval(statements: &[Statement], ctx: &mut Context) -> Result<Object, EvalError> {
    let mut result = Object::Unit;
    for stmt in statements {
        result = eval_stmt(stmt, ctx)?;
        if let Statement::Ret(_) = stmt {
            break;
        }
    }
    Ok(result)
}

fn eval_stmt(stmt: &Statement, ctx: &mut Context) -> Result<Object, EvalError> {
    match stmt {
        Statement::Let(name, expr) => {
            let obj = eval_expr(expr, ctx)?;
            ctx.put(name.clone(), obj);
            Ok(Object::Unit)
        }

        Statement::Ret(expr) => eval_expr(expr, ctx),

        Statement::If(expr, if_clause, else_clause) => {
            let cond = eval_expr(expr, ctx)?;
            match cond {
                Object::Bool(true) => eval(if_clause, ctx),
                Object::Bool(false) => eval(else_clause, ctx),
                obj => Err(EvalError::NotBoolean(obj)),
            }
        }

        Statement::Fn(name, args, body) => {
            // TODO Limit snapshot only to variables referenced in the function body?
            let snapshot = ctx.snapshot();
            let f = Object::Func(args.to_owned(), body.to_owned(), snapshot);
            ctx.put(name.clone(), f);
            Ok(Object::Unit)
        }

        Statement::Call(lhs, rhs) => eval_fn_expr(lhs, rhs, ctx),

        Statement::Expr(expr) => eval_expr(expr, ctx),
    }
}

fn eval_expr(expr: &Expression, ctx: &Context) -> Result<Object, EvalError> {
    match expr {
        Expression::Lit(str) if TRUE == str => Ok(Object::Bool(true)),
        Expression::Lit(str) if FALSE == str => Ok(Object::Bool(false)),
        Expression::Lit(str) if is_quoted(str) => Ok(Object::Str(unquote(str))),
        Expression::Lit(int) if is_integer(int) => Ok(Object::Int(int.parse::<i64>()?)),
        Expression::Lit(fpn) if is_float(fpn) => Ok(Object::Float(fpn.parse::<f64>()?)),
        Expression::Lit(str) => Err(EvalError::InvalidLiteral(str.to_string())),

        Expression::Var(name) if ctx.has(name) => Ok(ctx.get(name).cloned().unwrap()),
        Expression::Var(name) => Err(EvalError::NotFound(name.to_string())),

        Expression::Prefix(op, rhs) => eval_prefix(op.clone(), eval_expr(rhs, ctx)?),

        Expression::Infix(lhs, op, rhs) => {
            eval_infix(op.clone(), eval_expr(lhs, ctx)?, eval_expr(rhs, ctx)?)
        }

        Expression::Fn(args, body) => Ok(Object::Func(
            args.to_owned(),
            body.to_owned(),
            ctx.snapshot(),
        )),

        Expression::Apply(lhs, rhs) => eval_fn_expr(lhs, rhs, ctx),
    }
}

fn eval_prefix(op: Operator, obj: Object) -> Result<Object, EvalError> {
    match (op, obj) {
        (Operator::Neg, Object::Int(x)) => Ok(Object::Int(-x)),
        (Operator::Neg, Object::Float(x)) => Ok(Object::Float(-x)),
        (Operator::Not, Object::Bool(b)) => Ok(Object::Bool(!b)),
        (op, obj) => Err(EvalError::PrefixOp(op, obj)),
    }
}

fn eval_infix(op: Operator, lhs: Object, rhs: Object) -> Result<Object, EvalError> {
    match (op, lhs, rhs) {
        (Operator::Add, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a + b)),
        (Operator::Sub, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a - b)),
        (Operator::Mul, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a * b)),
        (Operator::Div, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a / b)),
        (Operator::Mod, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a % b)),
        (Operator::Eq, Object::Int(a), Object::Int(b)) => Ok(Object::Bool(a == b)),
        (Operator::Ne, Object::Int(a), Object::Int(b)) => Ok(Object::Bool(a != b)),
        (Operator::Lt, Object::Int(a), Object::Int(b)) => Ok(Object::Bool(a < b)),
        (Operator::Lte, Object::Int(a), Object::Int(b)) => Ok(Object::Bool(a <= b)),
        (Operator::Gt, Object::Int(a), Object::Int(b)) => Ok(Object::Bool(a > b)),
        (Operator::Gte, Object::Int(a), Object::Int(b)) => Ok(Object::Bool(a >= b)),

        (Operator::Add, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a + b)),
        (Operator::Sub, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a - b)),
        (Operator::Mul, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a * b)),
        (Operator::Div, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a / b)),
        (Operator::Mod, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a % b)),
        (Operator::Eq, Object::Float(a), Object::Float(b)) => Ok(Object::Bool(a == b)),
        (Operator::Ne, Object::Float(a), Object::Float(b)) => Ok(Object::Bool(a != b)),
        (Operator::Lt, Object::Float(a), Object::Float(b)) => Ok(Object::Bool(a < b)),
        (Operator::Lte, Object::Float(a), Object::Float(b)) => Ok(Object::Bool(a <= b)),
        (Operator::Gt, Object::Float(a), Object::Float(b)) => Ok(Object::Bool(a > b)),
        (Operator::Gte, Object::Float(a), Object::Float(b)) => Ok(Object::Bool(a >= b)),

        (Operator::Add, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 + b)),
        (Operator::Sub, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 - b)),
        (Operator::Mul, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 * b)),
        (Operator::Div, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 / b)),
        (Operator::Mod, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 % b)),
        (Operator::Eq, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(a as f64 == b)),
        (Operator::Ne, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(a as f64 != b)),
        (Operator::Lt, Object::Int(a), Object::Float(b)) => Ok(Object::Bool((a as f64) < b)),
        (Operator::Lte, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(a as f64 <= b)),
        (Operator::Gt, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(a as f64 > b)),
        (Operator::Gte, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(a as f64 >= b)),

        (Operator::Add, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a + b as f64)),
        (Operator::Sub, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a - b as f64)),
        (Operator::Mul, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a * b as f64)),
        (Operator::Div, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a / b as f64)),
        (Operator::Mod, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a % b as f64)),
        (Operator::Eq, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a == b as f64)),
        (Operator::Ne, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a != b as f64)),
        (Operator::Lt, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a < b as f64)),
        (Operator::Lte, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a <= b as f64)),
        (Operator::Gt, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a > b as f64)),
        (Operator::Gte, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a >= b as f64)),

        (Operator::Add, Object::Str(a), Object::Str(b)) => Ok(Object::Str(a + &b)),
        (Operator::Eq, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a == b)),
        (Operator::Ne, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a != b)),
        (Operator::Lt, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a < b)),
        (Operator::Lte, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a <= b)),
        (Operator::Gt, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a > b)),
        (Operator::Gte, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a >= b)),

        (Operator::And, Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a && b)),
        (Operator::Or, Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a || b)),
        (Operator::Eq, Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a == b)),
        (Operator::Ne, Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a != b)),

        (op, lhs, rhs) => Err(EvalError::InfixOp(op, lhs, rhs)),
    }
}

fn eval_fn_expr(lhs: &Expression, rhs: &[Expression], ctx: &Context) -> Result<Object, EvalError> {
    match lhs {
        Expression::Apply(expr, args) => {
            if let Object::Func(args, body, snap) = eval_fn_expr(expr, args, ctx)? {
                eval_fn(&args, &body, rhs, ctx, &snap)
            } else {
                Err(EvalError::Apply(*expr.to_owned()))
            }
        }
        Expression::Var(name) if ctx.has(name) => {
            if let Object::Func(args, body, snap) = ctx.get(name).unwrap() {
                eval_fn(args, body, rhs, ctx, snap)
            } else {
                Err(EvalError::NotFunction(name.clone()))
            }
        }
        Expression::Var(name) => Err(EvalError::NotFound(name.to_string())),
        Expression::Fn(args, body) => eval_fn(args, body, rhs, ctx, &Snapshot::default()),
        _ => Err(EvalError::Apply(lhs.clone())),
    }
}

fn eval_fn(
    args: &[String],
    body: &[Statement],
    rhs: &[Expression],
    ctx: &Context,
    snap: &Snapshot,
) -> Result<Object, EvalError> {
    if args.len() != rhs.len() {
        return Err(EvalError::ApplyArgsCount(args.len(), rhs.len()));
    }

    let mut fn_ctx = ctx.extend(snap);
    for (name, expr) in args.iter().zip(rhs.iter()) {
        let obj = eval_expr(expr, ctx)?;
        fn_ctx.put(name.clone(), obj);
    }

    eval(body, &mut fn_ctx)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;
    use crate::lexer::tokenize;
    use crate::parser::parse;
    use crate::run_file;
    use std::path::Path;

    #[test]
    fn test_eval() {
        let tests = vec![
            (
                Expression::Lit(TRUE.to_string()),
                vec![],
                Ok(Object::Bool(true)),
            ),
            (
                Expression::Lit(FALSE.to_string()),
                vec![],
                Ok(Object::Bool(false)),
            ),
            (
                Expression::Lit("123".to_string()),
                vec![],
                Ok(Object::Int(123)),
            ),
            (
                Expression::Lit("1.23".to_string()),
                vec![],
                Ok(Object::Float(1.23)),
            ),
            (
                Expression::Lit("\"hello\"".to_string()),
                vec![],
                Ok(Object::Str("hello".to_string())),
            ),
            (
                Expression::Var("x".to_string()),
                vec![("x".to_string(), Object::Int(42))],
                Ok(Object::Int(42)),
            ),
        ];

        for (expr, map, obj) in tests {
            let ctx = map
                .into_iter()
                .fold(Context::default(), |mut ctx, (name, object)| {
                    ctx.put(name, object);
                    ctx
                });
            let res = eval_expr(&expr, &ctx);

            assert_eq!(res, obj, "expr={:?} obj={:?} res={:?}", expr, obj, res);
        }
    }

    #[test]
    fn test_repl() {
        let tests = vec![
            (
                vec!["(fn (name) { return \"hello \" + name; })(\"world\");"],
                Ok(Object::Str("hello world".to_string())),
            ),
            (vec!["let x = 1;", "x + 1"], Ok(Object::Int(2))),
            (
                vec![
                    "let f = fn(a, b) { return a + b; };",
                    "let c = f(1, 2);",
                    "c",
                ],
                Ok(Object::Int(3)),
            ),
            (
                vec!["abc\"def\""],
                Err(EvalError::InvalidLiteral("abc\"def\"".to_string())),
            ),
            (
                vec!["let x = 1;", "if (x == 1) { \"a\" } else { \"b\" }"],
                Ok(Object::Str("a".to_string())),
            ),
            (
                vec!["let x = 1;", "if (x != 1) { \"a\" } else { \"b\" }"],
                Ok(Object::Str("b".to_string())),
            ),
            (
                vec![
                    "let x = 1;",
                    "fn f(a) { return a + x; }",
                    "fn g() { let x = f(1); return f(1); }",
                    "g()",
                ],
                Ok(Object::Int(2)),
            ),
            (
                vec!["-\"hello\""],
                Err(EvalError::PrefixOp(
                    Operator::Neg,
                    Object::Str("hello".to_string()),
                )),
            ),
            (
                vec![
                    "let f = fn(a, b) { a + b };",
                    "let x = 1;",
                    "let y = 2;",
                    "f(f(x + y, x * y), f(x - y, x / y))",
                ],
                Ok(Object::Int(4)),
            ),
            (
                vec!["let x = 42;", "if (x) { 1 } else { 2 }"],
                Err(EvalError::NotBoolean(Object::Int(42))),
            ),
            (
                vec![
                    "fn f(a, b) { a + b }",
                    "fn g(a) { return fn(b) { return f(a, b); }; }",
                    "g(1)(2)",
                ],
                Ok(Object::Int(3)),
            ),
            (
                vec![
                    "fn f(a,b) { a + b }",
                    "fn g(a) { fn(b) { f(a,b) } }",
                    "g(40)(2)",
                ],
                Ok(Object::Int(42)),
            ),
            (
                vec![
                    "let f = fn (x) { fn(y) { x + y } };",
                    "let g = f(40);",
                    "let apply = fn(x, F) { F(x) };",
                    "apply(2, g)",
                ],
                Ok(Object::Int(42)),
            ),
            (
                vec![
                    "let f = fn(a) { fn(b) { fn(c) { a+b+c } } };",
                    "f(100)(20)(3)",
                ],
                Ok(Object::Int(123)),
            ),
        ];

        for (src, obj) in tests {
            let res = run_eval(&src);
            assert_eq!(res, obj, "{:#?}", src);
        }
    }

    fn run_eval(src: &[&str]) -> Result<Object, EvalError> {
        let mut ctx = Context::default();
        let mut res = Ok(Object::Unit);
        for line in src.iter() {
            let buf = Buffer::from_string(line);
            let tokens = tokenize(&buf).unwrap();
            let buf = Buffer::new(tokens);
            let tree = parse(&buf).unwrap();
            res = eval(&tree, &mut ctx);
        }
        res
    }

    #[test]
    fn test_file() {
        let base = Path::new("examples");
        let tests = vec!["fib.yes", "rec.yes", "curry.yes"];

        for test in tests {
            let result = run_file(base.join(test).as_path(), &mut Context::default()).unwrap();

            assert_eq!(result, Object::Bool(true));
        }
    }
}
