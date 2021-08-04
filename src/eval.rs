use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::token::*;
use crate::error::EvalError;
use crate::parser::{Expression, Operator, Statement};

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
            Object::Str(x) => write!(f, "{}", x),
            Object::Func(_, _, _) => write!(f, "<function>"),
        }
    }
}

fn is_quoted(s: &str) -> bool {
    s.starts_with("\"") && s.ends_with("\"")
}

fn unquote(s: &str) -> String {
    if is_quoted(s) {
        let n = s.len();
        s.chars().skip(1).take(n-2).collect()
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
    parent: Option<&'a Context<'a>>,
    objects: HashMap<String, Object>,
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
        self.objects.get(name)
            .or_else(|| match self.parent {
                Some(p) => p.get(name),
                None => None
            })
    }

    fn has(&self, name: &str) -> bool {
        self.objects.contains_key(name) || self.parent.map(|p| p.has(name)).unwrap_or_default()
    }

    fn snapshot(&self) -> Snapshot {
        if self.objects.is_empty() {
            self.parent
                .map(|p| p.snapshot())
                .unwrap_or_default()
        } else {
            let mut result = Snapshot::default();
            for (k, v) in self.objects.iter() {
                result.objects.insert(k.clone(), v.clone());
            }
            result
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

pub(crate) fn eval(statements: Vec<Statement>, ctx: &mut Context) -> Result<Object, EvalError> {
    let mut result = Object::Unit;
    for stmt in statements {
        result = eval_stmt(&stmt, ctx)?;
    }
    Ok(result)
}

fn eval_stmt(stmt: &Statement, ctx: &mut Context) -> Result<Object, EvalError> {
    match stmt {
        Statement::Let(name, expr) => {
            let obj = eval_expr(expr, ctx.fork())?;
            ctx.put(name.clone(), obj.clone());
            Ok(obj)
        },

        Statement::Ret(expr) =>
            eval_expr(expr, ctx.fork()),

        Statement::If(expr, if_clause, else_clause) => {
            let cond = eval_expr(expr, ctx.fork())?;
            match cond {
                Object::Bool(true) => eval_expr(if_clause, ctx.fork()),
                Object::Bool(false) => eval_expr(else_clause, ctx.fork()),
                obj => Err(EvalError::NotBoolean(obj))
            }
        },

        Statement::Fn(name, args, body) => {
            let f = Object::Func(args.to_owned(), body.to_owned(), ctx.snapshot());
            ctx.put(name.clone(), f.clone());
            Ok(f)
        }

        Statement::Call(lhs, rhs) =>
            eval_fn_expr(lhs, rhs, ctx.fork()),

        Statement::Expr(expr) =>
            eval_expr(expr, ctx.fork()),
    }
}

fn eval_expr(expr: &Expression, ctx: Context) -> Result<Object, EvalError> {
    match expr {
        Expression::Lit(str) if TRUE == str => Ok(Object::Bool(true)),
        Expression::Lit(str) if FALSE == str => Ok(Object::Bool(false)),
        Expression::Lit(str) if is_quoted(str) => Ok(Object::Str(unquote(str))),
        Expression::Lit(int) if is_integer(int) => Ok(Object::Int(int.parse::<i64>()?)),
        Expression::Lit(fpn) if is_float(fpn) => Ok(Object::Float(fpn.parse::<f64>()?)),
        Expression::Lit(str) => Err(EvalError::InvalidLiteral(str.to_string())),
        Expression::Unit => Ok(Object::Unit),

        Expression::Var(name) if ctx.has(name) => Ok(ctx.get(name).cloned().unwrap()),
        Expression::Var(name) => Err(EvalError::NotFound(name.to_string())),

        Expression::Prefix(op, rhs) =>
            eval_prefix(op.clone(), eval_expr(rhs, ctx.fork())?),

        Expression::Infix(lhs, op, rhs) =>
            eval_infix(
                op.clone(),
                eval_expr(lhs, ctx.fork())?,
                eval_expr(rhs, ctx.fork())?),

        Expression::If(expr, if_clause, else_clause) => {
            let cond = eval_expr(expr, ctx.fork())?;
            match cond {
                Object::Bool(true) => eval_expr(if_clause, ctx.fork()),
                Object::Bool(false) => eval_expr(else_clause, ctx.fork()),
                obj => Err(EvalError::NotBoolean(obj))
            }
        }

        Expression::Fn(args, body) => {
            Ok(Object::Func(args.to_owned(), body.to_owned(), ctx.snapshot()))
        }

        Expression::Apply(lhs, rhs) => {
            eval_fn_expr(lhs, rhs, ctx.fork())
        }
    }
}

fn eval_prefix(op: Operator, obj: Object) -> Result<Object, EvalError> {
    match (op, obj) {
        (Operator::Neg, Object::Int(x)) => Ok(Object::Int(-x)),
        (Operator::Neg, Object::Float(x)) => Ok(Object::Float(-x)),
        (Operator::Not, Object::Bool(b)) => Ok(Object::Bool(!b)),
        (op, obj) => Err(EvalError::PrefixOp(op, obj))
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

        (Operator::Add, Object::Str(a), Object::Str(b)) => Ok(Object::Str(a.clone() + &b)),
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

        (op, lhs, rhs) => Err(EvalError::InfixOp(op, lhs, rhs))
    }
}

fn eval_fn_expr(lhs: &Expression, rhs: &[Expression], ctx: Context) -> Result<Object, EvalError> {
    match lhs {
        Expression::Var(name) if ctx.has(name) => {
            if let Object::Func(args, body, snap) = ctx.get(name).unwrap() {
                eval_fn(args, body, rhs, ctx.extend(snap))
            } else {
                Err(EvalError::NotFunction(name.clone()))
            }
        }
        Expression::Var(name) => {
            Err(EvalError::NotFound(name.to_string()))
        },
        Expression::Fn(args, body) => {
            eval_fn(&args, &body, rhs, ctx.fork())
        },
        Expression::Apply(expr, args) => {
            if let Object::Func(args, body, snap) = eval_fn_expr(expr, args, ctx.fork())? {
                eval_fn(&args, &body, rhs, ctx.extend(&snap))
            } else {
                Err(EvalError::Apply(*expr.to_owned()))
            }
        }
        _ => Err(EvalError::Apply(lhs.clone()))
    }
}

fn eval_fn(args: &[String], body: &[Statement], rhs: &[Expression], ctx: Context) -> Result<Object, EvalError> {
    if args.len() != rhs.len() {
        return Err(EvalError::ApplyArgsCount(args.len(), rhs.len()));
    }

    let mut fn_ctx = ctx.fork();
    for (name, expr) in args.into_iter().zip(rhs.into_iter()) {
        let obj = eval_expr(expr, ctx.fork())?;
        fn_ctx.put(name.clone(), obj);
    }

    let mut result = Object::Unit;
    for stmt in body {
        result = eval_stmt(stmt, &mut fn_ctx)?;
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokenize;
    use crate::parser::parse;
    use crate::buffer::Buffer;
    use std::path::Path;
    use crate::{get_file, run_file};

    #[test]
    fn test_eval_expr() {
        let tests = vec![
            (Expression::Unit, vec![], Ok(Object::Unit)),
            (Expression::Lit(TRUE.to_string()), vec![], Ok(Object::Bool(true))),
            (Expression::Lit(FALSE.to_string()), vec![], Ok(Object::Bool(false))),
            (Expression::Lit("123".to_string()), vec![], Ok(Object::Int(123))),
            (Expression::Lit("1.23".to_string()), vec![], Ok(Object::Float(1.23))),
            (Expression::Lit("\"hello\"".to_string()), vec![], Ok(Object::Str("hello".to_string()))),
            (Expression::Var("x".to_string()), vec![("x".to_string(), Object::Int(42))], Ok(Object::Int(42))),
        ];

        for (expr, map, obj) in tests {
            let ctx = map.into_iter().fold(
                Context::default(),
                |mut ctx, (name, object)| {
                    ctx.put(name, object);
                    ctx
                });
            let res = eval_expr(&expr, ctx);

            assert_eq!(res, obj, "expr={:?} obj={:?} res={:?}", expr, obj, res);
        }
    }

    #[test]
    fn test_repl() {
        let tests = vec![
            (
                vec![
                    "(fn (name) { return \"hello \" + name; })(\"world\");",
                ],
                Ok(Object::Str("hello world".to_string())),
            ),
            (
                vec![
                    "let x = 1;",
                    "x + 1"
                ],
                Ok(Object::Int(2)),
            ),
            (
                vec![
                    "let f = fn(a, b) { return a + b; };",
                    "let c = f(1, 2);",
                    "c"
                ],
                Ok(Object::Int(3)),
            ),
            (
                vec![
                    "abc\"def\"",
                ],
                Err(EvalError::InvalidLiteral("abc\"def\"".to_string())),
            ),
            (
                vec![
                    "let x = 1;",
                    "if (x == 1) { \"a\" } else { \"b\" }",
                ],
                Ok(Object::Str("a".to_string())),
            ),
            (
                vec![
                    "let x = 1;",
                    "if (x != 1) { \"a\" } else { \"b\" }",
                ],
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
                vec![
                    "-\"hello\"",
                ],
                Err(EvalError::PrefixOp(Operator::Neg, Object::Str("hello".to_string())))
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
                vec![
                    "let x = 42;",
                    "if (x) { 1 } else { 2 }",
                ],
                Err(EvalError::NotBoolean(Object::Int(42))),
            ),
            (
                vec![
                    "fn f(a, b) { a + b }",
                    "fn g(a) { return fn(b) { return f(a, b); }; }",
                    "g(1)(2)",
                ],
                Ok(Object::Int(3))
            ),
            (
                vec![
                    "fn f(a,b) { a + b }",
                    "fn g(a) { fn(b) { f(a,b) } }",
                    "g(40)(2)",
                ],
                Ok(Object::Int(42)),
            )
        ];

        for (src, obj) in tests {
            let mut ctx = Context::default();

            let mut res = Ok(Object::Unit);
            for line in src.iter() {
                let buf = Buffer::from_string(line);
                let tokens = tokenize(&buf).unwrap();
                let buf = Buffer::new(tokens);
                let tree = parse(&buf).unwrap();
                res = eval(tree, &mut ctx);
            }

            assert_eq!(res, obj, "{:#?}", src);
        }
    }

    #[test]
    fn test_file() {
        let base = Path::new("examples");
        let tests = vec![
            "fib.yes",
            "rec.yes",
            "curry.yes",
        ];

        for test in tests {
            let code = get_file(base.join(test).as_path()).unwrap();
            let result = run_file(&code).unwrap();

            assert_eq!(result, Object::Bool(true));
        }
    }
}
