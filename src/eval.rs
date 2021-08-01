use crate::parser::{Expression, Operator, Statement};
use crate::error::EvalError;
use crate::token::*;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Object {
    Unit,
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    //Func,
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Unit => write!(f, "()"),
            Object::Int(x) => write!(f, "{}", x),
            Object::Float(x) => write!(f, "{}", x),
            Object::Bool(true) => write!(f, "true"),
            Object::Bool(false) => write!(f, "false"),
            Object::Str(x) => write!(f, "{}", x)
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

#[derive(Default)]
pub(crate) struct Context<'a> {
    parent: Option<&'a Context<'a>>,
    objects: HashMap<String, Object>,
}

impl<'a> Context<'a> {
    fn empty() -> Self {
        Self {
            parent: None,
            objects: Default::default(),
        }
    }

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
    }

    fn has(&self, name: &str) -> bool {
        self.objects.contains_key(name)
    }
}

pub(crate) fn eval(_statements: Vec<Statement>, _ctx: &mut Context) -> Result<Object, EvalError> {
    Ok(Object::Unit) // TODO
}

fn eval_stmt(_stmt: &Statement, ctx: Context) -> Result<Object, EvalError> {
    Ok(Object::Unit) // TODO
}

fn eval_expr(expr: &Expression, ctx: Context) -> Result<Object, EvalError> {
    match expr {
        Expression::Lit(str) if TRUE == str => Ok(Object::Bool(true)),
        Expression::Lit(str) if FALSE == str => Ok(Object::Bool(false)),
        Expression::Lit(str) if is_quoted(str) => Ok(Object::Str(unquote(str))),
        Expression::Lit(int) if is_integer(int) => Ok(Object::Int(int.parse::<i64>()?)),
        Expression::Lit(fpn) if is_float(fpn) => Ok(Object::Float(fpn.parse::<f64>()?)),
        Expression::Unit => Ok(Object::Unit),

        Expression::Var(name) if ctx.has(name) => Ok(ctx.get(name).cloned().unwrap()),
        Expression::Var(name) => Err(EvalError::NotFound(name.to_string())),
        _ => todo!()
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

fn eval_infix(op: Operator, lhs: Object, rhs: Object, ctx: Context) -> Result<Object, EvalError> {
    match (op, lhs, rhs) {
        (Operator::Add, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a + b)),
        (Operator::Sub, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a - b)),
        (Operator::Mul, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a * b)),
        (Operator::Div, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a / b)),
        (Operator::Mod, Object::Int(a), Object::Int(b)) => Ok(Object::Int(a % b)),

        (Operator::Add, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a + b)),
        (Operator::Sub, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a - b)),
        (Operator::Mul, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a * b)),
        (Operator::Div, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a / b)),
        (Operator::Mod, Object::Float(a), Object::Float(b)) => Ok(Object::Float(a % b)),

        (Operator::Add, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 + b)),
        (Operator::Sub, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 - b)),
        (Operator::Mul, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 * b)),
        (Operator::Div, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 / b)),
        (Operator::Mod, Object::Int(a), Object::Float(b)) => Ok(Object::Float(a as f64 % b)),

        (Operator::Add, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a + b as f64)),
        (Operator::Sub, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a - b as f64)),
        (Operator::Mul, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a * b as f64)),
        (Operator::Div, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a / b as f64)),
        (Operator::Mod, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a % b as f64)),

        (Operator::Add, Object::Str(a), Object::Str(b)) => Ok(Object::Str(a.clone() + &b)),

        (op, lhs, rhs) => Err(EvalError::InfixOp(op, lhs, rhs))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval() {
        let tests = vec![
            (Expression::Lit(TRUE.to_string()), vec![], Object::Bool(true)),
            (Expression::Lit(FALSE.to_string()), vec![], Object::Bool(false)),
            (Expression::Lit("123".to_string()), vec![], Object::Int(123)),
            (Expression::Lit("1.23".to_string()), vec![], Object::Float(1.23)),
            (Expression::Lit("\"hello\"".to_string()), vec![], Object::Str("hello".to_string())),
            (Expression::Unit, vec![], Object::Unit),
            (Expression::Var("x".to_string()), vec![("x".to_string(), Object::Int(42))], Object::Int(42)),
        ];

        for (expr, map, obj) in tests {
            let ctx = map.into_iter().fold(
                Context::default(),
                |mut ctx, (name, object)| {
                    ctx.put(name, object);
                    ctx
                });
            let res = eval_expr(&expr, ctx).unwrap();

            assert_eq!(res, obj, "expr={:?} obj={:?} res={:?}", expr, obj, res);
        }
    }
}