//! Compiles the AST into a tree of closures once, so that running a program does not
//! dispatch on AST nodes, literals are converted to objects only once, and calls of
//! builtins with literal arguments (e.g. `selector("transfer(address,uint256)")`)
//! are evaluated only once.
//!
//! The semantics are exactly those of the tree-walking interpreter in `eval.rs`
//! (tests run both and compare the results).

use std::cell::OnceCell;
use std::rc::Rc;

use crate::builtin::Builtin;
use crate::error::{Error, EvalError};
use crate::eval::{
    CallDepthGuard, Context, Flow, Function, Limits, Object, at, eval, eval_eq, eval_index,
    eval_infix, eval_prefix, eval_slice, get_field, lookup, run_collecting,
};
use crate::parser::{Expression, Operator, Statement};

type Expr = Box<dyn Fn(&Context) -> Result<Object, EvalError>>;
type Stmt = Box<dyn Fn(&Context) -> Result<Flow, EvalError>>;

/// A compiled block of statements.
pub(crate) type Block = Vec<Stmt>;

/// A compiled program: parse once, compile once, run many times.
pub struct Program {
    code: Block,
    ast: Vec<Statement>,
}

// prints the syntax tree
impl std::fmt::Debug for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.ast.fmt(f)
    }
}

impl Program {
    /// Parses and compiles the source code.
    pub fn compile(code: &str) -> Result<Self, Error> {
        Ok(Program::new(&crate::parse_source(code)?))
    }

    pub(crate) fn new(statements: &[Statement]) -> Self {
        Program {
            code: compile_block(statements),
            ast: statements.to_vec(),
        }
    }

    /// Runs the program with the default `Limits`. Bindings made by the program
    /// (`let`, `fn`) are put into `ctx`, use `ctx.fork()` to keep `ctx` as it is.
    pub fn run(&self, ctx: &Context) -> Result<Object, EvalError> {
        self.run_with(ctx, Limits::default())
    }

    /// Runs the program in a context made for this run only (e.g. `ctx.fork()` with the
    /// input bound in it): the context is released after the run, so that nothing but the
    /// result is kept. Use this to run a program on many inputs, e.g. on each item of a stream.
    pub fn run_once(&self, ctx: Context, limits: Limits) -> Result<Object, EvalError> {
        run_collecting(&ctx, limits, false, || self.run_code(&ctx))
    }

    /// Runs the program with the tree-walking interpreter, which the compiled closures
    /// must agree with: for differential testing (fuzzing) only.
    #[doc(hidden)]
    pub fn run_tree_walking(&self, ctx: Context, limits: Limits) -> Result<Object, EvalError> {
        run_collecting(&ctx, limits, false, || eval(&self.ast, &ctx))
    }

    pub fn run_with(&self, ctx: &Context, limits: Limits) -> Result<Object, EvalError> {
        run_collecting(ctx, limits, true, || self.run_code(ctx))
    }

    fn run_code(&self, ctx: &Context) -> Result<Object, EvalError> {
        match run_block(&self.code, ctx)? {
            Flow::Next(obj) | Flow::Return(obj) => Ok(obj),
        }
    }
}

fn run_block(block: &[Stmt], ctx: &Context) -> Result<Flow, EvalError> {
    crate::grow(|| run_statements(block, ctx))
}

fn run_statements(block: &[Stmt], ctx: &Context) -> Result<Flow, EvalError> {
    let mut result = Object::Null;
    for stmt in block {
        match stmt(ctx)? {
            Flow::Next(obj) => result = obj,
            ret => return Ok(ret),
        }
    }
    Ok(Flow::Next(result))
}

fn compile_block(statements: &[Statement]) -> Block {
    crate::grow(|| statements.iter().map(compile_stmt).collect())
}

fn compile_stmt(stmt: &Statement) -> Stmt {
    match stmt {
        Statement::Let(name, expr) => {
            let name = name.clone();
            let expr = compile_expr(expr);
            Box::new(move |ctx| {
                let obj = expr(ctx)?;
                ctx.put(name.clone(), obj);
                Ok(Flow::Next(Object::Null))
            })
        }

        Statement::Ret(expr) => {
            let expr = compile_expr(expr);
            Box::new(move |ctx| Ok(Flow::Return(expr(ctx)?)))
        }

        Statement::If(cond, if_clause, else_clause) => {
            let pos = cond.position();
            let cond = compile_expr(cond);
            let if_clause = compile_block(if_clause);
            let else_clause = compile_block(else_clause);
            // an empty branch needs no scope of its own
            let run = |block: &Block, ctx: &Context| {
                if block.is_empty() {
                    Ok(Flow::Next(Object::Null))
                } else {
                    run_block(block, &ctx.fork())
                }
            };
            Box::new(move |ctx| match cond(ctx)? {
                Object::Bool(true) => run(&if_clause, ctx),
                Object::Bool(false) => run(&else_clause, ctx),
                obj => Err(at(EvalError::NotBoolean(obj), pos)),
            })
        }

        Statement::Fn(name, params, body) => {
            let name = name.clone();
            let make = compile_fn(params, body);
            // the function is put into the same scope it captures, so it can call itself
            Box::new(move |ctx| {
                ctx.put(name.clone(), make(ctx));
                Ok(Flow::Next(Object::Null))
            })
        }

        Statement::Expr(expr) => {
            let expr = compile_expr(expr);
            Box::new(move |ctx| Ok(Flow::Next(expr(ctx)?)))
        }
    }
}

// the body is compiled once, each evaluation only captures the current scope
fn compile_fn(params: &[String], body: &[Statement]) -> impl Fn(&Context) -> Object + use<> {
    let params = Rc::new(params.to_vec());
    let ast = Rc::new(body.to_vec());
    let code = Rc::new(compile_block(body));
    move |ctx| {
        Object::Func(Rc::new(Function {
            params: params.clone(),
            body: ast.clone(),
            code: Some(code.clone()),
            env: ctx.clone(),
        }))
    }
}

fn literal(expr: &Expression) -> Option<Object> {
    match expr {
        Expression::Lit(lit) => Some(lit.into()),
        _ => None,
    }
}

fn compile_exprs(exprs: &[Expression]) -> Vec<Expr> {
    exprs.iter().map(compile_expr).collect()
}

fn run_exprs(exprs: &[Expr], ctx: &Context) -> Result<Vec<Object>, EvalError> {
    exprs.iter().map(|expr| expr(ctx)).collect()
}

fn compile_expr(expr: &Expression) -> Expr {
    crate::grow(|| compile_nested_expr(expr))
}

// the stack can grow at every node that other nodes are nested in (see 'At', 'List', 'Record'
// and 'run_block'), so that running deeply nested code does not overflow it
fn compile_nested_expr(expr: &Expression) -> Expr {
    match expr {
        Expression::At(pos, expr) => {
            let pos = *pos;
            let expr = compile_expr(expr);
            Box::new(move |ctx| crate::grow(|| expr(ctx)).map_err(|e| e.at(pos)))
        }

        Expression::Lit(lit) => {
            let obj: Object = lit.into();
            Box::new(move |_| Ok(obj.clone()))
        }

        Expression::Var(name) => {
            let name = name.clone();
            Box::new(move |ctx| lookup(ctx, &name))
        }

        Expression::Prefix(op, rhs) => {
            let op = op.clone();
            let rhs = compile_expr(rhs);
            Box::new(move |ctx| eval_prefix(op.clone(), rhs(ctx)?))
        }

        Expression::Infix(lhs, op @ (Operator::And | Operator::Or), rhs) => {
            let is_and = *op == Operator::And;
            let lhs = compile_expr(lhs);
            let rhs = compile_expr(rhs);
            // short-circuit: 'rhs' is evaluated only when 'lhs' does not decide the result
            Box::new(move |ctx| match lhs(ctx)? {
                Object::Bool(b) if b != is_and => Ok(Object::Bool(b)),
                Object::Bool(_) => match rhs(ctx)? {
                    Object::Bool(b) => Ok(Object::Bool(b)),
                    obj => Err(EvalError::NotBoolean(obj)),
                },
                obj => Err(EvalError::NotBoolean(obj)),
            })
        }

        // comparison with a literal: the literal is compared in place, not copied
        Expression::Infix(lhs, op @ (Operator::Eq | Operator::Ne), rhs)
            if literal(rhs).is_some() =>
        {
            let op = op.clone();
            let lhs = compile_expr(lhs);
            let rhs = literal(rhs).unwrap();
            Box::new(move |ctx| Ok(eval_eq(&op, &lhs(ctx)?, &rhs)))
        }

        Expression::Infix(lhs, op, rhs) => {
            let op = op.clone();
            let lhs = compile_expr(lhs);
            let rhs = compile_expr(rhs);
            Box::new(move |ctx| eval_infix(op.clone(), lhs(ctx)?, rhs(ctx)?))
        }

        Expression::Fn(params, body) => {
            let make = compile_fn(params, body);
            Box::new(move |ctx| Ok(make(ctx)))
        }

        // a call of a builtin with literal arguments always gives the same result, so it
        // is evaluated once, unless the name is bound (the builtin is shadowed)
        Expression::Apply(callee, args)
            if matches!(callee.inner(), Expression::Var(name) if Builtin::from_name(name).is_some())
                && args.iter().all(|arg| literal(arg).is_some()) =>
        {
            let name = match callee.inner() {
                Expression::Var(name) => name.clone(),
                _ => unreachable!(),
            };
            let builtin = Builtin::from_name(&name).unwrap();
            let literals: Vec<Object> = args.iter().filter_map(literal).collect();
            let args = compile_exprs(args);
            let cached: OnceCell<Object> = OnceCell::new();
            Box::new(move |ctx| match ctx.get(&name) {
                Some(obj) => call(obj, &args, ctx),
                None => match cached.get() {
                    Some(obj) => Ok(obj.clone()),
                    None => {
                        let obj = builtin.call(literals.clone(), &call_value)?;
                        Ok(cached.get_or_init(|| obj).clone())
                    }
                },
            })
        }

        Expression::Apply(callee, args) => {
            let callee = compile_expr(callee);
            let args = compile_exprs(args);
            Box::new(move |ctx| call(callee(ctx)?, &args, ctx))
        }

        Expression::Index(target, index) => {
            let target = compile_expr(target);
            let index = compile_expr(index);
            Box::new(move |ctx| eval_index(target(ctx)?, index(ctx)?))
        }

        Expression::Slice(target, start, end) => {
            let target = compile_expr(target);
            let start = start.as_ref().map(|expr| compile_expr(expr));
            let end = end.as_ref().map(|expr| compile_expr(expr));
            Box::new(move |ctx| {
                let target = target(ctx)?;
                let bound = |bound: &Option<Expr>| match bound {
                    None => Ok(None),
                    Some(expr) => match expr(ctx)? {
                        Object::Int(i) => Ok(Some(i)),
                        obj => Err(EvalError::Index(target.clone(), obj)),
                    },
                };
                let start = bound(&start)?;
                let end = bound(&end)?;
                eval_slice(target, start, end)
            })
        }

        Expression::List(items) => {
            let items = compile_exprs(items);
            Box::new(move |ctx| crate::grow(|| Ok(Object::List(Rc::new(run_exprs(&items, ctx)?)))))
        }

        Expression::Record(fields) => {
            let fields: Vec<(String, Expr)> = fields
                .iter()
                .map(|(name, expr)| (name.clone(), compile_expr(expr)))
                .collect();
            Box::new(move |ctx| {
                crate::grow(|| {
                    let fields = fields
                        .iter()
                        .map(|(name, expr)| Ok((name.clone(), expr(ctx)?)))
                        .collect::<Result<_, EvalError>>()?;
                    Ok(Object::Record(Rc::new(fields)))
                })
            })
        }

        Expression::Field(target, name) => {
            let target = compile_expr(target);
            let name = name.clone();
            Box::new(move |ctx| match target(ctx)? {
                Object::Record(fields) => Ok(get_field(&fields, &name)),
                obj => Err(EvalError::Field(obj, name.clone())),
            })
        }
    }
}

fn call(callee: Object, args: &[Expr], ctx: &Context) -> Result<Object, EvalError> {
    match callee {
        Object::Func(f) => {
            if f.params.len() != args.len() {
                return Err(EvalError::ApplyArgsCount(f.params.len(), args.len()));
            }
            // arguments are evaluated in the caller's scope, the body - in the defining one
            let fn_ctx = f.env.fork();
            for (name, arg) in f.params.iter().zip(args.iter()) {
                fn_ctx.put(name.clone(), arg(ctx)?);
            }
            let _guard = CallDepthGuard::enter()?;
            run_fn(&f, &fn_ctx)
        }
        Object::Builtin(b) => b.call(run_exprs(args, ctx)?, &call_value),
        obj => Err(EvalError::NotFunction(obj)),
    }
}

// calls a function value with evaluated arguments (e.g. from a builtin like 'map')
fn call_value(f: &Object, args: Vec<Object>) -> Result<Object, EvalError> {
    match f {
        Object::Func(f) => {
            if f.params.len() != args.len() {
                return Err(EvalError::ApplyArgsCount(f.params.len(), args.len()));
            }
            let fn_ctx = f.env.fork();
            for (name, obj) in f.params.iter().zip(args) {
                fn_ctx.put(name.clone(), obj);
            }
            let _guard = CallDepthGuard::enter()?;
            run_fn(f, &fn_ctx)
        }
        Object::Builtin(b) => b.call(args, &call_value),
        obj => Err(EvalError::NotFunction(obj.clone())),
    }
}

fn run_fn(f: &Function, fn_ctx: &Context) -> Result<Object, EvalError> {
    match &f.code {
        Some(code) => match run_block(code, fn_ctx)? {
            Flow::Next(obj) | Flow::Return(obj) => Ok(obj),
        },
        // a function created by the tree-walking interpreter
        None => eval(&f.body, fn_ctx),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn program(src: &str) -> Program {
        Program::compile(src).unwrap()
    }

    #[test]
    fn test_cached_builtin_call() {
        let p = program("selector(\"transfer(address,uint256)\")");
        let selector = Ok(Object::Bytes(vec![0xa9, 0x05, 0x9c, 0xbb].into()));
        assert_eq!(p.run(&Context::default()), selector);
        // the cached result is used only while the builtin is not shadowed
        let ctx = Context::default();
        ctx.put("selector".to_string(), Object::Int(1.into()));
        assert_eq!(
            p.run(&ctx).map_err(EvalError::without_position),
            Err(EvalError::NotFunction(Object::Int(1.into())))
        );
        assert_eq!(p.run(&Context::default()), selector);

        let p = program("fn f(selector) { selector(\"a\") } f(len)");
        assert_eq!(p.run(&Context::default()), Ok(Object::Int(1.into())));

        // errors are not cached
        let p = program("int(\"x\")");
        for _ in 0..2 {
            assert_eq!(
                p.run(&Context::default())
                    .map_err(EvalError::without_position),
                Err(EvalError::Argument("int", Object::Str("x".to_string())))
            );
        }
    }

    #[test]
    fn test_program_reuse() {
        // a program keeps no state between runs
        let p = program("let x = n * 2; fn f() { x } f()");
        for n in 0..3 {
            let ctx = Context::default();
            ctx.put("n".to_string(), Object::Int(n.into()));
            assert_eq!(p.run(&ctx), Ok(Object::Int((n * 2).into())));
            assert_eq!(ctx.get("n"), Some(Object::Int(n.into())));
        }
    }
}
