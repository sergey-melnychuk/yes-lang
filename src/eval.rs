use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};

use crate::builtin::Builtin;
use crate::compile::Block;
use crate::error::{Error, EvalError};
use crate::json::IntoObject;
use crate::parser::{Expression, Literal, Operator, Statement};

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Null,
    Int(BigInt),
    Float(f64),
    Bool(bool),
    Str(String),
    Bytes(Vec<u8>),
    List(Rc<Vec<Object>>),
    Record(Rc<BTreeMap<String, Object>>),
    Func(Rc<Function>),
    Builtin(Builtin),
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Int(x) => write!(f, "{}", x),
            Object::Float(x) => write!(f, "{}", x),
            Object::Bool(true) => write!(f, "true"),
            Object::Bool(false) => write!(f, "false"),
            Object::Str(x) => write!(f, "\"{}\"", x),
            Object::Bytes(bytes) => {
                write!(f, "b\"")?;
                for b in bytes.iter() {
                    write!(f, "{:02x}", b)?;
                }
                write!(f, "\"")
            }
            Object::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Object::Record(fields) => {
                write!(f, "{{")?;
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, value)?;
                }
                write!(f, "}}")
            }
            Object::Func(_) => write!(f, "<function>"),
            Object::Builtin(b) => write!(f, "<builtin {}>", b.name()),
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &'static str {
        match self {
            Object::Null => "Null",
            Object::Int(_) => "Int",
            Object::Float(_) => "Float",
            Object::Bool(_) => "Bool",
            Object::Str(_) => "Str",
            Object::Bytes(_) => "Bytes",
            Object::List(_) => "List",
            Object::Record(_) => "Record",
            Object::Func(_) => "Func",
            Object::Builtin(_) => "Builtin",
        }
    }
}

impl From<&Literal> for Object {
    fn from(lit: &Literal) -> Self {
        match lit {
            Literal::Null => Object::Null,
            Literal::Int(x) => Object::Int(x.clone()),
            Literal::Float(x) => Object::Float(*x),
            Literal::Bool(x) => Object::Bool(*x),
            Literal::Str(x) => Object::Str(x.clone()),
            Literal::Bytes(x) => Object::Bytes(x.clone()),
        }
    }
}

/// A function defined in a program.
pub struct Function {
    pub(crate) params: Rc<Vec<String>>,
    pub(crate) body: Rc<Vec<Statement>>,
    // the body compiled into closures, when the function was created by compiled code
    pub(crate) code: Option<Rc<Block>>,
    // the scope the function was defined in (lexical scoping)
    pub(crate) env: Context,
}

// the defining scope is not printed: it may contain the function itself
impl Debug for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("params", &self.params)
            .finish()
    }
}

// functions are equal only when they are the same function
impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

#[derive(Default, Debug)]
struct Scope {
    objects: HashMap<String, Object>,
    parent: Option<Context>,
}

/// A handle to a scope: cloning it shares the scope, it does not copy it.
#[derive(Default, Clone)]
pub struct Context(Rc<RefCell<Scope>>);

impl Debug for Context {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.borrow().fmt(f)
    }
}

impl Context {
    /// A new scope inside this one: bindings of the new scope are not visible here.
    pub fn fork(&self) -> Self {
        Self(Rc::new(RefCell::new(Scope {
            objects: Default::default(),
            parent: Some(self.clone()),
        })))
    }

    /// Binds a value to a name: JSON text, a `serde_json::Value` or an object, e.g.
    /// `ctx.bind("tx", r#"{"to": "0x..."}"#)?` or `ctx.bind("tx", &value)?`.
    pub fn bind(&self, name: &str, value: impl IntoObject) -> Result<(), Error> {
        self.put(name.to_string(), value.into_object()?);
        Ok(())
    }

    pub(crate) fn put(&self, name: String, object: Object) {
        self.0.borrow_mut().objects.insert(name, object);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let scope = self.0.borrow();
        match scope.objects.get(name) {
            Some(object) => Some(object.clone()),
            None => scope.parent.as_ref().and_then(|p| p.get(name)),
        }
    }
}

/// Limits of a single run of a program.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Limits {
    /// Maximum depth of nested function calls. Each level takes ~2KB of stack in
    /// optimized builds (~15KB in debug builds), so deep calls need a thread with a
    /// big stack, see `with_stack`.
    pub max_depth: usize,
    /// Maximum number of function calls, `None` for no limit. Bounds the time a
    /// program can run: without loops, only calls can make it run long.
    pub max_calls: Option<u64>,
}

impl Default for Limits {
    /// Fits into the default stack of a thread (2MB): 1000 nested calls, any number of calls.
    fn default() -> Self {
        Limits {
            max_depth: 1_000,
            max_calls: None,
        }
    }
}

thread_local! {
    static LIMITS: Cell<Limits> = Cell::new(Limits::default());
    static CALL_DEPTH: Cell<usize> = const { Cell::new(0) };
    static CALLS: Cell<u64> = const { Cell::new(0) };
    // deduplicated, so that a predicate applied to many inputs warns only once
    static WARNINGS: RefCell<BTreeSet<String>> = const { RefCell::new(BTreeSet::new()) };
}

/// Runs `f` with `limits`, counting calls from zero.
pub(crate) fn with_limits<T>(limits: Limits, f: impl FnOnce() -> T) -> T {
    let saved = (
        LIMITS.with(|l| l.replace(limits)),
        CALLS.with(|c| c.replace(0)),
    );
    let result = f();
    LIMITS.with(|l| l.set(saved.0));
    CALLS.with(|c| c.set(saved.1));
    result
}

fn warn(message: String) {
    WARNINGS.with(|warnings| warnings.borrow_mut().insert(message));
}

/// Returns (and clears) the warnings collected during evaluation on this thread.
pub fn take_warnings() -> Vec<String> {
    WARNINGS
        .with(|warnings| std::mem::take(&mut *warnings.borrow_mut()))
        .into_iter()
        .collect()
}

/// Tracks the depth of nested calls and the number of calls against the limits.
pub(crate) struct CallDepthGuard;

impl CallDepthGuard {
    pub(crate) fn enter() -> Result<Self, EvalError> {
        let limits = LIMITS.with(|l| l.get());
        let calls = CALLS.with(|c| {
            c.set(c.get() + 1);
            c.get()
        });
        if limits.max_calls.is_some_and(|max| calls > max) {
            return Err(EvalError::CallLimit(limits.max_calls.unwrap()));
        }
        CALL_DEPTH.with(|depth| {
            if depth.get() >= limits.max_depth {
                return Err(EvalError::StackOverflow(limits.max_depth));
            }
            depth.set(depth.get() + 1);
            Ok(CallDepthGuard)
        })
    }
}

impl Drop for CallDepthGuard {
    fn drop(&mut self) {
        CALL_DEPTH.with(|depth| depth.set(depth.get() - 1));
    }
}

// result of executing a statement: either continue with the next one or return
pub(crate) enum Flow {
    Next(Object),
    Return(Object),
}

pub(crate) fn eval(statements: &[Statement], ctx: &Context) -> Result<Object, EvalError> {
    match exec_block(statements, ctx)? {
        Flow::Next(obj) | Flow::Return(obj) => Ok(obj),
    }
}

fn exec_block(statements: &[Statement], ctx: &Context) -> Result<Flow, EvalError> {
    let mut result = Object::Null;
    for stmt in statements {
        match exec_stmt(stmt, ctx)? {
            Flow::Next(obj) => result = obj,
            ret => return Ok(ret),
        }
    }
    Ok(Flow::Next(result))
}

fn exec_stmt(stmt: &Statement, ctx: &Context) -> Result<Flow, EvalError> {
    match stmt {
        Statement::Let(name, expr) => {
            let obj = eval_expr(expr, ctx)?;
            ctx.put(name.clone(), obj);
            Ok(Flow::Next(Object::Null))
        }

        Statement::Ret(expr) => Ok(Flow::Return(eval_expr(expr, ctx)?)),

        Statement::If(expr, if_clause, else_clause) => {
            let cond = eval_expr(expr, ctx)?;
            match cond {
                Object::Bool(true) => exec_block(if_clause, &ctx.fork()),
                Object::Bool(false) => exec_block(else_clause, &ctx.fork()),
                obj => Err(EvalError::NotBoolean(obj)),
            }
        }

        Statement::Fn(name, params, body) => {
            // the function is put into the same scope it captures, so it can call itself
            let f = make_fn(params, body, ctx);
            ctx.put(name.clone(), f);
            Ok(Flow::Next(Object::Null))
        }

        Statement::Expr(expr) => Ok(Flow::Next(eval_expr(expr, ctx)?)),
    }
}

fn make_fn(params: &[String], body: &[Statement], ctx: &Context) -> Object {
    Object::Func(Rc::new(Function {
        params: Rc::new(params.to_owned()),
        body: Rc::new(body.to_owned()),
        code: None,
        env: ctx.clone(),
    }))
}

/// A name refers to a bound value, or to a builtin when nothing is bound to it.
pub(crate) fn lookup(ctx: &Context, name: &str) -> Result<Object, EvalError> {
    ctx.get(name)
        .or_else(|| Builtin::from_name(name).map(Object::Builtin))
        .ok_or_else(|| EvalError::NotFound(name.to_string()))
}

fn eval_expr(expr: &Expression, ctx: &Context) -> Result<Object, EvalError> {
    match expr {
        Expression::Lit(lit) => Ok(lit.into()),

        Expression::Var(name) => lookup(ctx, name),

        Expression::Prefix(op, rhs) => eval_prefix(op.clone(), eval_expr(rhs, ctx)?),

        Expression::Infix(lhs, op @ (Operator::And | Operator::Or), rhs) => {
            // short-circuit: 'rhs' is evaluated only when 'lhs' does not decide the result
            match (op, eval_expr(lhs, ctx)?) {
                (Operator::And, Object::Bool(false)) => Ok(Object::Bool(false)),
                (Operator::Or, Object::Bool(true)) => Ok(Object::Bool(true)),
                (_, Object::Bool(_)) => match eval_expr(rhs, ctx)? {
                    Object::Bool(b) => Ok(Object::Bool(b)),
                    obj => Err(EvalError::NotBoolean(obj)),
                },
                (_, obj) => Err(EvalError::NotBoolean(obj)),
            }
        }

        Expression::Infix(lhs, op, rhs) => {
            eval_infix(op.clone(), eval_expr(lhs, ctx)?, eval_expr(rhs, ctx)?)
        }

        Expression::Fn(params, body) => Ok(make_fn(params, body, ctx)),

        Expression::Apply(lhs, args) => match eval_expr(lhs, ctx)? {
            Object::Func(f) => eval_fn(&f, args, ctx),
            Object::Builtin(b) => {
                let args = args
                    .iter()
                    .map(|arg| eval_expr(arg, ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                b.call(args)
            }
            obj => Err(EvalError::NotFunction(obj)),
        },

        Expression::Index(target, index) => {
            eval_index(eval_expr(target, ctx)?, eval_expr(index, ctx)?)
        }

        Expression::Slice(target, start, end) => {
            let target = eval_expr(target, ctx)?;
            let start = eval_bound(start, &target, ctx)?;
            let end = eval_bound(end, &target, ctx)?;
            eval_slice(target, start, end)
        }

        Expression::List(items) => {
            let items = items
                .iter()
                .map(|item| eval_expr(item, ctx))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Object::List(Rc::new(items)))
        }

        Expression::Field(target, name) => match eval_expr(target, ctx)? {
            Object::Record(fields) => Ok(get_field(&fields, name)),
            obj => Err(EvalError::Field(obj, name.clone())),
        },
    }
}

pub(crate) fn eval_prefix(op: Operator, obj: Object) -> Result<Object, EvalError> {
    match (op, obj) {
        (Operator::Neg, Object::Int(x)) => Ok(Object::Int(-x)),
        (Operator::Neg, Object::Float(x)) => Ok(Object::Float(-x)),
        (Operator::Not, Object::Bool(b)) => Ok(Object::Bool(!b)),
        (op, obj) => Err(EvalError::PrefixOp(op, obj)),
    }
}

fn eval_int_op(op: Operator, a: BigInt, b: BigInt) -> Result<Object, EvalError> {
    let result = match op {
        Operator::Div | Operator::Mod if b.is_zero() => return Err(EvalError::DivisionByZero),
        Operator::Add => a + b,
        Operator::Sub => a - b,
        Operator::Mul => a * b,
        Operator::Div => a / b,
        Operator::Mod => a % b,
        _ => unreachable!(),
    };
    Ok(Object::Int(result))
}

// never fails for BigInt: values beyond f64 range become +/-inf
fn to_f64(x: &BigInt) -> f64 {
    x.to_f64().unwrap_or(f64::NAN)
}

pub(crate) fn eval_infix(op: Operator, lhs: Object, rhs: Object) -> Result<Object, EvalError> {
    match (op, lhs, rhs) {
        (
            op @ (Operator::Add | Operator::Sub | Operator::Mul | Operator::Div | Operator::Mod),
            Object::Int(a),
            Object::Int(b),
        ) => eval_int_op(op, a, b),
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

        (Operator::Add, Object::Int(a), Object::Float(b)) => Ok(Object::Float(to_f64(&a) + b)),
        (Operator::Sub, Object::Int(a), Object::Float(b)) => Ok(Object::Float(to_f64(&a) - b)),
        (Operator::Mul, Object::Int(a), Object::Float(b)) => Ok(Object::Float(to_f64(&a) * b)),
        (Operator::Div, Object::Int(a), Object::Float(b)) => Ok(Object::Float(to_f64(&a) / b)),
        (Operator::Mod, Object::Int(a), Object::Float(b)) => Ok(Object::Float(to_f64(&a) % b)),
        (Operator::Eq, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(to_f64(&a) == b)),
        (Operator::Ne, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(to_f64(&a) != b)),
        (Operator::Lt, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(to_f64(&a) < b)),
        (Operator::Lte, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(to_f64(&a) <= b)),
        (Operator::Gt, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(to_f64(&a) > b)),
        (Operator::Gte, Object::Int(a), Object::Float(b)) => Ok(Object::Bool(to_f64(&a) >= b)),

        (Operator::Add, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a + to_f64(&b))),
        (Operator::Sub, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a - to_f64(&b))),
        (Operator::Mul, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a * to_f64(&b))),
        (Operator::Div, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a / to_f64(&b))),
        (Operator::Mod, Object::Float(a), Object::Int(b)) => Ok(Object::Float(a % to_f64(&b))),
        (Operator::Eq, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a == to_f64(&b))),
        (Operator::Ne, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a != to_f64(&b))),
        (Operator::Lt, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a < to_f64(&b))),
        (Operator::Lte, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a <= to_f64(&b))),
        (Operator::Gt, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a > to_f64(&b))),
        (Operator::Gte, Object::Float(a), Object::Int(b)) => Ok(Object::Bool(a >= to_f64(&b))),

        (Operator::Add, Object::Str(a), Object::Str(b)) => Ok(Object::Str(a + &b)),
        (Operator::Eq, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a == b)),
        (Operator::Ne, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a != b)),
        (Operator::Lt, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a < b)),
        (Operator::Lte, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a <= b)),
        (Operator::Gt, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a > b)),
        (Operator::Gte, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(a >= b)),

        (Operator::Eq, Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a == b)),
        (Operator::Ne, Object::Bool(a), Object::Bool(b)) => Ok(Object::Bool(a != b)),

        (Operator::Add, Object::Bytes(a), Object::Bytes(b)) => Ok(Object::Bytes([a, b].concat())),
        (Operator::Eq, Object::Bytes(a), Object::Bytes(b)) => Ok(Object::Bool(a == b)),
        (Operator::Ne, Object::Bytes(a), Object::Bytes(b)) => Ok(Object::Bool(a != b)),

        // any two values can be compared for equality, values of different types are never equal
        (op @ (Operator::Eq | Operator::Ne), lhs, rhs) => Ok(eval_eq(&op, &lhs, &rhs)),

        // membership: an item of a list, a substring, a byte substring, a field of a record
        (Operator::In, item, Object::List(items)) => {
            Ok(Object::Bool(items.iter().any(|x| equals(&item, x))))
        }
        (Operator::In, Object::Str(a), Object::Str(b)) => Ok(Object::Bool(b.contains(&a))),
        (Operator::In, Object::Bytes(a), Object::Bytes(b)) => Ok(Object::Bool(
            a.is_empty() || b.windows(a.len()).any(|w| w == a.as_slice()),
        )),
        (Operator::In, Object::Str(name), Object::Record(fields)) => {
            Ok(Object::Bool(fields.contains_key(&name)))
        }

        (op, lhs, rhs) => Err(EvalError::InfixOp(op, lhs, rhs)),
    }
}

/// '==' or '!=' of any two values, warns when values of different types are compared
pub(crate) fn eval_eq(op: &Operator, lhs: &Object, rhs: &Object) -> Object {
    let is_number = |x: &Object| matches!(x, Object::Int(_) | Object::Float(_));
    let comparable = same_type(lhs, rhs) || (is_number(lhs) && is_number(rhs));
    if !comparable && *lhs != Object::Null && *rhs != Object::Null {
        warn(format!(
            "Comparing {} with {} is always {}",
            lhs.type_name(),
            rhs.type_name(),
            *op == Operator::Ne
        ));
    }
    Object::Bool(equals(lhs, rhs) == (*op == Operator::Eq))
}

fn same_type(a: &Object, b: &Object) -> bool {
    std::mem::discriminant(a) == std::mem::discriminant(b)
}

// numbers are equal by value, other values are equal only to values of the same type
fn equals(a: &Object, b: &Object) -> bool {
    match (a, b) {
        (Object::Int(x), Object::Float(y)) | (Object::Float(y), Object::Int(x)) => to_f64(x) == *y,
        _ => same_type(a, b) && a == b,
    }
}

fn eval_fn(f: &Function, args: &[Expression], ctx: &Context) -> Result<Object, EvalError> {
    if f.params.len() != args.len() {
        return Err(EvalError::ApplyArgsCount(f.params.len(), args.len()));
    }

    // arguments are evaluated in the caller's scope, the body - in the defining one
    let fn_ctx = f.env.fork();
    for (name, expr) in f.params.iter().zip(args.iter()) {
        let obj = eval_expr(expr, ctx)?;
        fn_ctx.put(name.clone(), obj);
    }

    let _guard = CallDepthGuard::enter()?;
    eval(&f.body, &fn_ctx)
}

// a missing field is null
pub(crate) fn get_field(fields: &BTreeMap<String, Object>, name: &str) -> Object {
    fields.get(name).cloned().unwrap_or(Object::Null)
}

fn get_index(idx: &BigInt, len: usize) -> Result<usize, EvalError> {
    idx.to_usize()
        .filter(|i| *i < len)
        .ok_or_else(|| EvalError::IndexOutOfBounds(idx.clone(), len))
}

pub(crate) fn eval_index(target: Object, index: Object) -> Result<Object, EvalError> {
    match (target, index) {
        (Object::Bytes(b), Object::Int(i)) => Ok(Object::Int(b[get_index(&i, b.len())?].into())),
        (Object::Str(s), Object::Int(i)) => {
            let chars: Vec<char> = s.chars().collect();
            Ok(Object::Str(chars[get_index(&i, chars.len())?].to_string()))
        }
        (Object::List(items), Object::Int(i)) => Ok(items[get_index(&i, items.len())?].clone()),
        (Object::Record(fields), Object::Str(name)) => Ok(get_field(&fields, &name)),
        (target, index) => Err(EvalError::Index(target, index)),
    }
}

fn eval_bound(
    bound: &Option<Box<Expression>>,
    target: &Object,
    ctx: &Context,
) -> Result<Option<BigInt>, EvalError> {
    match bound {
        None => Ok(None),
        Some(expr) => match eval_expr(expr, ctx)? {
            Object::Int(i) => Ok(Some(i)),
            obj => Err(EvalError::Index(target.clone(), obj)),
        },
    }
}

// slice bounds are clamped to the length, so slicing never fails: b""[0..4] == b""
fn clamp_range(start: Option<BigInt>, end: Option<BigInt>, len: usize) -> (usize, usize) {
    let clamp = |i: BigInt| {
        if i.is_negative() {
            0
        } else {
            i.to_usize().unwrap_or(len).min(len)
        }
    };
    let start = start.map(clamp).unwrap_or(0);
    let end = end.map(clamp).unwrap_or(len);
    (start, end.max(start))
}

pub(crate) fn eval_slice(
    target: Object,
    start: Option<BigInt>,
    end: Option<BigInt>,
) -> Result<Object, EvalError> {
    match target {
        Object::Bytes(b) => {
            let (start, end) = clamp_range(start, end, b.len());
            Ok(Object::Bytes(b[start..end].to_vec()))
        }
        Object::Str(s) => {
            let chars: Vec<char> = s.chars().collect();
            let (start, end) = clamp_range(start, end, chars.len());
            Ok(Object::Str(chars[start..end].iter().collect()))
        }
        Object::List(items) => {
            let (start, end) = clamp_range(start, end, items.len());
            Ok(Object::List(Rc::new(items[start..end].to_vec())))
        }
        target => Err(EvalError::Index(target, Object::Null)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::buffer::Buffer;
    use crate::compile::Program;
    use crate::lexer::tokenize;
    use crate::parser::parse;
    use crate::tests::LIMITS;
    use crate::with_stack;

    fn int(x: i64) -> Object {
        Object::Int(x.into())
    }

    fn big(x: &str) -> Object {
        Object::Int(x.parse().unwrap())
    }

    fn string(s: &str) -> Object {
        Object::Str(s.to_string())
    }

    fn parse_line(line: &str) -> Vec<Statement> {
        let buf = Buffer::from_string(line);
        let tokens = tokenize(&buf).unwrap();
        let buf = Buffer::new(tokens);
        parse(&buf).unwrap()
    }

    // runs the lines with the tree-walking interpreter and, separately, compiled into
    // closures: both must give the same result
    fn run_eval_in(src: &[&str], ctx: &Context) -> Result<Object, EvalError> {
        let (tree_ctx, compiled_ctx) = (ctx.fork(), ctx.fork());
        let mut tree = Ok(Object::Null);
        let mut compiled = Ok(Object::Null);
        for line in src.iter() {
            let statements = parse_line(line);
            tree = with_limits(LIMITS, || eval(&statements, &tree_ctx));
            compiled = Program::new(&statements).run_with(&compiled_ctx, LIMITS);
        }
        // functions are compared by identity, so results are compared by their debug output
        assert_eq!(
            format!("{:?}", tree),
            format!("{:?}", compiled),
            "tree-walking and compiled results differ: {:?}",
            src
        );
        tree
    }

    fn run_eval(src: &[&str]) -> Result<Object, EvalError> {
        run_eval_in(src, &Context::default())
    }

    fn check(tests: Vec<(Vec<&str>, Result<Object, EvalError>)>) {
        for (src, obj) in tests {
            let res = run_eval(&src);
            assert_eq!(res, obj, "{:#?}", src);
        }
    }

    #[test]
    fn test_eval() {
        let tests = vec![
            (
                Expression::Lit(Literal::Bool(true)),
                vec![],
                Ok(Object::Bool(true)),
            ),
            (
                Expression::Lit(Literal::Bool(false)),
                vec![],
                Ok(Object::Bool(false)),
            ),
            (
                Expression::Lit(Literal::Int(123.into())),
                vec![],
                Ok(int(123)),
            ),
            (
                Expression::Lit(Literal::Float(1.23)),
                vec![],
                Ok(Object::Float(1.23)),
            ),
            (
                Expression::Lit(Literal::Str("hello".to_string())),
                vec![],
                Ok(string("hello")),
            ),
            (
                Expression::Lit(Literal::Bytes(vec![0xa9])),
                vec![],
                Ok(Object::Bytes(vec![0xa9])),
            ),
            (
                Expression::Var("x".to_string()),
                vec![("x".to_string(), int(42))],
                Ok(int(42)),
            ),
            (
                Expression::Var("x".to_string()),
                vec![],
                Err(EvalError::NotFound("x".to_string())),
            ),
        ];

        for (expr, map, obj) in tests {
            let ctx = Context::default();
            for (name, object) in map {
                ctx.put(name, object);
            }
            let res = eval_expr(&expr, &ctx);

            assert_eq!(res, obj, "expr={:?} obj={:?} res={:?}", expr, obj, res);
        }
    }

    #[test]
    fn test_repl() {
        check(vec![
            (
                vec!["(fn (name) { return \"hello \" + name; })(\"world\");"],
                Ok(string("hello world")),
            ),
            (vec!["let x = 1;", "x + 1"], Ok(int(2))),
            (
                vec![
                    "let f = fn(a, b) { return a + b; };",
                    "let c = f(1, 2);",
                    "c",
                ],
                Ok(int(3)),
            ),
            (
                vec!["let x = 1;", "if (x == 1) { \"a\" } else { \"b\" }"],
                Ok(string("a")),
            ),
            (
                vec!["let x = 1;", "if (x != 1) { \"a\" } else { \"b\" }"],
                Ok(string("b")),
            ),
            (
                vec![
                    "let x = 1;",
                    "fn f(a) { return a + x; }",
                    "fn g() { let x = f(1); return f(1); }",
                    "g()",
                ],
                Ok(int(2)),
            ),
            (
                vec!["-\"hello\""],
                Err(EvalError::PrefixOp(Operator::Neg, string("hello"))),
            ),
            (
                vec![
                    "let f = fn(a, b) { a + b };",
                    "let x = 1;",
                    "let y = 2;",
                    "f(f(x + y, x * y), f(x - y, x / y))",
                ],
                Ok(int(4)),
            ),
            (
                vec!["let x = 42;", "if (x) { 1 } else { 2 }"],
                Err(EvalError::NotBoolean(int(42))),
            ),
            (
                vec![
                    "fn f(a, b) { a + b }",
                    "fn g(a) { return fn(b) { return f(a, b); }; }",
                    "g(1)(2)",
                ],
                Ok(int(3)),
            ),
            (
                vec![
                    "fn f(a,b) { a + b }",
                    "fn g(a) { fn(b) { f(a,b) } }",
                    "g(40)(2)",
                ],
                Ok(int(42)),
            ),
            (
                vec![
                    "let f = fn (x) { fn(y) { x + y } };",
                    "let g = f(40);",
                    "let apply = fn(x, F) { F(x) };",
                    "apply(2, g)",
                ],
                Ok(int(42)),
            ),
            (
                vec![
                    "let f = fn(a) { fn(b) { fn(c) { a+b+c } } };",
                    "f(100)(20)(3)",
                ],
                Ok(int(123)),
            ),
            (vec!["1; 2"], Ok(int(2))),
            (
                vec!["fn f(a) { a }", "f(1, 2)"],
                Err(EvalError::ApplyArgsCount(1, 2)),
            ),
        ]);
    }

    #[test]
    fn test_lexical_scope() {
        check(vec![
            // callee must not see caller's bindings
            (
                vec!["fn f() { y } fn g() { let y = 5; return f(); } g()"],
                Err(EvalError::NotFound("y".to_string())),
            ),
            // arguments are evaluated in the caller's scope
            (
                vec!["let x = 1; fn f(a) { a } fn g() { let x = 2; f(x) } g()"],
                Ok(int(2)),
            ),
            // named functions see themselves and functions defined later in the same scope
            (
                vec![
                    "fn even(n) { if n == 0 { true } else { odd(n - 1) } }",
                    "fn odd(n) { if n == 0 { false } else { even(n - 1) } }",
                    "even(10) && odd(7)",
                ],
                Ok(Object::Bool(true)),
            ),
            // each closure keeps its own captured scope
            (
                vec!["fn make(x) { fn() { x } } let a = make(1); let b = make(2); a() + b()"],
                Ok(int(3)),
            ),
            // 'if' branches have their own scope
            (
                vec!["if true { let y = 1; } y"],
                Err(EvalError::NotFound("y".to_string())),
            ),
            (vec!["let y = 1; if true { let y = 2; } y"], Ok(int(1))),
        ]);
    }

    #[test]
    fn test_return() {
        check(vec![
            (vec!["fn f() { if true { return 1; } 2 } f()"], Ok(int(1))),
            (
                vec![
                    "fn f(n) { if n > 0 { if n > 10 { return \"big\"; } return \"small\"; } \"zero\" }",
                    "f(20) + f(5) + f(0)",
                ],
                Ok(string("bigsmallzero")),
            ),
            // 'return' leaves only the innermost function
            (vec!["fn f() { let g = fn() { return 1; }; g(); 2 } f()"], Ok(int(2))),
            (vec!["return 1; 2"], Ok(int(1))),
        ]);
    }

    #[test]
    fn test_short_circuit() {
        check(vec![
            (vec!["false && undefined()"], Ok(Object::Bool(false))),
            (vec!["true || undefined()"], Ok(Object::Bool(true))),
            (vec!["true && false"], Ok(Object::Bool(false))),
            (vec!["false || true"], Ok(Object::Bool(true))),
            (vec!["true || false && false"], Ok(Object::Bool(true))),
            (vec!["true && 1"], Err(EvalError::NotBoolean(int(1)))),
            (vec!["1 || true"], Err(EvalError::NotBoolean(int(1)))),
        ]);
    }

    #[test]
    fn test_equality() {
        check(vec![
            // values of different types are never equal
            (vec!["1 == \"1\""], Ok(Object::Bool(false))),
            (vec!["1 != \"1\""], Ok(Object::Bool(true))),
            (vec!["b\"01\" == \"01\""], Ok(Object::Bool(false))),
            (vec!["true == 1"], Ok(Object::Bool(false))),
            (vec!["fn f() {} f() == null"], Ok(Object::Bool(true))),
            (vec!["null == false"], Ok(Object::Bool(false))),
            (vec!["null != 0"], Ok(Object::Bool(true))),
            // but numbers are compared by value
            (vec!["1 == 1.0"], Ok(Object::Bool(true))),
            // functions are equal only to themselves
            (vec!["let f = fn() {}; f == f"], Ok(Object::Bool(true))),
            (vec!["fn() {} == fn() {}"], Ok(Object::Bool(false))),
            // ordering of different types is still an error
            (
                vec!["1 < \"1\""],
                Err(EvalError::InfixOp(Operator::Lt, int(1), string("1"))),
            ),
        ]);
    }

    #[test]
    fn test_lists_and_in() {
        check(vec![
            (vec!["[]"], Ok(Object::List(Rc::new(vec![])))),
            (
                vec!["let x = 2; [1, x + 1]"],
                Ok(Object::List(Rc::new(vec![int(1), int(3)]))),
            ),
            (vec!["[1, [2, 3]][1][0]"], Ok(int(2))),
            (
                vec!["[1, 2, 3][1..]"],
                Ok(Object::List(Rc::new(vec![int(2), int(3)]))),
            ),
            (vec!["[1, \"a\"] == [1, \"a\"]"], Ok(Object::Bool(true))),
            (vec!["[1, 2] != [2, 1]"], Ok(Object::Bool(true))),
            (vec!["2 in [1, 2, 3]"], Ok(Object::Bool(true))),
            (vec!["4 in [1, 2, 3]"], Ok(Object::Bool(false))),
            (vec!["2.0 in [1, 2]"], Ok(Object::Bool(true))),
            (vec!["\"1\" in [1, 2]"], Ok(Object::Bool(false))),
            (vec!["null in [1, null]"], Ok(Object::Bool(true))),
            (vec!["1 in []"], Ok(Object::Bool(false))),
            (vec!["!(1 in [2])"], Ok(Object::Bool(true))),
            (vec!["\"ell\" in \"hello\""], Ok(Object::Bool(true))),
            (vec!["\"\" in \"hello\""], Ok(Object::Bool(true))),
            (vec!["b\"059c\" in b\"a9059cbb\""], Ok(Object::Bool(true))),
            (vec!["b\"bbaa\" in b\"a9059cbb\""], Ok(Object::Bool(false))),
            (vec!["b\"\" in b\"\""], Ok(Object::Bool(true))),
            (
                vec!["1 in 2"],
                Err(EvalError::InfixOp(Operator::In, int(1), int(2))),
            ),
            (
                vec![
                    "fn sign(x) { if x < 0 { \"-\" } else if x == 0 { \"0\" } else { \"+\" } }",
                    "sign(-5) + sign(0) + sign(5)",
                ],
                Ok(string("-0+")),
            ),
        ]);
    }

    #[test]
    fn test_builtins() {
        fn t(src: &str) -> (Vec<&str>, Result<Object, EvalError>) {
            (vec![src], Ok(Object::Bool(true)))
        }
        check(vec![
            t("len(\"héllo\") == 5 && len(b\"a905\") == 2 && len([1, 2, 3]) == 3 && len([]) == 0"),
            t("int(b\"0100\") == 256 && int(b\"\") == 0 && int(b\"ff\") == 255"),
            t("int(\"0x10\") == 16 && int(\"-42\") == -42 && int(7) == 7"),
            t("bytes(\"0xab\") == b\"ab\" && bytes(\"hi\") == b\"6869\" && bytes(b\"01\") == b\"01\""),
            t("selector(\"transfer(address,uint256)\") == b\"a9059cbb\""),
            t("keccak(\"\") == b\"c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470\""),
            t("keccak(b\"\") == keccak(\"\")"),
            t("keccak(\"Transfer(address,address,uint256)\") == b\"ddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef\""),
            // builtins are values and can be shadowed
            t("let f = len; f(\"ab\") == 2"),
            t("let len = 42; len == 42"),
            t("fn f(len) { len + 1 } f(1) == 2"),
            (vec!["len"], Ok(Object::Builtin(Builtin::Len))),
            (vec!["len(1)"], Err(EvalError::Argument("len", int(1)))),
            (vec!["int(\"abc\")"], Err(EvalError::Argument("int", string("abc")))),
            (vec!["int(\"0x\")"], Err(EvalError::Argument("int", string("0x")))),
            // an odd number of digits is padded, like in JSON
            (vec!["bytes(\"0xabc\")"], Ok(Object::Bytes(vec![0x0a, 0xbc]))),
            (vec!["bytes(\"0xzz\")"], Err(EvalError::Argument("bytes", string("0xzz")))),
            (vec!["selector(b\"00\")"], Err(EvalError::Argument("selector", Object::Bytes(vec![0])))),
            (vec!["len(\"a\", \"b\")"], Err(EvalError::ApplyArgsCount(1, 2))),
            (vec!["nope(1)"], Err(EvalError::NotFound("nope".to_string()))),
        ]);
    }

    #[test]
    fn test_equality_warnings() {
        let _ = take_warnings();
        let src = ["1 == \"a\"; 2 == \"b\"; b\"\" != true; 1 == null; null == null"];
        assert_eq!(run_eval(&src), Ok(Object::Bool(true)));
        // comparing with null is not suspicious, repeated warnings are reported once
        assert_eq!(
            take_warnings(),
            vec![
                "Comparing Bytes with Bool is always true".to_string(),
                "Comparing Int with Str is always false".to_string(),
            ]
        );
        assert_eq!(take_warnings(), Vec::<String>::new());
    }

    #[test]
    fn test_arithmetic() {
        check(vec![
            (vec!["1 / 0"], Err(EvalError::DivisionByZero)),
            (vec!["1 % 0"], Err(EvalError::DivisionByZero)),
            (vec!["7 / 2"], Ok(int(3))),
            (vec!["-7 % 2"], Ok(int(-1))),
            (vec!["2 * 1.5"], Ok(Object::Float(3.0))),
            (vec!["1.0 / 0"], Ok(Object::Float(f64::INFINITY))),
            // integers have arbitrary precision
            (
                vec!["9223372036854775807 + 1"],
                Ok(big("9223372036854775808")),
            ),
            (
                vec!["-9223372036854775808 - 1"],
                Ok(big("-9223372036854775809")),
            ),
            (
                vec!["99999999999999999999 * 99999999999999999999"],
                Ok(big("9999999999999999999800000000000000000001")),
            ),
            (
                vec!["100000000000000000000 > 99999999999999999999"],
                Ok(Object::Bool(true)),
            ),
            (
                vec!["b\"ab\" + b\"cd\""],
                Ok(Object::Bytes(vec![0xab, 0xcd])),
            ),
        ]);
    }

    #[test]
    fn test_call_depth() {
        with_stack(|| {
            let ctx = Context::default();
            let f = "fn f(n) { if n == 0 { 0 } else { f(n - 1) } }";
            assert_eq!(
                run_eval_in(&[f, "f(100000)"], &ctx),
                Err(EvalError::StackOverflow(LIMITS.max_depth))
            );
            // the depth counter is reset after the error
            assert_eq!(run_eval_in(&[f, "f(9000)"], &ctx), Ok(int(0)));
        });
    }

    fn tx() -> Object {
        let topic = Object::Bytes(vec![0xdd, 0xf2, 0x52, 0xad]);
        let log = vec![("topics".to_string(), Object::List(Rc::new(vec![topic])))];
        let fields = vec![
            (
                "to".to_string(),
                string("0xdac17f958d2ee523a2206206994597c13d831ec7"),
            ),
            (
                "data".to_string(),
                Object::Bytes(vec![0xa9, 0x05, 0x9c, 0xbb, 0x00, 0x01]),
            ),
            ("value".to_string(), big("1000000000000000000")),
            (
                "logs".to_string(),
                Object::List(Rc::new(vec![Object::Record(Rc::new(
                    log.into_iter().collect(),
                ))])),
            ),
        ];
        Object::Record(Rc::new(fields.into_iter().collect()))
    }

    #[test]
    fn test_postfix() {
        let tests = vec![
            (
                "tx.to == \"0xdac17f958d2ee523a2206206994597c13d831ec7\" && tx.data[0..4] == b\"a9059cbb\"",
                Ok(Object::Bool(true)),
            ),
            ("tx[\"to\"] == tx.to", Ok(Object::Bool(true))),
            ("tx.data[0]", Ok(int(0xa9))),
            ("tx.data[4..]", Ok(Object::Bytes(vec![0x00, 0x01]))),
            ("tx.logs[0].topics[0][..2]", Ok(Object::Bytes(vec![0xdd, 0xf2]))),
            ("tx.value / 1000000000000000000", Ok(int(1))),
            ("tx.nope", Ok(Object::Null)),
            ("tx[\"nope\"] == null", Ok(Object::Bool(true))),
            ("tx.nope.x", Err(EvalError::Field(Object::Null, "x".to_string()))),
            ("\"to\" in tx && !(\"nope\" in tx)", Ok(Object::Bool(true))),
            ("tx.to in [\"0x00\", \"0xdac17f958d2ee523a2206206994597c13d831ec7\"]", Ok(Object::Bool(true))),
            ("tx.data[6]", Err(EvalError::IndexOutOfBounds(6.into(), 6))),
            ("tx.data[-1]", Err(EvalError::IndexOutOfBounds((-1).into(), 6))),
            (
                "tx.data[\"x\"]",
                Err(EvalError::Index(
                    Object::Bytes(vec![0xa9, 0x05, 0x9c, 0xbb, 0x00, 0x01]),
                    string("x"),
                )),
            ),
            ("(1).x", Err(EvalError::Field(int(1), "x".to_string()))),
            // slice bounds are clamped
            ("b\"\"[0..4] == b\"\"", Ok(Object::Bool(true))),
            ("b\"010203\"[2..1]", Ok(Object::Bytes(vec![]))),
            ("b\"010203\"[-5..2]", Ok(Object::Bytes(vec![1, 2]))),
            ("b\"010203\"[1..99999999999999999999]", Ok(Object::Bytes(vec![2, 3]))),
            ("\"hello\"[1..3]", Ok(string("el"))),
            ("\"héllo\"[1]", Ok(string("é"))),
            // any expression can be called
            ("let f = fn(x) { x }; (f)(1)", Ok(int(1))),
            ("(fn(x) { x })(1)", Ok(int(1))),
            ("1(2)", Err(EvalError::NotFunction(int(1)))),
        ];

        for (src, expected) in tests {
            let ctx = Context::default();
            ctx.put("tx".to_string(), tx());
            let res = run_eval_in(&[src], &ctx);
            assert_eq!(res, expected, "{}", src);
        }
    }
}
