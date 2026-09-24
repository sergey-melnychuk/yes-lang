//! Functions implemented in Rust, callable from programs.

use std::fmt::{Debug, Formatter};

use crate::error::EvalError;
use crate::eval::Object;

/// Calls a function value (of a program, or a native one) with evaluated arguments, so that
/// natives like `map` can call functions passed to them.
pub type Caller<'a> = &'a dyn Fn(&Object, Vec<Object>) -> Result<Object, EvalError>;

type Body = dyn Fn(Vec<Object>, Caller) -> Result<Object, EvalError>;

/// A function implemented in Rust. Bind it with `Context::define`:
///
/// ```
/// use yes_lang::{Context, EvalError, Native, Object, Program};
///
/// let ctx = Context::default();
/// ctx.define(Native::pure("double", 1, |args| match &args[0] {
///     Object::Int(x) => Ok(Object::Int(x * 2)),
///     arg => Err(EvalError::Argument("double", arg.clone())),
/// }));
/// assert_eq!(Program::compile("double(21)")?.run(&ctx)?, Object::Int(42.into()));
/// # Ok::<(), yes_lang::Error>(())
/// ```
pub struct Native {
    name: &'static str,
    arity: usize,
    pure: bool,
    body: Box<Body>,
}

impl Native {
    /// A function that can call functions passed to it (with the `Caller`).
    pub fn new(
        name: &'static str,
        arity: usize,
        body: impl Fn(Vec<Object>, Caller) -> Result<Object, EvalError> + 'static,
    ) -> Self {
        Native {
            name,
            arity,
            pure: false,
            body: Box::new(body),
        }
    }

    /// A function whose result depends only on its arguments: a call with literal arguments
    /// (e.g. `selector("transfer(address,uint256)")`) is evaluated only once.
    pub fn pure(
        name: &'static str,
        arity: usize,
        body: impl Fn(Vec<Object>) -> Result<Object, EvalError> + 'static,
    ) -> Self {
        Native {
            name,
            arity,
            pure: true,
            body: Box::new(move |args, _| body(args)),
        }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub(crate) fn is_pure(&self) -> bool {
        self.pure
    }

    pub(crate) fn call(&self, args: Vec<Object>, caller: Caller) -> Result<Object, EvalError> {
        if args.len() != self.arity {
            return Err(EvalError::ApplyArgsCount(self.arity, args.len()));
        }
        (self.body)(args, caller)
    }
}

impl Debug for Native {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Native").field("name", &self.name).finish()
    }
}

// natives are equal only when they are the same native
impl PartialEq for Native {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}
