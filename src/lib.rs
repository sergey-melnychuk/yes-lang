//! yes-lang: Yet anothEr Scripting Language.
//!
//! ```
//! use yes_lang::{Context, Object, Program};
//!
//! let predicate = Program::compile("tx.to == b\"dac17f958d2ee523a2206206994597c13d831ec7\"")?;
//! let ctx = Context::default();
//! ctx.bind("tx", r#"{"to": "0xdac17f958d2ee523a2206206994597c13d831ec7"}"#)?;
//! assert_eq!(predicate.run(&ctx)?, Object::Bool(true));
//! # Ok::<(), yes_lang::Error>(())
//! ```

mod buffer;
mod builtin;
mod compile;
mod error;
mod eval;
mod json;
mod lexer;
mod parser;
mod token;

pub use builtin::Builtin;
pub use compile::Program;
pub use error::{Error, EvalError, ParserError, TokenError};
pub use eval::{take_warnings, Bytes, Context, Function, Limits, Object};
pub use json::{from_json, to_json, IntoObject};
pub use parser::Operator;
pub use token::{is_identifier, Pos, Token};

use crate::buffer::Buffer;
use crate::lexer::tokenize_with_offsets;
use crate::parser::{parse, Input, Statement};

/// Stack size of the thread started by `with_stack`: enough for 10k nested calls
/// even in debug builds.
pub const STACK_SIZE: usize = 1 << 30;

/// Runs `f` on a new thread with a stack of `STACK_SIZE` bytes, for programs that
/// need more nested calls than the default `Limits` allow.
pub fn with_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    std::thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(f)
        .expect("failed to spawn interpreter thread")
        .join()
        .expect("interpreter thread panicked")
}

/// Tokenizes and parses the source, errors point at their line and column.
pub(crate) fn parse_source(code: &str) -> Result<Vec<Statement>, Error> {
    let tokens = tokenize_with_offsets(&Buffer::from_string(code))
        .map_err(|(e, offset)| Error::Lexer(e, Pos::of(code, offset)))?;
    let input = Input::new(tokens.tokens, Pos::all(code, &tokens.offsets));
    // the error is caused by the token the parser looked at last
    parse(&input).map_err(|e| Error::Parser(e, input.pos()))
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::eval::{eval, with_limits};
    use std::path::Path;

    /// Limits of the command line tool, which runs programs on a thread from `with_stack`.
    pub(crate) const LIMITS: Limits = Limits {
        max_depth: 10_000,
        max_calls: None,
    };

    pub(crate) fn read(path: impl AsRef<Path>) -> String {
        std::fs::read_to_string(path).unwrap()
    }

    // '--bind:NAME=VALUE' options in the comments of an example (e.g. in its 'cargo run' line)
    fn declared_binds(src: &str) -> Vec<(String, String)> {
        src.lines()
            .filter(|line| line.trim_start().starts_with("//"))
            .flat_map(|line| line.split_whitespace())
            .filter_map(|word| word.strip_prefix("--bind:"))
            .map(|bind| {
                let (name, value) = bind.split_once('=').unwrap();
                (name.to_string(), value.to_string())
            })
            .collect()
    }

    // every example must evaluate to 'true', with the bindings it declares in its comments
    #[test]
    fn test_examples() {
        let mut files: Vec<_> = std::fs::read_dir("examples")
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().is_some_and(|ext| ext == "yes"))
            .collect();
        files.sort();
        assert!(!files.is_empty(), "no examples found");

        for file in files {
            let name = file.display().to_string();
            let result = with_stack(move || {
                let src = read(&file);
                let ctx = Context::default();
                for (name, value) in declared_binds(&src) {
                    let json = match value.strip_prefix('@') {
                        Some(path) => read(path),
                        None => value,
                    };
                    ctx.bind(&name, json).unwrap();
                }
                // both the tree-walking interpreter and compiled closures are run;
                // 'Object' cannot leave the thread, so the results are returned as text
                let ast = parse_source(&src).unwrap();
                let text = |result: Result<Object, EvalError>| format!("{:?}", result);
                (
                    text(with_limits(LIMITS, || eval(&ast, &ctx.fork()))),
                    text(Program::new(&ast).run_with(&ctx.fork(), LIMITS)),
                )
            });
            let expected = format!("{:?}", Ok::<_, EvalError>(Object::Bool(true)));
            assert_eq!(result, (expected.clone(), expected), "{}", name);
        }
    }

    #[test]
    fn test_file_errors() {
        let tests = vec![
            ("division.yes", "Division by zero at 3:3"),
            (
                "recursion.yes",
                "Stack overflow: call depth exceeds 10000 at 2:24",
            ),
            ("scope.yes", "Not found: 'y' at 2:10"),
            ("syntax.yes", "Unexpected 'Identifier(\"x\")' at 3:7"),
            ("string.yes", "Unterminated string at 2:9"),
            ("bytes.yes", "Invalid hex bytes: b\"a9059cb\" at 2:16"),
        ];

        for (test, expected) in tests {
            let src = read(Path::new("examples/errors").join(test));
            let error = with_stack(move || {
                Program::compile(&src)
                    .and_then(|program| Ok(program.run_with(&Context::default(), LIMITS)?))
                    .unwrap_err()
                    .to_string()
            });

            assert_eq!(error, expected, "{}", test);
        }
    }

    #[test]
    fn test_error_positions() {
        let tests = vec![
            ("let x = 1;\n  x +; y", "Unexpected 'Delimiter(';')' at 2:6"),
            (
                "fn f( {",
                "Expected 'Identifier(\"*\")' but got 'Delimiter('{')' at 1:7",
            ),
            ("f(1,\n2", "Expected 'Delimiter(')')' but got 'End' at 2:2"),
            ("1abc", "Invalid literal: '1abc' at 1:1"),
            (
                "x.1",
                "Expected 'Identifier(\"*\")' but got 'Literal(\"1\")' at 1:3",
            ),
            ("\"héllo\" 2", "Unexpected 'Literal(\"2\")' at 1:9"),
            ("let a = 1 & 2;", "Unexpected: '&' at 1:11"),
            ("\"ok\";\n  b\"zz\"", "Invalid hex bytes: b\"zz\" at 2:3"),
            ("x;\n\"abc", "Unterminated string at 2:1"),
            ("\"\\q\"", "Unexpected: '\\q' at 1:1"),
            ("/* never\nclosed", "Unterminated comment at 1:1"),
        ];
        for (src, expected) in tests {
            let error = parse_source(src).unwrap_err().to_string();
            assert_eq!(error, expected, "{}", src);
        }
    }

    #[test]
    fn test_runtime_error_positions() {
        let tests = vec![
            ("let x = 1;\nx + y", "Not found: 'y' at 2:5"),
            ("fn f(a) {\n  a / 0\n}\nf(1)", "Division by zero at 2:5"),
            ("if 1 { 2 }", "Expected boolean but got '1' at 1:1"),
            (
                "if false { 1 } else if \"x\" { 2 }",
                "Expected boolean but got '\"x\"' at 1:21",
            ),
            ("[1][5]", "Index out of bounds: 5 (length: 1) at 1:4"),
            ("{a: 1}.a.b", "Cannot access 'b' of '1' at 1:9"),
            ("len(1)", "Invalid argument of 'len': '1' at 1:4"),
            ("-\"a\"", "Cannot apply: (Neg, Str(\"a\")) at 1:1"),
            ("1 && 2", "Expected boolean but got '1' at 1:3"),
            (
                "let f = fn(a) { a };\n  f(1, 2)",
                "Cannot apply: expected number of arguments: 1, got: 2 at 2:4",
            ),
            // the innermost position: where the error happened, not where the function was called
            (
                "fn g(x) { x.y.z }\ng({})",
                "Cannot access 'z' of 'null' at 1:14",
            ),
        ];
        for (src, expected) in tests {
            let program = Program::compile(src).unwrap();
            let error = program.run(&Context::default()).unwrap_err().to_string();
            assert_eq!(error, expected, "{}", src);
        }
    }

    #[test]
    fn test_limits() {
        let fib = "fn fib(n) { if n < 2 { n } else { fib(n - 1) + fib(n - 2) } }";
        let program = Program::compile(&format!("{} fib(20)", fib)).unwrap();
        let ctx = Context::default();
        let limits = Limits {
            max_depth: 100,
            max_calls: Some(1000),
        };
        assert_eq!(
            program
                .run_with(&ctx, limits)
                .map_err(EvalError::without_position),
            Err(EvalError::CallLimit(1000))
        );
        // the calls are counted per run
        let program = Program::compile(&format!("{} fib(10)", fib)).unwrap();
        for _ in 0..3 {
            assert_eq!(program.run_with(&ctx, limits), Ok(Object::Int(55.into())));
        }
        // the default limits fit into the stack of a test thread (2MB)
        let deep = "fn f(n) { if n == 0 { 0 } else { f(n - 1) } }";
        let program = Program::compile(&format!("{} f(999)", deep)).unwrap();
        assert_eq!(program.run(&ctx), Ok(Object::Int(0.into())));
        let program = Program::compile(&format!("{} f(1000)", deep)).unwrap();
        assert_eq!(
            program.run(&ctx).map_err(EvalError::without_position),
            Err(EvalError::StackOverflow(1000))
        );
    }
}

// cargo test --release bench -- --ignored --nocapture
#[cfg(test)]
mod bench {
    use super::tests::read;
    use super::*;
    use crate::eval::eval;
    use std::time::Instant;

    const ROUNDS: usize = 2000;

    fn report(what: &str, count: usize, started: Instant) {
        let ns = started.elapsed().as_nanos() as f64 / count as f64;
        println!("{:<40} {:>10.0} ns/tx", what, ns);
    }

    fn transactions() -> (Vec<String>, Vec<Object>) {
        let lines: Vec<String> = read("examples/block.ndjson")
            .lines()
            .map(str::to_string)
            .collect();
        let txs = lines
            .iter()
            .map(|line| line.as_str().into_object().unwrap())
            .collect();
        (lines, txs)
    }

    #[test]
    #[ignore]
    fn bench_stream() {
        let (lines, txs) = transactions();
        let predicate = parse_source(&read("examples/usdt.yes")).unwrap();
        let count = ROUNDS * txs.len();

        let started = Instant::now();
        for _ in 0..ROUNDS {
            for line in &lines {
                std::hint::black_box(line.as_str().into_object().unwrap());
            }
        }
        report("JSON -> Object", count, started);

        let ctx = Context::default();
        let started = Instant::now();
        let mut matched = 0;
        for _ in 0..ROUNDS {
            for tx in &txs {
                let ctx = ctx.fork();
                ctx.put("tx".to_string(), tx.clone());
                if eval(&predicate, &ctx) == Ok(Object::Bool(true)) {
                    matched += 1;
                }
            }
        }
        report("usdt.yes: tree-walking", count, started);
        assert_eq!(matched, 7 * ROUNDS);

        let program = Program::new(&predicate);
        let started = Instant::now();
        let mut matched = 0;
        for _ in 0..ROUNDS {
            for tx in &txs {
                let ctx = ctx.fork();
                ctx.put("tx".to_string(), tx.clone());
                if program.run(&ctx) == Ok(Object::Bool(true)) {
                    matched += 1;
                }
            }
        }
        report("usdt.yes: compiled", count, started);
        assert_eq!(matched, 7 * ROUNDS);
    }

    #[test]
    #[ignore]
    fn bench_breakdown() {
        let (lines, txs) = transactions();
        let count = ROUNDS * txs.len();

        let started = Instant::now();
        for _ in 0..ROUNDS {
            for line in &lines {
                std::hint::black_box(serde_json::from_str::<serde_json::Value>(line).unwrap());
            }
        }
        report("JSON parse only (serde_json::Value)", count, started);

        let values: Vec<serde_json::Value> = lines
            .iter()
            .map(|line| serde_json::from_str(line).unwrap())
            .collect();
        let started = Instant::now();
        for _ in 0..ROUNDS {
            for value in &values {
                std::hint::black_box(from_json(value));
            }
        }
        report("Value -> Object only", count, started);

        for (what, src) in [
            ("true", "true"),
            ("tx.to", "tx.to"),
            (
                "tx.to == b\"..\"",
                "tx.to == b\"dac17f958d2ee523a2206206994597c13d831ec7\"",
            ),
            ("tx.input[..4]", "tx.input[..4]"),
            (
                "selector(\"..\")",
                "selector(\"transfer(address,uint256)\")",
            ),
        ] {
            let program = Program::compile(src).unwrap();
            let ctx = Context::default();
            let started = Instant::now();
            for _ in 0..ROUNDS {
                for tx in &txs {
                    let ctx = ctx.fork();
                    ctx.put("tx".to_string(), tx.clone());
                    std::hint::black_box(program.run(&ctx).unwrap());
                }
            }
            report(what, count, started);
        }
    }
}
