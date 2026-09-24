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
pub use eval::{Bytes, Context, Function, Limits, Object, take_warnings};
pub use json::{IntoObject, from_json, to_json};
pub use parser::Operator;
pub use token::{Pos, Token, is_identifier};

use crate::buffer::Buffer;
use crate::lexer::tokenize_with_offsets;
use crate::parser::{Input, Statement, parse};

// more than a single level of recursion takes, even in debug builds
const RED_ZONE: usize = 128 * 1024;
const STACK_SEGMENT: usize = 2 * 1024 * 1024;

/// Runs `f`, on a new stack segment if the current stack is almost used up. Called at
/// each level of recursion (parsing, compiling and running nested code), so that a
/// deeply nested or recursive program cannot overflow the stack of its host thread.
pub(crate) fn grow<R>(f: impl FnOnce() -> R) -> R {
    stacker::maybe_grow(RED_ZONE, STACK_SEGMENT, f)
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

    // the result an example declares with '// expect: JSON', 'true' by default
    fn expected_result(src: &str) -> serde_json::Value {
        src.lines()
            .find_map(|line| line.trim_start().strip_prefix("// expect:"))
            .map(|json| serde_json::from_str(json).unwrap())
            .unwrap_or(serde_json::Value::Bool(true))
    }

    // every example must evaluate to its expected result (see above), with the bindings
    // it declares in its comments
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
            let result = {
                let src = read(&file);
                let ctx = Context::default();
                for (name, value) in declared_binds(&src) {
                    let json = match value.strip_prefix('@') {
                        Some(path) => read(path),
                        None => value,
                    };
                    ctx.bind(&name, json).unwrap();
                }
                // both the tree-walking interpreter and compiled closures are run
                let ast = parse_source(&src).unwrap();
                let json = |result: Result<Object, EvalError>| {
                    result
                        .and_then(|obj| to_json(&obj))
                        .map(|json| json.to_string())
                        .map_err(|e| e.to_string())
                };
                (
                    json(with_limits(LIMITS, || eval(&ast, &ctx.fork()))),
                    json(Program::new(&ast).run_with(&ctx.fork(), LIMITS)),
                    expected_result(&src).to_string(),
                )
            };
            let expected = Ok(result.2);
            assert_eq!((&result.0, &result.1), (&expected, &expected), "{}", name);
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
            let error = Program::compile(&src)
                .and_then(|program| Ok(program.run_with(&Context::default(), LIMITS)?))
                .unwrap_err()
                .to_string();

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

    // a program nested 'n' times, in each of the ways to nest
    fn nested_programs(n: usize) -> Vec<(&'static str, String)> {
        vec![
            ("parens", format!("{}1{}", "(".repeat(n), ")".repeat(n))),
            ("sum", vec!["1"; n].join(" + ")),
            ("and", vec!["true"; n].join(" && ")),
            ("prefix", format!("{}true", "!".repeat(n))),
            ("lists", format!("{}{}", "[".repeat(n), "]".repeat(n))),
            ("records", format!("{}1{}", "{a: ".repeat(n), "}".repeat(n))),
            (
                "fields",
                format!("let r = {{a: null}}; r{}", ".a".repeat(n)),
            ),
            ("calls", format!("fn f() {{ f }} f{}", "()".repeat(n))),
            ("index", format!("let x = [0]; x{}", "[0]".repeat(n))),
            (
                "ifs",
                format!("{}1{}", "if true { ".repeat(n), " }".repeat(n)),
            ),
            (
                "fns",
                format!("{}1{}", "(fn() { ".repeat(n), " })()".repeat(n)),
            ),
        ]
    }

    #[test]
    fn test_nesting_limit() {
        // on a thread with the default stack (2MB), like in any host of the library
        std::thread::spawn(|| {
            for i in 0..nested_programs(0).len() {
                // the most nested program that can be parsed: it must run without a crash
                let (n, program) = (1..)
                    .map(|n| (n, Program::compile(&nested_programs(n)[i].1)))
                    .take_while(|(_, program)| program.is_ok())
                    .last()
                    .unwrap();
                let (name, src) = &nested_programs(n)[i];
                assert!(n >= 20, "{}: only {} levels", name, n);
                let limits = Limits::default();
                let _ = program.unwrap().run_with(&Context::default(), limits);
                let ast = parse_source(src).unwrap();
                let _ = with_limits(limits, || eval(&ast, &Context::default()));

                let (_, src) = &nested_programs(n + 1)[i];
                let error = Program::compile(src).map(|_| ()).unwrap_err().to_string();
                assert!(error.starts_with("Nesting too deep"), "{}: {}", name, error);
            }
            // a lot of code is fine, as long as it is not nested
            let src = vec!["let x = 1 + 2;"; 10_000].join("\n");
            assert!(Program::compile(&src).is_ok());
        })
        .join()
        .unwrap();
    }

    #[test]
    fn test_scopes_are_freed() {
        let programs = [
            // a named function is bound in the scope it captures: a cycle
            "fn f(n) { if n == 0 { 0 } else { f(n - 1) } } f(10)",
            "let f = fn(x) { x }; f(1)",
            // functions defined in calls, in blocks, closures of closures
            "fn outer(n) { fn inner(x) { x + n } inner(1) } outer(1) + outer(2)",
            "if true { fn g() { 1 } g() } else { 0 }",
            "fn adder(n) { fn(x) { x + n } } map([1, 2], adder(10))",
            "fn f(n, k) { if n == 0 { k() } else { f(n - 1, fn() { k() + 1 }) } } f(50, fn() { 0 })",
            "fn even(n) { if n == 0 { true } else { odd(n - 1) } } fn odd(n) { if n == 0 { false } else { even(n - 1) } } even(20)",
            // errors in the middle of a run
            "fn f() { fn g() { 1 / 0 } g() } f()",
            // closures passed through (and returned from) calls that create no functions
            "fn apply(f, x) { f(x) } fn keep(f) { [f] } apply(fn(x) { x }, 1) + keep(fn() { 2 })[0]()",
            "if true { let y = 1; let f = fn() { y }; f() }",
        ];
        let base = Context::default();
        base.bind("tx", r#"{"to": "0x01"}"#).unwrap();
        for src in programs {
            let program = Program::compile(src).unwrap();
            let live = eval::live_scopes();
            for _ in 0..10 {
                let _ = program.run_once(base.fork(), LIMITS);
                let _ = program.run_tree_walking(base.fork(), LIMITS);
            }
            assert_eq!(eval::live_scopes(), live, "scopes leak: {}", src);
        }
    }

    #[test]
    fn test_only_capturable_scopes_are_tracked() {
        let tracked = |src: &str| {
            let program = Program::compile(src).unwrap();
            let before = eval::tracked_scopes();
            program.run_with(&Context::default(), LIMITS).unwrap();
            eval::tracked_scopes() - before
        };
        // calls and blocks that create no functions: freed as soon as they are done
        let sum = "fn add(a, b) { let c = a + b; c } fn sum(n) { if n == 0 { 0 } else { let m = n - 1; add(n, sum(m)) } }";
        assert_eq!(tracked(&format!("{} sum(100)", sum)), 0);
        assert_eq!(tracked("fn f(x) { x } map([1, 2, 3], f)"), 0);
        // a call that creates a function: each of its scopes can be captured
        assert_eq!(
            tracked("fn adder(n) { fn(x) { x + n } } adder(1)(2) + adder(3)(4)"),
            2
        );
        // a block that defines a function, blocks that bind nothing get no scope at all
        assert_eq!(tracked("if true { fn g() { 1 } g() } else { 0 }"), 1);
        assert_eq!(tracked("if true { if true { 1 } }"), 0);
    }

    #[test]
    fn test_collected_runs_keep_what_is_used() {
        // a closure in the result keeps its scope
        let program = Program::compile("fn adder(n) { fn(x) { x + n } } adder(40)").unwrap();
        let add = program.run_once(Context::default(), LIMITS).unwrap();
        let ctx = Context::default();
        ctx.bind("add", add).unwrap();
        let program = Program::compile("add(2)").unwrap();
        assert_eq!(program.run(&ctx), Ok(Object::Int(42.into())));

        // bindings of 'run' stay in the context, with the scopes they use
        let ctx = Context::default();
        let program =
            Program::compile("fn adder(n) { fn(x) { x + n } } let a = adder(1);").unwrap();
        program.run(&ctx).unwrap();
        let program = Program::compile("fn twice(x) { a(a(x)) } twice(40)").unwrap();
        assert_eq!(program.run(&ctx), Ok(Object::Int(42.into())));
        assert_eq!(program.run(&ctx), Ok(Object::Int(42.into())));
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
        // the stack grows as needed: the default depth works on a test thread (2MB stack)
        let deep = "fn f(n) { if n == 0 { 0 } else { f(n - 1) } }";
        let program = Program::compile(&format!("{} f(9999)", deep)).unwrap();
        assert_eq!(program.run(&ctx), Ok(Object::Int(0.into())));
        let program = Program::compile(&format!("{} f(10000)", deep)).unwrap();
        assert_eq!(
            program.run(&ctx).map_err(EvalError::without_position),
            Err(EvalError::StackOverflow(10_000))
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
