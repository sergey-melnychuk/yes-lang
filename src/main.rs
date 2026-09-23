use std::env::args;
use std::io::{self, BufRead, Write};
use std::process::ExitCode;

use yes_lang::{
    is_identifier, take_warnings, to_json, with_stack, Context, Error, Limits, Object, Program,
};

const USAGE: &str = "usage: yes-lang [FILE] [OPTIONS]

  yes-lang                       start REPL
  yes-lang FILE                  run FILE, print the result
  yes-lang FILE --stream:NAME    read JSON lines from stdin, bind each to NAME and run FILE:
                                 'true' prints the line, 'false' or 'null' skips it,
                                 any other value is printed as JSON

options:
  --bind:NAME=JSON               bind JSON (converted to records, lists, ...) to NAME
  --bind:NAME=@PATH              bind JSON from the file at PATH to NAME
  --count                        (stream) print the number of outputs instead of them
  --max-calls:N                  stop a run after N function calls (default: no limit)
  --max-depth:N                  stop a run at N nested calls (default: 10000)
  --debug                        (REPL) print AST and scope after each line

exit code (like grep): 0 - the result is not 'false' (a stream printed something),
                       1 - the result is 'false' (a stream printed nothing), 2 - an error

REPL commands:
  :load PATH                     run the file at PATH
  :bind NAME=JSON, :bind NAME=@PATH
  :q                             quit";

const OK: u8 = 0;
const FALSE: u8 = 1;
const ERROR: u8 = 2;

#[derive(Debug, PartialEq)]
struct Options {
    file: Option<String>,
    binds: Vec<(String, String)>,
    stream: Option<String>,
    count: bool,
    limits: Limits,
    debug: bool,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            file: None,
            binds: vec![],
            stream: None,
            count: false,
            // runs on a thread from 'with_stack', so calls can be nested deeper
            limits: Limits {
                max_depth: 10_000,
                max_calls: None,
            },
            debug: false,
        }
    }
}

fn parse_args(args: &[String]) -> Result<Options, String> {
    let number = |value: &str, arg: &str| {
        value
            .parse::<u64>()
            .map_err(|_| format!("expected a number: '{}'", arg))
    };
    let mut options = Options::default();
    for arg in args {
        if let Some(bind) = arg.strip_prefix("--bind:") {
            options.binds.push(parse_bind(bind)?);
        } else if let Some(name) = arg.strip_prefix("--stream:") {
            if !is_identifier(name) {
                return Err(format!("invalid name: '{}'", name));
            }
            options.stream = Some(name.to_string());
        } else if arg == "--count" {
            options.count = true;
        } else if let Some(value) = arg.strip_prefix("--max-calls:") {
            options.limits.max_calls = Some(number(value, arg)?);
        } else if let Some(value) = arg.strip_prefix("--max-depth:") {
            options.limits.max_depth = number(value, arg)? as usize;
        } else if arg == "--debug" {
            options.debug = true;
        } else if arg.starts_with("--") || options.file.is_some() {
            return Err(format!("unexpected argument: '{}'", arg));
        } else {
            options.file = Some(arg.clone());
        }
    }
    if options.stream.is_some() && options.file.is_none() {
        return Err("--stream:NAME requires FILE".to_string());
    }
    if options.count && options.stream.is_none() {
        return Err("--count requires --stream:NAME".to_string());
    }
    Ok(options)
}

// parses 'NAME=JSON' or 'NAME=@PATH'
fn parse_bind(bind: &str) -> Result<(String, String), String> {
    match bind.split_once('=') {
        Some((name, value)) if is_identifier(name) => Ok((name.to_string(), value.to_string())),
        _ => Err(format!("expected NAME=JSON or NAME=@PATH, got: '{}'", bind)),
    }
}

/// Binds JSON text, or JSON from a file if the value is '@PATH'.
fn bind(ctx: &Context, name: &str, value: &str) -> Result<(), Error> {
    match value.strip_prefix('@') {
        Some(path) => ctx.bind(name, std::fs::read_to_string(path)?),
        None => ctx.bind(name, value),
    }
}

fn main() -> ExitCode {
    ExitCode::from(with_stack(run))
}

fn run() -> u8 {
    let args: Vec<String> = args().skip(1).collect();
    let options = match parse_args(&args) {
        Ok(options) => options,
        Err(e) => {
            eprintln!("Error: {}\n\n{}", e, USAGE);
            return ERROR;
        }
    };

    let ctx = Context::default();
    for (name, value) in &options.binds {
        if let Err(e) = bind(&ctx, name, value) {
            eprintln!("Error: --bind:{}: {}", name, e);
            return ERROR;
        }
    }

    match (&options.file, &options.stream) {
        (None, _) => {
            run_repl(&ctx, &options);
            OK
        }
        (Some(file), None) => {
            let result = compile_file(file).and_then(|p| Ok(p.run_with(&ctx, options.limits)?));
            print_warnings();
            match result {
                Ok(obj) => {
                    println!("{}", obj);
                    if obj == Object::Bool(false) {
                        FALSE
                    } else {
                        OK
                    }
                }
                Err(e) => {
                    eprintln!("Error: {}", e);
                    ERROR
                }
            }
        }
        (Some(file), Some(name)) => {
            let code = match compile_file(file) {
                Ok(program) => run_stream(&program, &ctx, name, &options),
                Err(e) => {
                    eprintln!("Error: {}", e);
                    ERROR
                }
            };
            print_warnings();
            code
        }
    }
}

fn compile_file(path: &str) -> Result<Program, Error> {
    Program::compile(&std::fs::read_to_string(path)?)
}

fn print_warnings() {
    for warning in take_warnings() {
        eprintln!("Warning: {}", warning);
    }
}

/// What to print for a line of a stream: the line itself, nothing, or a value as JSON.
fn apply(
    program: &Program,
    ctx: &Context,
    name: &str,
    line: &str,
    limits: Limits,
) -> Result<Option<String>, Error> {
    // each line is bound in its own scope, so lines do not see each other
    let ctx = ctx.fork();
    ctx.bind(name, line)?;
    Ok(match program.run_with(&ctx, limits)? {
        Object::Bool(true) => Some(line.to_string()),
        Object::Bool(false) | Object::Null => None,
        obj => Some(to_json(&obj)?.to_string()),
    })
}

// the program is compiled once and applied to each line of stdin
fn run_stream(program: &Program, ctx: &Context, name: &str, options: &Options) -> u8 {
    let mut out = io::BufWriter::new(io::stdout().lock());
    let (mut outputs, mut errors) = (0u64, 0u64);
    for (number, line) in io::stdin().lock().lines().enumerate() {
        let line = match line {
            Ok(line) => line,
            Err(e) => {
                eprintln!("Error: {}", e);
                return ERROR;
            }
        };
        if line.trim().is_empty() {
            continue;
        }
        match apply(program, ctx, name, &line, options.limits) {
            Ok(Some(output)) => {
                outputs += 1;
                if !options.count && writeln!(out, "{}", output).is_err() {
                    // the reader is gone (e.g. '| head -1'): not an error, just stop
                    return OK;
                }
            }
            Ok(None) => {}
            Err(e) => {
                errors += 1;
                eprintln!("Error: line {}: {}", number + 1, e);
            }
        }
    }
    if options.count {
        let _ = writeln!(out, "{}", outputs);
    }
    let _ = out.flush();
    match (errors, outputs) {
        (0, 0) => FALSE,
        (0, _) => OK,
        _ => ERROR,
    }
}

fn run_repl(ctx: &Context, options: &Options) {
    println!("REPL: enter :q to quit.");

    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let line = if let Some(Ok(line)) = stdin.lock().lines().next() {
            line
        } else {
            break;
        };

        let result = if line.is_empty() {
            continue;
        } else if line == ":q" {
            break;
        } else if let Some(path) = line.strip_prefix(":load ") {
            compile_file(path).and_then(|p| Ok(p.run_with(ctx, options.limits)?))
        } else if let Some(arg) = line.strip_prefix(":bind ") {
            match parse_bind(arg) {
                Ok((name, value)) => match bind(ctx, &name, &value) {
                    Ok(()) => println!("bound '{}'", name),
                    Err(e) => eprintln!("Error: {}", e),
                },
                Err(e) => eprintln!("Error: {}", e),
            }
            continue;
        } else {
            Program::compile(&line).and_then(|program| {
                let obj = program.run_with(ctx, options.limits)?;
                if options.debug {
                    println!("{:#?}\n", program);
                    println!("{:#?}\n", ctx);
                }
                Ok(obj)
            })
        };

        print_warnings();
        match result {
            Ok(obj) => println!("{}", obj),
            Err(e) => eprintln!("Error: {}", e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use yes_lang::EvalError;

    fn read(path: &str) -> String {
        std::fs::read_to_string(path).unwrap()
    }

    fn program(src: &str) -> Program {
        Program::compile(src).unwrap()
    }

    fn apply_to(
        program: &Program,
        ctx: &Context,
        name: &str,
        line: &str,
    ) -> Result<Option<String>, Error> {
        apply(program, ctx, name, line, Limits::default())
    }

    #[test]
    fn test_bind() {
        let ctx = Context::default();
        let value = serde_json::json!({"to": "0x00", "n": 1});
        ctx.bind("a", r#"{"to": "0x00"}"#).unwrap();
        ctx.bind("b", r#"[1, 2]"#.to_string()).unwrap();
        ctx.bind("c", &value).unwrap();
        ctx.bind("d", value).unwrap();
        ctx.bind("e", Object::Int(42.into())).unwrap();
        bind(&ctx, "f", "@examples/tx.json").unwrap();
        bind(&ctx, "g", "\"@not a file\"").unwrap();
        let src = "a.to == b\"00\" && b == [1, 2] && c == d && c.n == 1 && e == 42 \
                   && f.to == b\"dac17f958d2ee523a2206206994597c13d831ec7\" && g == \"@not a file\"";
        assert_eq!(program(src).run(&ctx), Ok(Object::Bool(true)));

        assert!(matches!(ctx.bind("x", "{"), Err(Error::Json(_))));
        assert!(matches!(bind(&ctx, "x", "@nope.json"), Err(Error::IO(_))));
    }

    #[test]
    fn test_parse_args() {
        let args =
            |args: &[&str]| parse_args(&args.iter().map(|arg| arg.to_string()).collect::<Vec<_>>());
        assert_eq!(args(&[]), Ok(Options::default()));
        assert_eq!(
            args(&[
                "f.yes",
                "--bind:tx=@tx.json",
                "--bind:n={\"a\": \"=\"}",
                "--stream:t",
                "--count",
                "--max-calls:100",
                "--max-depth:50",
            ]),
            Ok(Options {
                file: Some("f.yes".to_string()),
                binds: vec![
                    ("tx".to_string(), "@tx.json".to_string()),
                    ("n".to_string(), "{\"a\": \"=\"}".to_string()),
                ],
                stream: Some("t".to_string()),
                count: true,
                limits: Limits {
                    max_depth: 50,
                    max_calls: Some(100),
                },
                debug: false,
            })
        );
        assert_eq!(
            args(&["--debug", "--stream:x", "f.yes"]).map(|o| (o.debug, o.stream)),
            Ok((true, Some("x".to_string())))
        );
        assert!(args(&["--bind:tx"]).is_err());
        assert!(args(&["--bind:1x=1"]).is_err());
        assert!(args(&["f.yes", "--stream:"]).is_err());
        assert!(args(&["f.yes", "--stream"]).is_err());
        assert!(args(&["f.yes", "--filter"]).is_err());
        assert!(args(&["f.yes", "--count"]).is_err());
        assert!(args(&["f.yes", "--max-calls:x"]).is_err());
        assert!(args(&["--stream:x"]).is_err());
        assert!(args(&["a.yes", "b.yes"]).is_err());
    }

    #[test]
    fn test_stream_example() {
        let predicate = program(&read("examples/usdt.yes"));
        let block = read("examples/block.ndjson");
        let matched: Vec<usize> = block
            .lines()
            .enumerate()
            .filter(|(_, line)| {
                apply_to(&predicate, &Context::default(), "tx", line).unwrap()
                    == Some(line.to_string())
            })
            .map(|(i, _)| i)
            .collect();
        // USDT transfers among the first 30 transactions of block 26040477
        assert_eq!(matched, vec![12, 13, 14, 15, 16, 18, 22]);
    }

    #[test]
    fn test_stream_output() {
        let ctx = Context::default();
        let line = r#"{"to": "0x01", "n": 2}"#;
        let apply = |src: &str| apply_to(&program(src), &ctx, "x", line);
        // 'true' is the line itself, 'false' and 'null' are nothing, other values are JSON
        assert_eq!(apply("x.to == b\"01\"").unwrap(), Some(line.to_string()));
        assert_eq!(apply("x.to == b\"02\"").unwrap(), None);
        assert_eq!(apply("x.nope").unwrap(), None);
        assert_eq!(apply("x.n * 21").unwrap(), Some("42".to_string()));
        assert_eq!(apply("x.to").unwrap(), Some("\"0x01\"".to_string()));
        assert_eq!(
            apply("[x.n, \"a\"]").unwrap(),
            Some("[2,\"a\"]".to_string())
        );
        assert_eq!(
            apply("if x.n > 1 { x } else { null }").unwrap(),
            Some(r#"{"n":2,"to":"0x01"}"#.to_string())
        );
        assert!(matches!(
            apply("len"),
            Err(Error::Eval(EvalError::NotJson(_)))
        ));
    }

    #[test]
    fn test_stream_binds() {
        // the program sees other bindings, each line is bound in its own scope
        let ctx = Context::default();
        bind(&ctx, "tokens", r#"["0x01", "0x02"]"#).unwrap();
        let predicate = program("tx.to in tokens");
        assert!(apply_to(&predicate, &ctx, "t", r#"{}"#).is_err());
        assert!(apply_to(&predicate, &ctx, "tx", r#"{"to": "0x02"}"#)
            .unwrap()
            .is_some());
        assert!(apply_to(&predicate, &ctx, "tx", r#"{"to": "0x03"}"#)
            .unwrap()
            .is_none());
        assert_eq!(ctx.get("tx"), None);
    }

    #[test]
    fn test_stream_errors() {
        let ctx = Context::default();
        let predicate = program("tx.to == b\"00\"");
        assert!(apply_to(&predicate, &ctx, "tx", r#"{"to": null}"#)
            .unwrap()
            .is_none());
        assert!(apply_to(&predicate, &ctx, "tx", r#"{}"#).unwrap().is_none());
        assert!(matches!(
            apply_to(&predicate, &ctx, "tx", "not json"),
            Err(Error::Json(_))
        ));
        // limits apply to each line
        let predicate = program("fn f(n) { if n == 0 { true } else { f(n - 1) } } f(tx.n)");
        let limits = Limits {
            max_depth: 100,
            max_calls: Some(10),
        };
        assert!(apply(&predicate, &ctx, "tx", r#"{"n": 5}"#, limits)
            .unwrap()
            .is_some());
        assert!(matches!(
            apply(&predicate, &ctx, "tx", r#"{"n": 50}"#, limits),
            Err(Error::Eval(EvalError::CallLimit(10)))
        ));
        assert!(apply(&predicate, &ctx, "tx", r#"{"n": 5}"#, limits)
            .unwrap()
            .is_some());
    }
}
