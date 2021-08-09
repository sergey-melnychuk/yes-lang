pub(crate) mod buffer;
pub(crate) mod error;
pub(crate) mod eval;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod token;

use std::env::args;
use std::fs::File;
use std::io::{self, BufRead, Read, Write};
use std::path::Path;

use crate::buffer::Buffer;
use crate::error::Error;
use crate::eval::{eval, Context, Object};
use crate::lexer::tokenize;
use crate::parser::parse;

fn main() {
    if args().len() == 1 {
        run_repl(false);
    } else if args().len() == 2 {
        let arg = args().nth(1).unwrap();
        if &arg == "--debug" {
            run_repl(true);
        } else {
            let path = Path::new(&arg);
            let mut ctx = Context::default();
            match run_file(path, &mut ctx) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
    } else {
        eprintln!(
            "unexpected arguments:\n{}",
            args().skip(2).collect::<Vec<_>>().join("\n")
        )
    }
}

pub(crate) fn get_file(path: &Path) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut content = String::with_capacity(4096);
    let _ = file.read_to_string(&mut content)?;
    Ok(content)
}

pub(crate) fn run_file(path: &Path, ctx: &mut Context) -> Result<Object, Error> {
    let code = get_file(path)?;
    let tok = tokenize(&Buffer::from_string(&code))?;
    let ast = parse(&Buffer::new(tok))?;
    Ok(eval(&ast, ctx)?)
}

fn run_repl(debug: bool) {
    println!("REPL: enter :q to quit.");
    let mut ctx = Context::default();

    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let line = if let Some(Ok(line)) = stdin.lock().lines().next() {
            line
        } else {
            break;
        };

        if line.is_empty() {
            continue;
        } else if line == ":q" {
            break;
        } else if line.starts_with(":load ") {
            let path = line.strip_prefix(":load ").unwrap_or_default();
            match run_file(Path::new(path), &mut ctx) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Error: {}", e),
            }
            continue;
        }

        match repl(line, &mut ctx, debug) {
            Ok(obj) => println!("{}", obj),
            Err(e) => eprintln!("Error: {}", e),
        }
    }
}

fn repl(line: String, ctx: &mut Context, debug: bool) -> Result<Object, Error> {
    let buf = Buffer::from_string(&line);
    let tokens = tokenize(&buf)?;
    let buf = Buffer::new(tokens);
    let tree = parse(&buf)?;
    if debug {
        println!("{:#?}\n", &tree);
    }
    let obj = eval(&tree, ctx)?;
    Ok(obj)
}
