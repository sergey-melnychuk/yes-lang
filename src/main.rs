pub(crate) mod buffer;
pub(crate) mod error;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod token;
pub(crate) mod eval;

use std::env::args;
use std::fs::File;
use std::path::Path;
use std::io::{self, BufRead, Write, Read};

use crate::error::Error;
use crate::buffer::Buffer;
use crate::lexer::tokenize;
use crate::parser::parse;
use crate::eval::{eval, Object, Context};

fn main() {
    if args().len() == 1 {
        run_repl(false);
    } else if args().len() == 2 {
        let arg = args().nth(1).unwrap();
        if &arg == "--debug" {
            run_repl(true);
        } else {
            let path = Path::new(&arg);
            match get_file(path) {
                Ok(code) => {
                    match run_file(&code) {
                        Ok(result) => println!("{}", result),
                        Err(e) => eprintln!("Error: {}", e)
                    }
                },
                Err(e) => eprintln!("{}: {}", arg, e)
            }
        }
    } else {
        eprintln!("unexpected arguments:\n{}", args().skip(2).collect::<Vec<_>>().join("\n"))
    }
}

pub(crate) fn get_file(path: &Path) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut content = String::with_capacity(4096);
    let _ = file.read_to_string(&mut content)?;
    Ok(content)
}

pub(crate) fn run_file(content: &str) -> Result<Object, Error> {
    let mut ctx = Context::default();
    let tok = tokenize(&Buffer::from_string(content))?;
    let ast = parse(&Buffer::new(tok))?;
    Ok(eval(ast, &mut ctx)?)
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
            return;
        };
        if line.is_empty() {
            continue;
        }
        if line == ":q" {
            break;
        }

        match repl(line, &mut ctx, debug) {
            Ok(obj) => println!("{}", obj),
            Err(e) => eprintln!("Error: {}", e)
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
    let obj = eval(tree, ctx)?;
    Ok(obj)
}

