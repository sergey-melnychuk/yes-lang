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
    if args().len() > 1 {
        let path = args().nth(1).unwrap();
        match get_file(&path) {
            Ok(code) => {
                match run_file(&code) {
                    Ok(result) => println!("{}", result),
                    Err(e) => eprintln!("Error: {}", e)
                }
            },
            Err(e) => eprintln!("{}: {}", path, e)
        }
    } else {
        run_repl();
    }
}

pub(crate) fn get_file(path: &str) -> std::io::Result<String> {
    let mut file = File::open(Path::new(path))?;
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

fn run_repl() {
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

        match repl(line, &mut ctx) {
            Ok(obj) => println!("{}", obj),
            Err(e) => eprintln!("Error: {}", e)
        }
    }
}

fn repl(line: String, ctx: &mut Context) -> Result<Object, Error> {
    let buf = Buffer::from_string(&line);
    let tokens = tokenize(&buf)?;
    let buf = Buffer::new(tokens);
    let tree = parse(&buf)?;
    dbg!(&tree);
    let obj = eval(tree, ctx)?;
    Ok(obj)
}

