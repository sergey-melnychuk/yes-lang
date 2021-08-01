pub(crate) mod buffer;
pub(crate) mod error;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod token;
pub(crate) mod eval;

use std::io;
use std::io::{BufRead, Write};

use crate::error::Error;
use crate::buffer::Buffer;
use crate::lexer::tokenize;
use crate::parser::parse;
use crate::eval::{eval, Object, Context};

fn main() {
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

