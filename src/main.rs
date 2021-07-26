pub(crate) mod error;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod token;
pub(crate) mod buffer;

use std::io;
use std::io::{BufRead, Write};

use crate::lexer::tokenize;
use crate::buffer::Buffer;

fn main() {
    println!("REPL: enter :q to quit.");
    let stdin = io::stdin();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let line = stdin.lock().lines().next().unwrap().unwrap();
        if line.is_empty() {
            continue;
        }
        if line == ":q" {
            break;
        }

        let tokens = {
            let mut buf = Buffer::new(&line);
            tokenize(&mut buf)
                .unwrap_or_default()
                .into_iter()
                .map(|t| format!("\t{:?}", t))
                .collect::<Vec<_>>()
                .join("\n")
        };
        println!("[\n{}\n]", tokens);
    }
}
