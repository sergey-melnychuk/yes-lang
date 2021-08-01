pub(crate) mod buffer;
pub(crate) mod error;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod token;
pub(crate) mod eval;

use std::io;
use std::io::{BufRead, Write};

use crate::buffer::Buffer;
use crate::lexer::tokenize;
use crate::parser::parse;

fn main() {
    println!("REPL: enter :q to quit.");
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

        let (lex, tree) = {
            let buf = Buffer::from_string(&line);
            let (tokens, lexer) = match tokenize(&buf) {
                Ok(tokens) => (
                    tokens.clone(),
                    tokens
                        .iter()
                        .map(|t| format!("\t{:?}", t))
                        .collect::<Vec<_>>()
                        .join("\n"),
                ),
                Err(e) => (vec![], format!("{}", e)),
            };

            let buf = Buffer::new(tokens);
            let tree = match parse(&buf) {
                Ok(statements) => statements
                    .iter()
                    .map(|stmt| format!("\t{:?}", stmt))
                    .collect::<Vec<_>>()
                    .join("\n"),
                Err(e) => format!("{}", e),
            };
            (lexer, tree)
        };
        println!("[\n{}\n]\n[\n{}\n]", lex, tree);
    }
}
