use std::io;
use std::io::{BufRead, Write};
use crate::lexer::{Buf, tokenize};

pub(crate) mod error;
pub(crate) mod token;
pub(crate) mod lexer;
pub(crate) mod parser;

fn main() {
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
            let mut buf = Buf::new(&line);
            tokenize(&mut buf).unwrap_or_default()
                .into_iter()
                .map(|t| format!("\t{:?}", t))
                .collect::<Vec<_>>()
                .join("\n")
        };
        println!("[\n{}\n]", tokens);
    }
}
