//! This file is joker
//!
//!

use std::env;
use std::fs;
use std::io::{self, stdout, Write};
use std::result;

use super::{
    error::{JokerError, ReportError},
    interpreter::Interpreter,
    parse::Parser,
    scanner::Scanner,
    token::Token,
};

pub fn joker_main() {
    let args: Vec<String> = env::args().collect();
    let joker = Joker::new();
    match args.len() {
        1 => joker.run_prompt(),
        2 => joker.run_file(&args[1]).expect("Could not run file."),
        _ => {
            println!("Usage: joker-ast [script]");
            std::process::exit(64);
        }
    }
}

pub struct Joker {
    interpreter: Interpreter,
}

impl Joker {
    pub fn new() -> Joker {
        Joker {
            interpreter: Interpreter::new(),
        }
    }

    fn run_file(&self, path: &str) -> io::Result<()> {
        let contents: String = fs::read_to_string(path)?;
        if let Err(err) = self.run(contents) {
            err.report();
            std::process::exit(err.code.into());
        }
        Ok(())
    }

    fn run_prompt(&self) {
        print!("> ");
        let _ = stdout().flush();

        let stdin: io::Stdin = io::stdin();
        for r_line in stdin.lines() {
            if let Ok(line) = r_line {
                if line.is_empty() {
                    break;
                }
                if let Err(err) = self.run(line) {
                    err.report();
                }
                print!("> ");
                let _ = stdout().flush();
            } else {
                break;
            }
        }
    }

    fn run(&self, source: String) -> result::Result<(), JokerError> {
        let mut scanner: Scanner = Scanner::new(source);
        let tokens: Vec<Token> = scanner.scan_tokens()?;
        for t in &tokens {
            println!("{t:#?}")
        }
        let mut parser: Parser = Parser::new(tokens);
        if let Some(stmts) = parser.parse() {
            self.interpreter.interpreter(&stmts)?
        }

        Ok(())
    }
}
