//! This file is joker
//!
//!

use std::env;
use std::fs;
use std::io::{self, stdout, Write};
use std::rc::Rc;
use std::result;

use super::{
    error::{JokerError, ReportError},
    interpreter::Interpreter,
    parse::Parser,
    resolver::Resolver,
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
    interpreter: Rc<Interpreter>,
}

impl Joker {
    pub fn new() -> Joker {
        Joker {
            interpreter: Rc::new(Interpreter::new()),
        }
    }

    fn run_file(&self, path: &str) -> io::Result<()> {
        let contents: String = fs::read_to_string(path)?;
        if let Err(err) = self.run(contents) {
            match err {
                JokerError::Scanner(scanner_err) => {
                    scanner_err.report();
                    std::process::exit(65);
                }
                JokerError::Parser(_) => std::process::exit(66),
                JokerError::Env(_) => std::process::exit(67),
                JokerError::Interpreter(_) => std::process::exit(68),
                JokerError::Abort(_) => std::process::exit(69),
                JokerError::Call(_) => std::process::exit(70),
                JokerError::System(_) => std::process::exit(71),
                JokerError::Resolver(_) => std::process::exit(72),
            }
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
                if line == "@" {
                    self.interpreter.println_local()
                };
                let _ = self.run(line);
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
        let mut parser: Parser = Parser::new(tokens);

        let stmts: Vec<super::ast::Stmt> = parser.parse()?;
        let resolver: Resolver = Resolver::new(Rc::clone(&self.interpreter));
        resolver.resolve(&stmts)?;

        self.interpreter.interpreter(&stmts)?;

        Ok(())
    }
}
