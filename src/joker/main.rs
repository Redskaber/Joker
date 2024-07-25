//! This file is joker
use std::env;
use std::fs;
use std::io;
use std::result;

use super::error::parser_error;
use super::error::scanner_error;
use super::error::Error;
use super::interpreter::Interpreter;
use super::{
    error::{ReportError, RuntimeError},
    parse::Parser,
    scanner::Scanner,
};

pub fn joker_main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => {
            let self_: &str = args[0].as_str();
            let _ = run_file(self_);
        }
        2.. => {
            println!("Usage: joker [script]");
            let path = args[1].as_str();
            let _ = run_file(path);
            std::process::exit(64);
        }
        _ => {
            let _ = run_prompt();
        }
    }

    Ok(())
}

fn run_file(path: &str) -> io::Result<()> {
    let contents: String = fs::read_to_string(path)?;
    if let Err(err) = run(contents.as_str()) {
        match err {
            Error::RuntimeError(run_err) => match run_err {
                RuntimeError::EvalError(_)
                | RuntimeError::ScannerError(_)
                | RuntimeError::ParseError(_) 
                | RuntimeError::DataEnvError(_) => {
                    run_err.report();
                    std::process::exit(70)
                }
            },
            Error::OtherError => std::process::exit(65),
        }
    }
    Ok(())
}

fn run_prompt() -> io::Result<()> {
    let stdin: io::Stdin = io::stdin();
    for r_line in stdin.lines() {
        if let Ok(line) = r_line {
            if line.is_empty() {
                break;
            }
            if let Err(err) = run(line.as_str()) {
                match err {
                    Error::RuntimeError(err) => err.report(),
                    Error::OtherError => {}
                }
            }
        }
    }

    Ok(())
}

// this func can raise err JokerError
fn run<'a>(source: &'a str) -> result::Result<(), Error> {
    let mut scanner: Scanner = Scanner::new(source);
    match scanner.scan_tokens() {
        Ok(tokens) => {
            let mut parser = Parser::new(tokens.as_slice());
            match parser.parse() {
                Ok(stmts) => {
                    let mut interpreter = Interpreter::new(stmts);
                    if let Err(err) = interpreter.interpret() {
                        return Err(Error::RuntimeError(RuntimeError::new(err)))
                    }
                }
                Err(err) => return Err(parser_error(err)),
            }
        }
        Err(err) => return Err(scanner_error(err)),
    };
    Ok(())
}
