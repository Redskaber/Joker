//! This file is joker
use std::env;
use std::fs;
use std::io;
use std::io::{stdout, Write};
use std::result;

use super::{error::JokerError, scanner::Scanner, token::Token};

pub fn joker_main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => run_prompt(),
        2 => run_file(&args[1]).expect("Could not run file."),
        _ => {
            println!("Usage: joker-ast [script]");
            std::process::exit(64);
        }
    }
}

fn run_file(path: &str) -> io::Result<()> {
    let contents: String = fs::read_to_string(path)?;
    if let Err(err) = run(contents) {
        err.report("".to_string());
        std::process::exit(65)
    }
    Ok(())
}

fn run_prompt() {
    print!("> ");
    let _ = stdout().flush();

    let stdin: io::Stdin = io::stdin();
    for r_line in stdin.lines() {
        if let Ok(line) = r_line {
            if line.is_empty() {
                break;
            }
            match run(line) {
                Ok(_) => {
                    print!("> ");
                    let _ = stdout().flush();
                }
                Err(err) => err.report("".to_string()),
            }
        } else {
            break;
        }
    }
}

fn run(source: String) -> result::Result<(), JokerError> {
    let mut scanner: Scanner = Scanner::new(source);
    let tokens: &Vec<Token> = scanner.scan_tokens()?;

    for token in tokens {
        println!("{token:#?}");
    }
    Ok(())
}
