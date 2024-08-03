//! This file is error.rs
//!
//!
//!

use super::token::{Token,TokenType};

pub trait ReportError {
    fn report(&self);
}

#[derive(Debug)]
pub struct JokerError {
    line: usize,
    where_: String,
    msg: String,
}

impl JokerError {
    pub fn new(token: &Token, msg: String) -> JokerError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };        
        JokerError { line: token.line, where_, msg }
    }
    pub fn error(line: usize, msg: String) -> JokerError {
        JokerError { line, where_: String::from(""), msg }
    }
}

impl ReportError for JokerError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
