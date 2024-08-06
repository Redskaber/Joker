//! This file is error.rs
//!
//!
//!

use super::token::{Token,TokenType};

pub trait ReportError {
    fn report(&self);
}

#[derive(Debug)]
pub enum ErrorCode {
    Scanner        = 66,
    Parser         = 67,
    Eval           = 68,
    Interpreter    = 69,
    Env            = 70,
}

impl From<ErrorCode> for i32 {
    fn from(value: ErrorCode) -> Self {
        match value {
            ErrorCode::Scanner      => 66,
            ErrorCode::Parser       => 67,
            ErrorCode::Eval         => 68,
            ErrorCode::Interpreter  => 69,
            ErrorCode::Env          => 70,
        }
    }
}

#[derive(Debug)]
pub struct JokerError {
    pub line: usize,
    pub where_: String,
    pub msg: String,
    pub code: ErrorCode,
}

impl JokerError {
    pub fn new(token: &Token, msg: String, code: ErrorCode) -> JokerError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };        
        JokerError { line: token.line, where_, msg , code}
    }
    pub fn error(line: usize, msg: String, code: ErrorCode) -> JokerError {
        JokerError { line, where_: String::from(""), msg , code}
    }
    pub fn scanner(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::Scanner)
    } 
    pub fn parse(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::Parser)
    }    
    pub fn eval(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::Eval)
    }
    pub fn interpreter(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::Interpreter)
    }
    pub fn env(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::Env)
    }
    pub fn scan_error(line: usize, msg: String) -> JokerError {
        JokerError { line, where_: String::from(""), msg , code: ErrorCode::Scanner}
    }
    pub fn interpreter_error(msg: String) -> JokerError {
        JokerError { line: 0, where_: String::from(""), msg, code: ErrorCode::Interpreter }
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
