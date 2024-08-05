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
    ScannerError        = 66,
    ParserError         = 67,
    EvalError           = 68,
    InterpreterError    = 69,
}

impl Into<i32> for ErrorCode {
    fn into(self) -> i32 {
        match self {
            Self::ScannerError      => 66,
            Self::ParserError       => 67,
            Self::EvalError         => 68,
            Self::InterpreterError  => 69,
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
        JokerError::new(token, msg, ErrorCode::ScannerError)
    } 
    pub fn parse(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::ParserError)
    }    
    pub fn eval(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::EvalError)
    }
    pub fn interpreter(token: &Token, msg: String) -> JokerError {
        JokerError::new(token, msg, ErrorCode::InterpreterError)
    }
    pub fn scan_error(line: usize, msg: String) -> JokerError {
        JokerError { line, where_: String::from(""), msg , code: ErrorCode::ScannerError}
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
