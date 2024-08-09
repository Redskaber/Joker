//! This file is error.rs
//!
//!
//!

use super::{
    abort::AbortError, env::EnvError, interpreter::InterpreterError, parse::ParserError,
    scanner::ScannerError,
};

pub trait ReportError {
    fn report(&self);
}

#[derive(Debug)]
pub enum JokerError {
    Env(EnvError),
    Scanner(ScannerError),
    Parser(ParserError),
    Interpreter(InterpreterError),
    Abort(AbortError),
}

impl ReportError for JokerError {
    fn report(&self) {
        match self {
            JokerError::Scanner(scanner) => ReportError::report(scanner),
            JokerError::Parser(parser) => ReportError::report(parser),
            JokerError::Interpreter(inter) => ReportError::report(inter),
            JokerError::Env(env) => ReportError::report(env),
            JokerError::Abort(abort) => ReportError::report(abort),
        }
    }
}
