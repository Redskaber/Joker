pub use std::error::Error as JokerError;
use std::fmt::{Debug, Display};

use super::ast::EvalError;
use super::env::DataEnvError;
use super::parse::ParserError;
use super::scanner::ScannerError;
use super::token::Token;

pub trait ReportError {
    fn report(&self);
}

pub enum Error {
    OtherError,
    RuntimeError(RuntimeError),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::RuntimeError(err) => Display::fmt(&err, f),
            Error::OtherError => write!(f, "OtherError"),
        }
    }
}
impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::RuntimeError(err) => Display::fmt(&err, f),
            Error::OtherError => write!(f, "OtherError"),
        }
    }
}

impl JokerError for Error {}

pub enum RuntimeError {
    EvalError(EvalError),
    ParseError(ParserError),
    ScannerError(ScannerError),
    DataEnvError(DataEnvError),
}
impl RuntimeError {
    pub fn new(err_type: RuntimeError) -> RuntimeError {
        match err_type {
            RuntimeError::EvalError(err) => RuntimeError::EvalError(err),
            RuntimeError::ParseError(err) => RuntimeError::ParseError(err),
            RuntimeError::ScannerError(err) => RuntimeError::ScannerError(err),
            RuntimeError::DataEnvError(err) => RuntimeError::DataEnvError(err),
        }
    }
}

impl ReportError for RuntimeError {
    fn report<'a>(&self) {
        match self {
            RuntimeError::EvalError(err) => err.report(),
            RuntimeError::ParseError(err) => err.report(),
            RuntimeError::ScannerError(err) => err.report(),
            RuntimeError::DataEnvError(err) => err.report(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::EvalError(err) => Display::fmt(err, f),
            RuntimeError::ParseError(err) => Display::fmt(err, f),
            RuntimeError::ScannerError(err) => Display::fmt(err, f),
            RuntimeError::DataEnvError(err) => Display::fmt(err, f),
        }
    }
}
impl Debug for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::EvalError(err) => Debug::fmt(err, f),
            RuntimeError::ParseError(err) => Debug::fmt(err, f),
            RuntimeError::ScannerError(err) => Debug::fmt(err, f),
            RuntimeError::DataEnvError(err) => Debug::fmt(err, f),
        }
    }
}

impl JokerError for RuntimeError {}


pub fn parser_error(err: ParserError) -> Error {
    Error::RuntimeError(RuntimeError::ParseError(err))
}
pub fn scanner_error(err: ScannerError) -> Error {
    Error::RuntimeError(RuntimeError::ScannerError(err))
}

pub fn eval_error_new<'a>(token:&'a Token<'a>, msg: String) -> RuntimeError {
    RuntimeError::EvalError(EvalError::new(token, msg))
}
pub fn data_env_error_new<'a>(token:&'a Token<'a>, msg: String) -> RuntimeError {
    RuntimeError::DataEnvError(DataEnvError::new(token, msg))
}


#[cfg(test)]
mod test {}
