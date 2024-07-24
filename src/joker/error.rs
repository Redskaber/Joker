pub use std::error::Error as JokerError;
use std::fmt::{Debug, Display};

use super::ast::EvalError;
use super::parse::ParserError;
use super::scanner::ScannerError;

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
}
impl ReportError for RuntimeError {
    fn report<'a>(&self) {
        match self {
            RuntimeError::EvalError(err) => err.report(),
            RuntimeError::ParseError(err) => err.report(),
            RuntimeError::ScannerError(err) => err.report(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::EvalError(err) => Display::fmt(err, f),
            RuntimeError::ParseError(err) => Display::fmt(err, f),
            RuntimeError::ScannerError(err) => Display::fmt(err, f),
        }
    }
}
impl Debug for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::EvalError(err) => Debug::fmt(err, f),
            RuntimeError::ParseError(err) => Debug::fmt(err, f),
            RuntimeError::ScannerError(err) => Debug::fmt(err, f),
        }
    }
}

impl JokerError for RuntimeError {}

pub fn eval_error(err: EvalError) -> Error {
    Error::RuntimeError(RuntimeError::EvalError(err))
}
pub fn parser_error(err: ParserError) -> Error {
    Error::RuntimeError(RuntimeError::ParseError(err))
}
pub fn scanner_error(err: ScannerError) -> Error {
    Error::RuntimeError(RuntimeError::ScannerError(err))
}

#[cfg(test)]
mod test {}
