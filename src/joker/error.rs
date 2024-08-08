//! This file is error.rs
//!
//!
//!

use std::fmt::Display;

use super::{
    env::EnvError, interpreter::IntoIteratorError, parse::ParserError, scanner::ScannerError,
};

pub trait ReportError {
    fn report(&self);
}

#[derive(Debug)]
pub enum JokerError {
    Env(EnvError),
    Scanner(ScannerError),
    Parser(ParserError),
    Interpreter(IntoIteratorError),
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

#[derive(Debug)]
pub enum AbortError {
    ControlFlow(ControlFlowAbort),
}

impl ReportError for AbortError {
    fn report(&self) {
        match &self {
            AbortError::ControlFlow(control_flow) => ReportError::report(control_flow),
        }
    }
}

#[derive(Debug)]
pub enum ControlFlowAbort {
    Break,
    Continue,
}
impl Display for ControlFlowAbort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ControlFlowAbort::Break => write!(f, "Break"),
            ControlFlowAbort::Continue => write!(f, "Continue"),
        }
    }
}
impl ReportError for ControlFlowAbort {
    fn report(&self) {
        eprintln!("{self}");
    }
}
