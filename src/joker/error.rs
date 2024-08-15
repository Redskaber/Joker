//! This file is error.rs
//!
//!
//!

use std::{
    error::Error,
    fmt::{Debug, Display},
};

use super::{
    abort::AbortError, callable::CallError, env::EnvError, interpreter::InterpreterError,
    parse::ParserError, resolver::ResolverError, scanner::ScannerError,
};

pub trait ReportError {
    fn report(&self);
}

#[derive(Debug)]
pub enum JokerError {
    Scanner(ScannerError),
    Parser(ParserError),
    Env(EnvError),
    Interpreter(InterpreterError),
    Resolver(ResolverError),
    Abort(AbortError),
    Call(CallError),
    System(SystemError),
}

impl Display for JokerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JokerError::Abort(abort) => Display::fmt(abort, f),
            JokerError::Call(call) => Display::fmt(call, f),
            JokerError::Env(env) => Display::fmt(env, f),
            JokerError::Interpreter(inter) => Display::fmt(inter, f),
            JokerError::Parser(parse) => Display::fmt(parse, f),
            JokerError::Resolver(resolve) => Display::fmt(resolve, f),
            JokerError::Scanner(scanner) => Display::fmt(scanner, f),
            JokerError::System(system) => Display::fmt(system, f),
        }
    }
}

impl Error for JokerError {}

impl ReportError for JokerError {
    fn report(&self) {
        match self {
            JokerError::Scanner(scanner) => ReportError::report(scanner),
            JokerError::Parser(parser) => ReportError::report(parser),
            JokerError::Interpreter(inter) => ReportError::report(inter),
            JokerError::Env(env) => ReportError::report(env),
            JokerError::Abort(abort) => ReportError::report(abort),
            JokerError::Call(call) => ReportError::report(call),
            JokerError::System(system) => ReportError::report(system),
            JokerError::Resolver(resolver) => ReportError::report(resolver),
        }
    }
}

#[derive(Debug)]
pub enum SystemError {
    Time(SystemTimeError),
}

impl Display for SystemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SystemError::Time(time) => Display::fmt(time, f),
        }
    }
}

impl Error for SystemError {}

impl ReportError for SystemError {
    fn report(&self) {
        match self {
            SystemError::Time(time) => ReportError::report(time),
        }
    }
}

#[derive(Debug)]
pub struct SystemTimeError {
    msg: String,
}

impl SystemTimeError {
    pub fn new(msg: String) -> SystemTimeError {
        SystemTimeError { msg }
    }
    pub fn report_error(msg: String) -> SystemTimeError {
        let sys_terr = SystemTimeError::new(msg);
        sys_terr.report();
        sys_terr
    }
}

impl Display for SystemTimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SystemTimeError(msg: {})", self.msg)
    }
}

impl Error for SystemTimeError {}

impl ReportError for SystemTimeError {
    fn report(&self) {
        eprintln!("msg: {}\n", self.msg);
    }
}
