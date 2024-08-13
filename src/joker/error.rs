//! This file is error.rs
//!
//!
//!

use super::{
    abort::AbortError, callable::CallError, env::EnvError, interpreter::InterpreterError,
    parse::ParserError, resolver::ResolverError, scanner::ScannerError,
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
    Call(CallError),
    System(SystemError),
    Resolver(ResolverError),
}

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
impl ReportError for SystemTimeError {
    fn report(&self) {
        eprintln!("msg: {}\n", self.msg);
    }
}
