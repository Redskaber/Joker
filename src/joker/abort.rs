//! This file is Flow handle
//!
//!  - AbortError
//!     - ControlFlow
//!     - Argument
//!
//! - ControlFlowAbort
//!     - Break
//!     - Continue
//!     - Return(Object)
//!
//! - ArgumentAbort
//!     - ArgLimitAbort
//!
//!

use std::{error::Error, fmt::Display};

use super::{
    error::ReportError,
    token::{Token, TokenType},
    types::Object,
};

#[derive(Debug)]
pub enum AbortError {
    ControlFlow(ControlFlowAbort),
    // Argument(ArgumentAbort),
}

impl Display for AbortError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AbortError::ControlFlow(control) => Display::fmt(control, f),
            // AbortError::Argument(arg) => Display::fmt(arg, f),
        }
    }
}

impl Error for AbortError {}

impl ReportError for AbortError {
    fn report(&self) {
        match &self {
            AbortError::ControlFlow(control_flow) => ReportError::report(control_flow),
            // AbortError::Argument(argument) => ReportError::report(argument),
        }
    }
}

#[derive(Debug)]
pub enum ControlFlowAbort {
    Break,
    Continue,
    Return(Option<Object>),
}

impl Display for ControlFlowAbort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ControlFlowAbort::Break => write!(f, "Break"),
            ControlFlowAbort::Continue => write!(f, "Continue"),
            ControlFlowAbort::Return(return_) => match return_ {
                Some(value) => Display::fmt(value, f),
                None => write!(f, "Return(None)"),
            },
        }
    }
}

impl ReportError for ControlFlowAbort {
    fn report(&self) {
        eprintln!("{self}");
    }
}

// #[derive(Debug)]
// pub enum ArgumentAbort {
//     Limit(ArgLimitAbort),
// }

// impl Display for ArgumentAbort {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             ArgumentAbort::Limit(limit) => Display::fmt(limit, f),
//         }
//     }
// }

// impl ReportError for ArgumentAbort {
//     fn report(&self) {
//         match self {
//             ArgumentAbort::Limit(limit) => ReportError::report(limit),
//         }
//     }
// }

#[derive(Debug)]
pub struct ArgLimitAbort {
    line: usize,
    where_: String,
    msg: String,
}

impl ArgLimitAbort {
    pub fn new(token: &Token, msg: String) -> ArgLimitAbort {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        ArgLimitAbort {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> ArgLimitAbort {
        let arg_limit = ArgLimitAbort::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for ArgLimitAbort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ArgLimitAbort(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl ReportError for ArgLimitAbort {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
