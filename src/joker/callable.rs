//! This file is callable rs
//!
//! - Callable Trait
//!
//! - Error
//!     - NonError
//!     - ArgumentError
//!
//!

use std::fmt::{Debug, Display};

use super::{
    error::{JokerError, ReportError},
    interpreter::Interpreter,
    token::{Token, TokenType},
    types::Object,
};

pub trait Callable: Debug + Display {
    fn call(
        &self,
        interpreter: &Interpreter,
        arguments: &[Object],
    ) -> Result<Option<Object>, JokerError>;
    fn arity(&self) -> usize;
}

#[derive(Debug)]
pub enum Error {
    NonCallable(NonError),
    Argument(ArgumentError),
    Struct(StructError),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Argument(arg) => Display::fmt(arg, f),
            Error::NonCallable(non_call) => Display::fmt(non_call, f),
            Error::Struct(struct_) => Display::fmt(struct_, f),
        }
    }
}

impl std::error::Error for Error {}

impl ReportError for Error {
    fn report(&self) {
        match self {
            Error::NonCallable(non_call) => ReportError::report(non_call),
            Error::Argument(arg) => ReportError::report(arg),
            Error::Struct(struct_) => ReportError::report(struct_),
        }
    }
}

#[derive(Debug)]
pub struct NonError {
    line: usize,
    where_: String,
    msg: String,
}

impl NonError {
    pub fn new(token: &Token, msg: String) -> NonError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        NonError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> NonError {
        let non_err = NonError::new(token, msg);
        non_err.report();
        non_err
    }
}

impl Display for NonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "NonError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl std::error::Error for NonError {}

impl ReportError for NonError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[derive(Debug)]
pub struct ArgumentError {
    line: usize,
    where_: String,
    msg: String,
}

impl ArgumentError {
    pub fn new(token: &Token, msg: String) -> ArgumentError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        ArgumentError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> ArgumentError {
        let arg_err = ArgumentError::new(token, msg);
        arg_err.report();
        arg_err
    }
}

impl Display for ArgumentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ArgumentError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl std::error::Error for ArgumentError {}

impl ReportError for ArgumentError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[derive(Debug)]
pub struct StructError {
    line: usize,
    where_: String,
    msg: String,
}

impl StructError {
    pub fn new(token: &Token, msg: String) -> StructError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        StructError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> StructError {
        let arg_err = StructError::new(token, msg);
        arg_err.report();
        arg_err
    }
}

impl Display for StructError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "StructError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl std::error::Error for StructError {}

impl ReportError for StructError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
