//! This file is callable rs
//!
//! - Callable Trait
//!
//! - CallError
//!     - NonCallError
//!     - ArgumentError
//!
//!

use std::{
    error::Error,
    fmt::{Debug, Display},
};

use super::{
    error::{JokerError, ReportError},
    interpreter::Interpreter,
    object::Object,
    token::{Token, TokenType},
};

pub trait Callable: Debug + Display {
    fn call(&self, interpreter: &Interpreter, arguments: &[Object]) -> Result<Object, JokerError>;
    fn arity(&self) -> usize;
}

#[derive(Debug)]
pub enum CallError {
    NonCallable(NonCallError),
    Argument(ArgumentError),
    Struct(StructError),
}

impl Display for CallError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CallError::Argument(arg) => Display::fmt(arg, f),
            CallError::NonCallable(non_call) => Display::fmt(non_call, f),
            CallError::Struct(struct_) => Display::fmt(struct_, f),
        }
    }
}

impl Error for CallError {}

impl ReportError for CallError {
    fn report(&self) {
        match self {
            CallError::NonCallable(non_call) => ReportError::report(non_call),
            CallError::Argument(arg) => ReportError::report(arg),
            CallError::Struct(struct_) => ReportError::report(struct_),
        }
    }
}

#[derive(Debug)]
pub struct NonCallError {
    line: usize,
    where_: String,
    msg: String,
}

impl NonCallError {
    pub fn new(token: &Token, msg: String) -> NonCallError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        NonCallError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> NonCallError {
        let non_err = NonCallError::new(token, msg);
        non_err.report();
        non_err
    }
}

impl Display for NonCallError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "NonCallError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl Error for NonCallError {}

impl ReportError for NonCallError {
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

impl Error for ArgumentError {}

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

impl Error for StructError {}

impl ReportError for StructError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
