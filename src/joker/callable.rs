//! This file is callable rs
//!
//! - Callable Trait
//!
//! - CallError
//!     - NonCallError
//!     - ArgumentError
//!
//!

use std::fmt::{Debug, Display};

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
impl ReportError for StructError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
