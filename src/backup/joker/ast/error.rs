//! This file is ast error handler.(evaluate)
use std::fmt::{Debug, Display};
use super::super::{token::Token, error::ReportError};

pub struct EvalError {
    line: usize,
    where_: String,
    msg: String,
}
impl EvalError {
    pub fn new(token: &Token, msg: String) -> EvalError {
        let line: usize = token.line;
        let where_: String = String::from(format!("at '{}'", token));
        EvalError { line, where_, msg }
    }
}
impl ReportError for EvalError {
    fn report(&self) {
        eprintln!("{self:#?}")
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "EvalError(\n\tline: {}, \n\twhere: {}, \n\tmsg: {}\n)",
            self.line, self.where_, self.msg
        )
    }
}
impl Debug for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] where: {}\n\tmsg: {}\n",
            self.line, self.where_, self.msg
        )
    }
}
