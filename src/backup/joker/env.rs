//! This file is joker var save store or load env.

use std::collections::HashMap;
use std::fmt::{Debug, Display};

use super::{
    ast::Literal, 
    error::{ReportError, JokerError}, 
    token::{Token, TokenType}
};


pub struct GlobalDataEnv<'a> {
    vars: HashMap<&'a str, Literal<'a>>
}
impl<'a> GlobalDataEnv<'a> {
    pub fn new() -> GlobalDataEnv<'a> {
        GlobalDataEnv { vars: HashMap::new() }
    }
    pub fn define_var(&mut self, var_name: &'a str, var_value: Literal<'a>) {
        self.vars.insert(var_name, var_value);
    }
    pub fn get_var_value(&self, var_name: &'a Token<'a>) -> Result<&Literal<'a>, DataEnvError> {
        match self.vars.get(var_name.lexeme) {
            Some(value) => Ok(value),
            None => Err(DataEnvError::new(
                var_name, 
                String::from(format!("Undefined variable '{}'.", var_name.lexeme))))
        }
    }
}

pub struct DataEnvError {
    line: usize,
    where_: String,
    msg: String,
}

impl DataEnvError {
    pub fn new<'a>(token: &'a Token<'a>, msg: String) -> DataEnvError {
        let where_: String = if token.token_type == TokenType::Eof {
            String::from(" at end")
        } else {
            String::from(format!(" at '{}'", token.lexeme))
        };
        DataEnvError {
            line: token.line,
            where_,
            msg,
        }
    }
}
impl ReportError for DataEnvError {
    fn report(&self) {
        eprintln!("{self:#?}")
    }
}
impl Display for DataEnvError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "DataEnvError(\n\tline: {}, \n\twhere: {}, \n\tmsg: {}\n)",
            self.line, self.where_, self.msg
        )
    }
}
impl Debug for DataEnvError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[line {}] where: {}\n\tmsg: {}\n",
            self.line, self.where_, self.msg
        )
    }
}

impl JokerError for DataEnvError {}
