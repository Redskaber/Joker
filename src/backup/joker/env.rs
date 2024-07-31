//! This file is joker var save store or load env.

use std::collections::HashMap;
use std::cell::RefCell;
use std::fmt::{Debug, Display};


use super::{
    ast::Literal, 
    error::{ReportError, JokerError}, 
    token::{Token, TokenType}
};

pub struct NodeEnv<'a> {
    env: RefCell<&'a mut DataEnv<'a>>,
}

pub struct DataEnv<'a> {
    symbols: HashMap<&'a str, Literal<'a>>,
    enclosing: Option<NodeEnv<'a>>,
}


impl<'a> DataEnv<'a> {
    pub fn new() -> DataEnv<'a> {
        DataEnv { 
            symbols: HashMap::new(),
            enclosing: None, 
        }
    }
    pub fn define_var(&mut self, var_name: &'a str, var_value: Literal<'a>) {
        self.symbols.insert(var_name, var_value);
    }
    pub fn get_var_value(&self, var_name: &'a Token<'a>) -> Result<Literal<'a>, DataEnvError> {
        match self.symbols.get(var_name.lexeme) {
            Some(value) => Ok(*value),
            None => {
                match &self.enclosing {
                    Some(p_env) => {
                        p_env.env.borrow().get_var_value(var_name)
                    }
                    None => Err(DataEnvError::new(
                        var_name, 
                        String::from(format!("Undefined variable '{}'.", var_name.lexeme))
                    ))
                }
            }
        }
    }
    pub fn assign(&mut self, var_name: &'a Token<'a>, var_value: Literal<'a>) -> Result<(), DataEnvError> {
        if self.symbols.contains_key(var_name.lexeme) {
            self.symbols.insert(var_name.lexeme, var_value);
            return Ok(());
        }else { 
            match &self.enclosing {
                Some(p_env) => {
                    p_env.env.borrow_mut().assign(var_name, var_value)
                },
                None => Err(DataEnvError::new(
                    var_name,
                    String::from(format!("Undefined variable '{}'.", var_name.lexeme)), 
                )),
            }
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
