//! This file is handle ast self'evaluate not env, create Interpreter.
//! 
use super::{ast::Statement, env::DataEnv, error::RuntimeError};


pub struct Interpreter<'a> {
    env: DataEnv<'a>,
    stmts: Vec<Statement<'a>>,
}
impl<'a> Interpreter<'a> {
    pub fn new(stmts: Vec<Statement<'a>>) -> Interpreter<'a> {
        Interpreter{
            env: DataEnv::new(), 
            stmts
        }
    }
    pub fn interpret(&'a mut self) -> Result<(), RuntimeError> {
        for stmt in &self.stmts {
            if let Err(err) = stmt.execute(&mut self.env) {
                return Err(err);
            }
        }
        Ok(())
    }
} 
