use std::collections::{hash_map::Entry, HashMap};

use crate::joker::{
    callable::StructError,
    env::EnvError,
    error::JokerError,
    resolver::Error::{Env, Struct},
    token::Token,
};

use super::Type;

pub struct TypeEnv {
    pub symbol: Vec<HashMap<String, Type>>,
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv { symbol: Vec::new() }
    }
    pub fn new_global() -> TypeEnv {
        TypeEnv {
            symbol: vec![HashMap::new()],
        }
    }
    pub fn begin_scope(&mut self) {
        self.symbol.push(HashMap::new())
    }
    pub fn end_scope(&mut self) {
        self.symbol.pop();
    }
    pub fn declare_type(&mut self, name: &Token, ty: Type) -> Result<(), JokerError> {
        if let Some(current_scope) = self.symbol.last_mut() {
            current_scope.insert(name.lexeme.clone(), ty);
            Ok(())
        } else {
            Err(JokerError::Resolver(Struct(StructError::report_error(
                name,
                format!(
                    "[TypeEnv::declare_type] Current scope is None.\ntype env: {:#?}",
                    self.symbol
                ),
            ))))
        }
    }
    pub fn get_type(&self, name: &Token) -> Result<Type, JokerError> {
        for scope in self.symbol.iter().rev() {
            if let Some(type_) = scope.get(&name.lexeme) {
                return Ok(type_.clone());
            }
        }
        Err(JokerError::Resolver(Env(EnvError::report_error(
            name,
            String::from("Expected find type, but not find type."),
        ))))
    }
    pub fn assign_type(&mut self, name: &Token, ty: Type) -> Result<(), JokerError> {
        for scope in self.symbol.iter_mut().rev() {
            if let Entry::Occupied(mut occ) = scope.entry(name.lexeme.clone()) {
                occ.insert(ty);
                return Ok(());
            }
        }
        Err(JokerError::Resolver(Struct(StructError::report_error(
            name,
            format!("Undefined variable '{}'.", name.lexeme),
        ))))
    }
}
