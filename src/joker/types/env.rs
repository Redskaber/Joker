use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use crate::joker::{
    callable::StructError, env::EnvError, error::JokerError, resolver::Error::Struct, token::Token,
};

use super::Type;

pub struct TypeEnv {
    pub symbol: HashMap<String, Type>,
    pub enclosing: Option<Rc<RefCell<TypeEnv>>>, // rc: 引用计数， RefCell: 运行时管理生命周期
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv {
            symbol: HashMap::new(),
            enclosing: None,
        }
    }
    pub fn new_with_enclosing(enclosing: Rc<RefCell<TypeEnv>>) -> TypeEnv {
        TypeEnv {
            symbol: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }
    pub fn declare_type(&mut self, name: String, ty: Type) {
        self.symbol.insert(name, ty);
    }
    pub fn ancestor(&self, distance: usize) -> Option<Rc<RefCell<TypeEnv>>> {
        let mut current_env: Option<Rc<RefCell<TypeEnv>>> = self.enclosing.clone();
        for _ in 1..distance {
            match current_env {
                Some(env) => {
                    current_env = env.borrow().enclosing.clone();
                }
                None => break,
            }
        }
        current_env
    }
    pub fn get_with_depth(&self, depth: usize, name: &Token) -> Result<Type, JokerError> {
        match depth {
            0 => {
                if let Some(object) = self.symbol.get(&name.lexeme) {
                    return Ok(object.clone());
                }
            }
            1.. => {
                if let Some(env) = &self.ancestor(depth) {
                    if let Some(object) = env.borrow().symbol.get(&name.lexeme) {
                        return Ok(object.clone());
                    }
                }
            }
        }
        Err(JokerError::Env(EnvError::report_error(
            name,
            format!(
                "[TypeEnv::get_with_depth] Undefined variable '{}' at line {}.",
                name.lexeme, name.line
            ),
        )))
    }
    pub fn get_type(&self, name: &Token) -> Result<Type, JokerError> {
        match self.symbol.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get_type(name),
                None => Err(JokerError::Resolver(Struct(StructError::report_error(
                    name,
                    format!(
                        "[TypeEnv::get_type] Unknown type for variable '{}'",
                        name.lexeme
                    ),
                )))),
            },
        }
    }
    pub fn assign_with_depth(
        &mut self,
        depth: usize,
        name: &Token,
        value: Type,
    ) -> Result<(), JokerError> {
        match depth {
            0 => {
                self.symbol.insert(name.lexeme.clone(), value);
                return Ok(());
            }
            1.. => {
                if let Some(env) = &self.ancestor(depth) {
                    env.borrow_mut().symbol.insert(name.lexeme.clone(), value);
                    return Ok(());
                }
            }
        }
        Err(JokerError::Env(EnvError::report_error(
            name,
            format!(
                "[TypeEnv::assign_with_depth] Undefined variable '{}' at line {}.",
                name.lexeme, name.line
            ),
        )))
    }
    pub fn assign(&mut self, name: &Token, value: Type) -> Result<(), JokerError> {
        if let Entry::Occupied(mut entry) = self.symbol.entry(name.lexeme.clone()) {
            entry.insert(value);
            Ok(())
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.borrow_mut().assign(name, value),
                None => Err(JokerError::Env(EnvError::report_error(
                    name,
                    format!("[TypeEnv::assign] Undefined variable '{}'.", name.lexeme),
                ))),
            }
        }
    }
}
