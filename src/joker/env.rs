//! This file is env rs

use std::collections::{hash_map::Entry, HashMap};

use super::{error::JokerError, object::Object, token::Token};

#[derive(Debug)]
pub struct Env {
    symbol: HashMap<String, Object>,
    enclosing: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Env {
        Env {
            symbol: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn define(&mut self, name: &String, value: Object) {
        self.symbol.insert(name.to_string(), value);
    }
    pub fn get(&self, name: &Token) -> Result<Object, JokerError> {
        match self.symbol.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => Err(JokerError::env(
                name,
                format!("Undefined variable '{}'.", name.lexeme),
            )),
        }
    }
    pub fn assign(&mut self, name: &Token, value: &Object) -> Result<(), JokerError> {
        if let Entry::Occupied(mut entry) = self.symbol.entry(name.lexeme.to_string()) {
            entry.insert(value.clone());
            Ok(())
        } else {
            Err(JokerError::env(
                name,
                format!("Undefined variable '{}'.", name.lexeme),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::joker::{
        object::{literal_null, literal_str},
        token::TokenType,
    };

    use super::*;

    #[test]
    fn test_define_a_variable() {
        let mut env = Env::new();
        env.define(&String::from("name"), literal_str(String::from("reds")));
        assert!(env.symbol.contains_key("name"));
        assert_eq!(
            env.get(&Token::new(
                TokenType::Identifier,
                String::from("name"),
                literal_null(),
                0
            ))
            .unwrap(),
            literal_str(String::from("reds"))
        );
    }
}
