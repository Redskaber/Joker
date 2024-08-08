//! This file is env rs

use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

use super::{
    error::{JokerError, ReportError},
    object::Object,
    token::{Token, TokenType},
};

#[derive(Debug, PartialEq)]
pub struct Env {
    symbol: HashMap<String, Object>,
    enclosing: Option<Rc<RefCell<Env>>>, // rc: 引用计数， RefCell: 运行时管理生命周期
}

impl Env {
    pub fn new() -> Env {
        Env {
            symbol: HashMap::new(),
            enclosing: None,
        }
    }
    pub fn new_with_enclosing(enclosing: Rc<RefCell<Env>>) -> Env {
        Env {
            symbol: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: &str, value: Object) {
        self.symbol.insert(name.to_string(), value);
    }
    pub fn get(&self, name: &Token) -> Result<Object, JokerError> {
        match self.symbol.get(&name.lexeme) {
            Some(value) => Ok(value.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get(name),
                None => Err(JokerError::Env(EnvError::new(
                    name,
                    format!("Undefined variable '{}'.", name.lexeme),
                ))),
            },
        }
    }
    pub fn assign(&mut self, name: &Token, value: &Object) -> Result<(), JokerError> {
        if let Entry::Occupied(mut entry) = self.symbol.entry(name.lexeme.to_string()) {
            entry.insert(value.clone());
            Ok(())
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.borrow_mut().assign(name, value),
                None => Err(JokerError::Env(EnvError::new(
                    name,
                    format!("Undefined variable '{}'.", name.lexeme),
                ))),
            }
        }
    }
}

#[derive(Debug)]
pub struct EnvError {
    line: usize,
    where_: String,
    msg: String,
}
impl EnvError {
    pub fn new(token: &Token, msg: String) -> EnvError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        EnvError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn error(line: usize, msg: String) -> EnvError {
        EnvError {
            line,
            where_: String::from(""),
            msg,
        }
    }
}
impl ReportError for EnvError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::joker::{
        object::{literal_i32, literal_null, literal_str},
        token::TokenType,
    };

    use super::*;

    fn maker_token(lexeme: String) -> Token {
        Token {
            ttype: TokenType::Eof,
            lexeme,
            literal: literal_null(),
            line: 0,
        }
    }

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

    #[test]
    fn test_env_new_with_enclosing() {
        let parent = Rc::new(RefCell::new(Env::new()));
        let sub = Env::new_with_enclosing(Rc::clone(&parent)); // Rc::clone -> ptr clone && strong ref
        let enclosing = sub.enclosing.unwrap();
        assert_eq!(enclosing, parent);
    }

    #[test]
    fn test_env_through_sub_visit_parent() {
        let parent: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new()));
        let sub: Env = Env::new_with_enclosing(Rc::clone(&parent));

        sub.enclosing
            .unwrap()
            .borrow_mut()
            .define("sub_key", literal_i32(100));
        let parent_get: Object = parent
            .borrow()
            .get(&maker_token(String::from("sub_key")))
            .unwrap();

        assert_eq!(parent_get, literal_i32(100));
    }

    #[test]
    fn test_env_upcast_get_value() {
        let parent: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new()));
        let sub: Env = Env::new_with_enclosing(Rc::clone(&parent));

        parent.borrow_mut().define("sub_key", literal_i32(100));
        let sub_get: Object = sub.get(&maker_token(String::from("sub_key"))).unwrap();

        assert_eq!(sub_get, literal_i32(100));
    }

    #[test]
    #[should_panic = "\"Undefined variable 'sub_key'.\""]
    fn test_env_get_undefined_value() {
        let parent: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new()));
        let sub: Env = Env::new_with_enclosing(Rc::clone(&parent));

        let sub_get: Object = sub.get(&maker_token(String::from("sub_key"))).unwrap();

        assert_eq!(sub_get, literal_i32(100));
    }
}
