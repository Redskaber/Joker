//! This file is instance rs
//!
//! - Instance
//!     - Instance
//!
//!
//!

use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::joker::{
    error::JokerError,
    interpreter::InterpreterError,
    object::Object as OEnum,
    token::Token,
    types::{DeepClone, Object},
};

use super::{Binder, Class, UpCast};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instance {
    pub class: Rc<RefCell<Box<Class>>>,
    fields: Rc<RefCell<HashMap<String, Object>>>,
}

impl DeepClone for Instance {
    fn deep_clone(&self) -> Self {
        Instance {
            class: Rc::new(RefCell::new((*self.class.borrow()).clone())),
            fields: Rc::new(RefCell::new((*self.fields.borrow()).clone())),
        }
    }
}

impl DeepClone for Box<Instance> {
    fn deep_clone(&self) -> Self {
        Box::new(Instance {
            class: Rc::new(RefCell::new((*self.class.borrow()).clone())),
            fields: Rc::new(RefCell::new((*self.fields.borrow()).clone())),
        })
    }
}

impl UpCast<OEnum> for Instance {
    fn upcast(&self) -> OEnum {
        OEnum::Instance(Box::new(self.clone()))
    }
    fn upcast_into(self) -> OEnum {
        OEnum::Instance(Box::new(self))
    }
}

impl Hash for Instance {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (Rc::as_ptr(&self.class), Rc::as_ptr(&self.fields)).hash(state)
    }
}

impl Instance {
    pub fn new(class: Box<Class>) -> Instance {
        Instance {
            class: Rc::new(RefCell::new(class)),
            fields: Rc::new(RefCell::new(HashMap::new())),
        }
    }
    pub fn getter(&self, name: &Token) -> Result<Option<Object>, JokerError> {
        match self.fields.borrow().get(&name.lexeme) {
            Some(ins_value) => Ok(Some(ins_value.clone())),
            None => match self.class.borrow().get_field(&name.lexeme) {
                Some(cls_value) => match cls_value {
                    Some(value) => Ok(Some(value.clone())),
                    None => Err(JokerError::Interpreter(InterpreterError::report_error(
                        name,
                        format!(
                            "class attribute '{}' is declared, but not define.",
                            name.lexeme
                        ),
                    ))),
                },
                None => {
                    if let Some(method) = self.class.borrow().get_method(&name.lexeme) {
                        return Ok(Some(Object::new(method.bind(self.clone()).upcast_into())));
                    }
                    Ok(None)
                }
            },
        }
    }
    pub fn setter(&mut self, name: &str, value: Object) -> Result<(), JokerError> {
        self.fields.borrow_mut().insert(name.to_string(), value);
        Ok(())
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Instance(class: {}, fields: {:?})",
            self.class.borrow(),
            self.fields
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_instance_clone_modify() -> Result<(), JokerError> {
        let instance = Instance::new(Box::new(Class::new(
            String::from("Demo"),
            None,
            None,
            None,
            None,
        )));
        println!("instance: {:#?}", instance);
        let mut clone_instance = instance.clone();
        clone_instance.setter(
            "test",
            Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100))),
        )?;
        println!("clone_instance: {:#?}", clone_instance); // 100
        println!("instance: {:#?}", instance); // 100
        println!("clone_instance: {:#?}", clone_instance); // 100

        Ok(())
    }
}
