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
    // find link: instance fields -> class fields -> super fields
    // -> class method -> class function -> super method -> super function
    pub fn getter(&self, name: &Token) -> Result<Option<Object>, JokerError> {
        if let Some(instance_value) = self.fields.borrow().get(&name.lexeme) {
            return Ok(Some(instance_value.clone()));
        }

        if let Some(class_value_status) = self.class.borrow().get_field(&name.lexeme) {
            if let Some(class_value) = class_value_status {
                return Ok(Some(class_value.clone()));
            } else {
                return Err(JokerError::Interpreter(InterpreterError::report_error(
                    name,
                    format!(
                        "class attribute '{}' is declared, but not define.",
                        name.lexeme
                    ),
                )));
            }
        }

        if let Some(super_class) = self.class.borrow().super_class.as_ref() {
            if let Some(super_value_status) = super_class.get_field(&name.lexeme) {
                if let Some(super_value) = super_value_status {
                    return Ok(Some(super_value.clone()));
                } else {
                    return Err(JokerError::Interpreter(InterpreterError::report_error(
                        name,
                        format!(
                            "super class attribute '{}' is declared, but not define.",
                            name.lexeme
                        ),
                    )));
                }
            }
        }

        if let Some(method) = self.class.borrow().get_method(&name.lexeme) {
            return Ok(Some(Object::new(method.bind(self.clone()).upcast_into())));
        }

        Ok(None)
    }
    // first find name: getter, if have modify else insert.
    pub fn setter(&mut self, name: &Token, value: Object) -> Result<(), JokerError> {
        if let Ok(op_obj) = self.getter(name) {
            match op_obj {
                Some(object) => object.set(value.get().clone()),
                None => {
                    self.fields.borrow_mut().insert(name.to_string(), value);
                }
            }
        } else {
            // class declared but not define.
            if let Some(class_fields) = self.class.borrow_mut().fields.as_mut() {
                if let Some(class_field) = class_fields.get_mut(&name.lexeme) {
                    if class_field.is_none() {
                        *class_field = Some(value);
                        return Ok(());
                    }
                }
            }
            // super class declared but not define.
            if let Some(super_class) = self.class.borrow_mut().super_class.as_mut() {
                if let Some(super_fields) = super_class.fields.as_mut() {
                    if let Some(super_field) = super_fields.get_mut(&name.lexeme) {
                        if super_field.is_none() {
                            *super_field = Some(value);
                            return Ok(());
                        }
                    }
                }
            }
        }
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
    use crate::joker::object::literal_null;

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
            &Token::new(
                crate::joker::token::TokenType::Identifier,
                String::from("test"),
                literal_null(),
                0,
            ),
            Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100))),
        )?;
        println!("clone_instance: {:#?}", clone_instance); // 100
        println!("instance: {:#?}", instance); // 100
        println!("clone_instance: {:#?}", clone_instance); // 100

        Ok(())
    }
}
