//! This file is joker types rs
//!
//!
//!

use crate::joker::object::Object as OEnum;
use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

#[derive(Clone, PartialEq, Eq)]
pub struct Object {
    inner: Rc<RefCell<OEnum>>,
}

impl Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.inner.borrow().hash(state)
    }
}

impl Object {
    pub fn new(inner: OEnum) -> Self {
        Object {
            inner: Rc::new(RefCell::new(inner)),
        }
    }
    pub fn set(&self, inner: OEnum) {
        *self.inner.borrow_mut() = inner;
    }
    pub fn get(&self) -> Ref<OEnum> {
        self.inner.borrow()
    }
    pub fn get_mut(&self) -> RefMut<OEnum> {
        self.inner.borrow_mut()
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner.borrow(), f)
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Object(inner: {})", self.inner.borrow())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_object_inner_change() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!("Object: {:#?}", obj);
        *obj.inner.borrow_mut() = OEnum::Literal(crate::joker::object::Literal::Bool(false));
        println!("Object: {:#?}", obj);
        obj.set(OEnum::Literal(crate::joker::object::Literal::Bool(true)));
        println!("Object: {:#?}", obj);

        let en: Ref<OEnum> = obj.get();
        fn p(en: &OEnum) {
            println!("OEnum:{}", en);
        }
        p(&en);
        // *obj.get_mut() = OEnum::Literal(super::super::object::Literal::Null);
        // println!("Object: {:#?}", obj);
    }
}
