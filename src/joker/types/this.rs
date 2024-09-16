use std::{
    cell::{Ref, RefCell, RefMut},
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

use crate::joker::object::{Caller, Object as OEnum};

pub trait DeepClone {
    fn deep_clone(&self) -> Self;
}
pub trait FromObject: Sized {
    type Err;
    fn from_object(obj: &Object) -> Result<Self, Self::Err>;
}

#[derive(Clone, PartialEq, Eq)]
pub struct Object {
    inner: Rc<RefCell<OEnum>>,
}

impl DeepClone for Object {
    fn deep_clone(&self) -> Self {
        Object {
            inner: Rc::new(RefCell::new((*self.inner.borrow()).deep_clone())),
        }
    }
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
    pub fn is_fn(&self) -> bool {
        matches!(&*self.get(), OEnum::Caller(Caller::Func(_)))
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
    pub fn parse<F: FromObject>(&self) -> Result<F, F::Err> {
        FromObject::from_object(self)
    }
    pub fn into_inner(self) -> OEnum {
        self.inner
            .replace(OEnum::Literal(crate::joker::object::Literal::Null))
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
        println!("Object: obj {:#?}", obj);
        *obj.inner.borrow_mut() = OEnum::Literal(crate::joker::object::Literal::Bool(false));
        println!("Object: obj {:#?}", obj);
        obj.set(OEnum::Literal(crate::joker::object::Literal::Bool(true)));
        println!("Object: obj {:#?}", obj);

        let en: Ref<OEnum> = obj.get();
        fn p(en: &OEnum) {
            println!("OEnum: en {}", en);
        }
        p(&en);
    }

    #[test]
    fn test_object_inner_ref() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 1
        let ref_obj = obj.clone(); // Rc::clone
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // 2
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 2
        assert_eq!(obj.inner, ref_obj.inner); // good
    }

    #[test]
    fn test_object_clone() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1

        let ref_obj = obj.clone(); // Rc::clone
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // 100 2
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 2

        ref_obj.set(OEnum::Literal(crate::joker::object::Literal::Bool(false)));
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // false 2
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // false 2
    }

    #[test]
    fn test_object_clone_ptr() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1

        let ref_obj = obj.clone(); // Rc::clone
        println!(
            "Object: ref_obj {:#?}, count: {}",
            ref_obj,
            Rc::strong_count(&ref_obj.inner)
        ); // 100 2

        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 2
        println!(
            "Object: std::ptr::addr_eq(obj, &ref_obj): {}",
            std::ptr::addr_eq(&obj, &ref_obj)
        ); // false
    }

    #[test]
    fn test_object_deep_clone() {
        let obj = Object::new(OEnum::Literal(crate::joker::object::Literal::I32(100)));
        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1

        let deep_obj = obj.deep_clone();
        println!(
            "Object: deep_obj {:#?}, count: {}",
            deep_obj,
            Rc::strong_count(&deep_obj.inner)
        ); // 100 1
        deep_obj.set(OEnum::Literal(crate::joker::object::Literal::I32(10000)));
        println!(
            "Object: deep_obj {:#?}, count: {}",
            deep_obj,
            Rc::strong_count(&deep_obj.inner)
        ); // 100 1

        println!(
            "Object: obj {:#?}, count: {}",
            obj,
            Rc::strong_count(&obj.inner)
        ); // 100 1
        println!(
            "Object: std::ptr::addr_eq(obj, &deep_obj): {}",
            std::ptr::addr_eq(&obj, &deep_obj)
        ); // false
    }
}
