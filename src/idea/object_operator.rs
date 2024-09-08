//! This file is joker language in rust rs idea.
//! 
//! Object 
//!     Add
//!     Sub
//!     Mul
//!     ... 
//! 
//! 

use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Add;


#[derive(Debug, Clone)]
enum Inner {
    Int(i32),
    Str(String),
}

impl Add for Inner {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (Inner::Int(a), Inner::Int(b)) => Inner::Int(a + b),
            (Inner::Str(a), Inner::Str(b)) => Inner::Str(a + &b),
            _ => panic!("Incompatible types for addition"),
        }
    }
}

#[derive(Debug)]
struct Object {
    inner: Rc<RefCell<Inner>>
}

impl Object {
    pub fn add(&self, other: &Self) {
        let self_inner = self.inner.borrow().clone();
        let other_inner = other.inner.borrow().clone();

        let result = self_inner + other_inner;

        *self.inner.borrow_mut() = result;
    }
}

pub fn object_operator() {
    let obj: Object = Object { inner: Rc::new(RefCell::new(Inner::Int(100))) };
    println!("obj: {:#?}", obj);
    obj.add(&Object { inner: Rc::new(RefCell::new(Inner::Int(1000))) });
    println!("obj: {:#?}", obj);
}
