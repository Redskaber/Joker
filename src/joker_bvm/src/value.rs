//! This file is Joker-language rs
//!
//! 
//! 
//! 

use std::fmt::Display;


#[derive(Debug)]
#[repr(align(1))]
pub enum Value {
    // I32(i32),
    // F64(f64),
    Constant(usize),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Value::I32(value) => write!(f, "{}", value),
            // Value::F64(value) => write!(f, "{}", value), 
            Value::Constant(value) => write!(f, "{}", value), 
        }
    }
}




