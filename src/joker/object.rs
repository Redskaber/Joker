//! This file is type.rs
//!
//!
//!
use std::fmt::Display;


#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Literal(Literal),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Literal(literal) => Display::fmt(literal, f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    I32(i32),
    F64(f64),
    Str(String),
    Bool(bool),
    Null,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::I32(i32_) => write!(f, "{i32_}"),
            Literal::F64(f64_) => write!(f, "{f64_}"),
            Literal::Str(str_) => write!(f, "\"{str_}\""),
            Literal::Bool(bool_) => write!(f, "{bool_}"),
            Literal::Null => write!(f, "null"),
        }
    }
}

pub fn literal_str(text: String) -> Object {
    Object::Literal(Literal::Str(text))
}

pub fn literal_f64(f64_: f64) -> Object {
    Object::Literal(Literal::F64(f64_))
}

pub fn literal_i32(i32_: i32) -> Object {
    Object::Literal(Literal::I32(i32_))
}

pub fn literal_bool(bool_: bool) -> Object {
    Object::Literal(Literal::Bool(bool_))
}

pub fn literal_null() -> Object {
    Object::Literal(Literal::Null)
}
