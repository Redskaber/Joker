//! This file is Literal rs
//!
//! - Literal
//!     - I32
//!     - F64
//!     - Str
//!     - Bool
//!     - Null
//!
//!
//!

use std::{
    fmt::Display,
    hash::{Hash, Hasher},
};

use super::{Object, UpCast};

#[derive(Debug, Clone)]
pub enum Literal {
    I32(i32),
    F64(f64),
    Str(String),
    Bool(bool),
    Null,
}

impl UpCast<Object> for Literal {
    fn upcast(&self) -> Object {
        match self {
            Literal::I32(value) => Object::Literal(Literal::I32(*value)),
            Literal::F64(value) => Object::Literal(Literal::F64(*value)),
            Literal::Str(string) => Object::Literal(Literal::Str(string.clone())),
            Literal::Bool(bool_) => Object::Literal(Literal::Bool(*bool_)),
            Literal::Null => Object::Literal(Literal::Null),
        }
    }
    fn upcast_into(self) -> Object {
        Object::Literal(self)
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::I32(a), Literal::I32(b)) => a == b,
            (Literal::F64(a), Literal::F64(b)) => a.eq(b) || (a.is_nan() && b.is_nan()), // 特殊处理 NaN
            (Literal::Str(a), Literal::Str(b)) => a == b,
            (Literal::Bool(a), Literal::Bool(b)) => a == b,
            (Literal::Null, Literal::Null) => true,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::I32(val) => val.hash(state),
            Literal::F64(val) => {
                // 对于 f64，使用 to_bits 来确保 NaN 有相同的哈希码
                // 并且正无穷和负无穷有不同的哈希码
                val.to_bits().hash(state);
            }
            Literal::Str(val) => val.hash(state),
            Literal::Bool(val) => val.hash(state),
            Literal::Null => 0.hash(state), // 或者选择一个固定的值作为 null 的哈希码
        }
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translator_object_from_literal() {
        let l_i32 = Literal::I32(100);
        let l_f64 = Literal::F64(10.0);
        let l_string = Literal::Str(String::from("literal_string"));
        let l_bool = Literal::Bool(true);
        let l_null = Literal::Null;

        let lr_i32 = &l_i32;
        let lr_f64 = &l_f64;
        let lr_string = &l_string;
        let lr_bool = &l_bool;
        let lr_null = &l_null;
        // copy clone
        assert_eq!(Object::Literal(Literal::I32(100)), lr_i32.upcast());
        assert_eq!(Object::Literal(Literal::F64(10.0)), lr_f64.upcast());
        assert_eq!(
            Object::Literal(Literal::Str(String::from("literal_string"))),
            lr_string.upcast()
        );
        assert_eq!(Object::Literal(Literal::Bool(true)), lr_bool.upcast());
        assert_eq!(Object::Literal(Literal::Null), lr_null.upcast());
        // move
        assert_eq!(Object::Literal(Literal::I32(100)), l_i32.upcast_into());
        assert_eq!(Object::Literal(Literal::F64(10.0)), l_f64.upcast_into());
        assert_eq!(
            Object::Literal(Literal::Str(String::from("literal_string"))),
            l_string.upcast_into()
        );
        assert_eq!(Object::Literal(Literal::Bool(true)), l_bool.upcast_into());
        assert_eq!(Object::Literal(Literal::Null), l_null.upcast_into());
    }
}
