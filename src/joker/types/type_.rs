//! This file is Joker language in Rust impl rs
//!
//! - PS:
//!     Hash Trait: hash value is in depend, not sequence.
//!     so. hash HashMap cant through Hash.
//!
//!

use std::{collections::HashMap, fmt::Display, hash::Hash};

use crate::joker::{
    callable::StructError,
    error::JokerError,
    parse::Parser,
    resolver::Error::Struct,
    token::{Token, TokenType},
};

use super::TypeInferrer;

pub trait IsInstance {
    type Err;
    fn is_instance(&self, parent: &Self) -> Result<bool, Self::Err>;
    fn is_inherit(&self, parent: &Self) -> Result<bool, JokerError>;
    fn contains_key(&self, name: &Token) -> Result<bool, Self::Err>;
    fn get_type(&self, name: &Token) -> Result<Option<&Type>, Self::Err>;
}

// TODO: ADD Type more info? position, ...
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I32,
    F64,
    Str,
    Bool,
    Null, // Null is empty(empty value is value), None is not have anything(not value).
    Fn {
        params: Option<Vec<ParamPair>>,
        return_type: Option<Box<Type>>,
    },
    Class {
        name: Token,
        super_class: Option<Box<Type>>,
        fields: Option<HashMap<String, Type>>,
        methods: Option<HashMap<String, Type>>,
        functions: Option<HashMap<String, Type>>,
    },
    Instance {
        class: Box<Type>,
        fields: Option<HashMap<String, Type>>,
        methods: Option<HashMap<String, Type>>,
    },
    This(Box<Type>),
    UserDefined(Token),
}

impl Type {
    pub fn is_fn(&self) -> bool {
        matches!(
            self,
            Type::Fn {
                params: _,
                return_type: _
            }
        )
    }
    pub fn is_this(&self) -> bool {
        matches!(self, Type::This(_))
    }
    pub fn is_class(&self) -> bool {
        matches!(
            self,
            Type::Class {
                name: _,
                super_class: _,
                fields: _,
                methods: _,
                functions: _
            }
        )
    }
    pub fn is_instance(&self) -> bool {
        matches!(
            self,
            Type::Instance {
                class: _,
                methods: _,
                fields: _,
            }
        )
    }
    pub fn is_class_param(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Type::This(class),
                Type::Class {
                    name: _,
                    super_class: _,
                    fields: _,
                    methods: _,
                    functions: _,
                },
            ) => other.eq(class),
            _ => self.eq_type(other),
        }
    }
    pub fn eq_type(&self, other: &Self) -> bool {
        match (self, other) {
            (Type::I32, Type::I32) => true,
            (Type::F64, Type::F64) => true,
            (Type::Str, Type::Str) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Null, Type::Null) => true,
            (
                Type::Fn {
                    params: p1,
                    return_type: r1,
                },
                Type::Fn {
                    params: p2,
                    return_type: r2,
                },
            ) => {
                if p1.is_none() && p2.is_none() {
                    if r1.is_none() && r2.is_none() {
                        true
                    } else if r1.is_some() && r2.is_some() {
                        return r1.eq(r2);
                    } else {
                        return false;
                    }
                } else if p1.is_some() && p2.is_some() {
                    let ps1 = p1.as_ref().unwrap();
                    let ps2 = p2.as_ref().unwrap();

                    if ps1.len() != ps2.len() {
                        return false;
                    } else if ps2[0].get_type().is_class_param(ps1[0].get_type())
                        && ps1[1..]
                            .iter()
                            .zip(ps2[1..].iter())
                            .all(|(p1, p2)| p1.get_type().eq_type(p2.get_type()))
                    {
                        if r1.is_none() && r2.is_none() {
                            return true;
                        } else if r1.is_some() && r2.is_some() {
                            return r1.eq(r2);
                        } else {
                            return false;
                        }
                    } else {
                        false
                    }
                } else {
                    return false;
                }
            }
            (
                Type::Class {
                    name: n1,
                    super_class: sc1,
                    fields: f1,
                    methods: m1,
                    functions: fn1,
                },
                Type::Class {
                    name: n2,
                    super_class: sc2,
                    fields: f2,
                    methods: m2,
                    functions: fn2,
                },
            ) => {
                if n1 != n2 {
                    return false;
                }

                if sc1.is_none() && sc2.is_none() {
                    // Both super_class are None
                } else if sc1.is_some() && sc2.is_some() {
                    if !sc1.as_ref().unwrap().eq_type(sc2.as_ref().unwrap()) {
                        return false;
                    }
                } else {
                    return false;
                }

                if f1.is_none() && f2.is_none() {
                    // Both fields are None
                } else if f1.is_some() && f2.is_some() {
                    if f1.as_ref().unwrap() != f2.as_ref().unwrap() {
                        return false;
                    }
                } else {
                    return false;
                }

                if m1.is_none() && m2.is_none() {
                    // Both methods are None
                } else if m1.is_some() && m2.is_some() {
                    if m1.as_ref().unwrap() != m2.as_ref().unwrap() {
                        return false;
                    }
                } else {
                    return false;
                }

                if fn1.is_none() && fn2.is_none() {
                    // Both functions are None
                } else if fn1.is_some() && fn2.is_some() {
                    if fn1.as_ref().unwrap() != fn2.as_ref().unwrap() {
                        return false;
                    }
                } else {
                    return false;
                }

                true
            }
            (
                Type::Instance {
                    class: c1,
                    methods: _,
                    fields: _,
                },
                Type::Instance {
                    class: c2,
                    methods: _,
                    fields: _,
                },
            ) => c1.eq_type(c2),
            (Type::UserDefined(name1), Type::UserDefined(name2)) => name1 == name2,
            _ => false,
        }
    }
    pub fn as_ref(&self) -> &Self {
        self
    }
}

impl IsInstance for Type {
    type Err = JokerError;
    fn is_instance(&self, parent: &Self) -> Result<bool, Self::Err> {
        if let Type::Instance {
            class,
            fields: _,
            methods: _,
        } = self
        {
            if parent.is_class() {
                if class.eq_type(parent) {
                    Ok(true)
                } else {
                    IsInstance::is_inherit(class.as_ref(), parent)
                }
            } else {
                Err(JokerError::Resolver(Struct(StructError::report_error(
                    &Token::eof(0),
                    format!("[IsInstance::is_instance] Type mismatch: instance '{}', parent '{}'.\n\t\tCurrent type not's Class.",
                    self, parent,
                    )
                ))))
            }
        } else {
            Err(JokerError::Resolver(Struct(StructError::report_error(
                &Token::eof(0),
                format!("[IsInstance::is_instance] Type mismatch: left type '{}', right type '{}'.\n\t\tCurrent type not's Instance.",
                parent, self
                )
            ))))
        }
    }
    // TODO: INHERIT STORE? cache inherit info.
    fn is_inherit(&self, parent: &Self) -> Result<bool, JokerError> {
        if parent.is_class() {
            let mut current: &Type = self;
            while let Type::Class {
                name: _,
                super_class,
                fields: _,
                methods: _,
                functions: _,
            } = current
            {
                if let Some(super_class) = super_class {
                    if super_class.eq_type(parent) {
                        return Ok(true);
                    }
                    current = super_class.as_ref();
                } else {
                    return Ok(false);
                }
            }
            Err(JokerError::Resolver(Struct(StructError::report_error(
                &Token::eof(0),
                format!(
                    "[IsInstance::is_inherit] Type mismatch: Expected left Class type, found '{}'.",
                    self,
                ),
            ))))
        } else {
            Err(JokerError::Resolver(Struct(StructError::report_error(
                &Token::eof(0),
                format!("[IsInstance::is_inherit] Type mismatch: Expected right Class type, found '{}'.",
                parent,
                )
            ))))
        }
    }
    fn contains_key(&self, name: &Token) -> Result<bool, Self::Err> {
        match self {
            Type::Class {
                name: _,
                super_class,
                fields,
                methods,
                functions,
            } => {
                if let Some(fields) = fields {
                    if fields.contains_key(&name.lexeme) {
                        return Ok(true);
                    }
                }

                if let Some(methods) = methods {
                    if methods.contains_key(&name.lexeme) {
                        return Ok(true);
                    }
                }

                if let Some(functions) = functions {
                    if functions.contains_key(&name.lexeme) {
                        return Ok(true);
                    }
                }

                if let Some(super_class) = super_class {
                    if super_class.contains_key(name)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            Type::Instance {
                class,
                fields,
                methods: _,
            } => {
                if let Some(fields) = fields {
                    if fields.contains_key(&name.lexeme) {
                        return Ok(true);
                    }
                }
                if class.contains_key(name)? {
                    return Ok(true);
                }
                Ok(false)
            }
            _ => Err(JokerError::Resolver(Struct(StructError::report_error(
                name,
                format!(
                    "[IsInstance::contains_key] This type '{}' is not call.",
                    self
                ),
            )))),
        }
    }
    fn get_type(&self, name: &Token) -> Result<Option<&Type>, Self::Err> {
        if let Type::This(class_type) = self {
            return class_type.get_type(name);
        }

        match self {
            Type::Class {
                name: _,
                super_class,
                fields,
                methods,
                functions,
            } => {
                if let Some(fields) = fields {
                    if let Some(type_) = fields.get(&name.lexeme) {
                        return Ok(Some(type_))
                    }
                }

                if let Some(methods) = methods {
                    if let Some(type_) = methods.get(&name.lexeme) {
                        return Ok(Some(type_));
                    }
                }

                if let Some(functions) = functions {
                    if let Some(type_) = functions.get(&name.lexeme) {
                        return Ok(Some(type_));
                    }
                }

                if let Some(super_class) = super_class {
                    if let Some(type_) = super_class.get_type(name)? {
                        return Ok(Some(type_));
                    }
                }
                Ok(None)
            }
            Type::Instance { class, fields , methods } => {
                if let Some(fields) = fields {
                    if let Some(type_) = fields.get(&name.lexeme) {
                        return Ok(Some(type_));
                    }
                }

                if let Some(methods) = methods {
                    if let Some(type_) = methods.get(&name.lexeme) {
                        return Ok(Some(type_));
                    }
                }

                if let Some(type_) = class.get_type(name)? {
                    return Ok(Some(type_));
                }
                Ok(None)
            }
            _ => Err(JokerError::Resolver(Struct(
                StructError::report_error(
                    name,
                    format!(
                        "[IsInstance::contain_key] This type '{}' don't get type, need class type or instance type.",
                        self
                    ),
                ))))
        }
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Type::Null => 0.hash(state),
            Type::I32 => 1.hash(state),
            Type::F64 => 2.hash(state),
            Type::Str => 3.hash(state),
            Type::Bool => 4.hash(state),
            Type::Fn {
                params,
                return_type,
            } => {
                5.hash(state);
                if let Some(params) = params {
                    for pair in params {
                        pair.hash(state);
                    }
                }
                if let Some(return_type) = return_type {
                    return_type.hash(state);
                }
            }
            Type::Class {
                name,
                super_class,
                fields,
                methods,
                functions,
            } => {
                6.hash(state);
                name.hash(state);
                if let Some(super_class) = super_class {
                    super_class.hash(state);
                }
                if let Some(fields) = fields {
                    for (key, value) in fields {
                        key.hash(state);
                        value.hash(state);
                    }
                }
                if let Some(methods) = methods {
                    for (key, value) in methods {
                        key.hash(state);
                        value.hash(state);
                    }
                }
                if let Some(functions) = functions {
                    for (key, value) in functions {
                        key.hash(state);
                        value.hash(state);
                    }
                }
            }
            Type::Instance {
                class,
                fields,
                methods,
            } => {
                7.hash(state);
                class.hash(state);
                if let Some(fields) = fields {
                    for (key, value) in fields {
                        key.hash(state);
                        value.hash(state);
                    }
                }
                if let Some(methods) = methods {
                    for (key, value) in methods {
                        key.hash(state);
                        value.hash(state);
                    }
                }
            }
            Type::This(class) => {
                8.hash(state);
                class.hash(state);
            }
            Type::UserDefined(token) => {
                9.hash(state);
                token.hash(state);
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::F64 => write!(f, "f64"),
            Type::Str => write!(f, "str"),
            Type::Bool => write!(f, "bool"),
            Type::Null => write!(f, "null"),
            Type::Fn {
                params,
                return_type,
            } => {
                let params = match params {
                    Some(params) => params
                        .iter()
                        .map(|p| TypeInferrer::param_pair_type(p).to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                    None => String::new(),
                };
                match return_type {
                    Some(return_type) => write!(f, "Fn({}) -> {}", params, return_type),
                    None => write!(f, "Fn({})", params),
                }
            }
            Type::Class {
                name,
                super_class: _,
                fields: _,
                methods: _,
                functions: _,
            } => write!(f, "class({})", name.lexeme),
            Type::Instance {
                class,
                fields: _,
                methods: _,
            } => write!(f, "instance({})", class),
            Type::This(class) => write!(f, "{}", class),
            Type::UserDefined(name) => write!(f, "{}", name.lexeme),
        }
    }
}

impl FromParamPair for Type {
    type Err = JokerError;
    fn from_param_pair(param_pair: &ParamPair) -> Result<Self, Self::Err> {
        Ok(param_pair.get_type().clone())
    }
    fn from_param_pair_ref(param_pair: &ParamPair) -> Result<&Self, Self::Err> {
        Ok(param_pair.get_type())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParamPair {
    This { param: Token, type_: Type },
    Normal { param: Token, type_: Type },
    Label { type_: Type },
}

impl ParamPair {
    pub fn this(param: Token, type_: Type) -> ParamPair {
        ParamPair::This { param, type_ }
    }
    pub fn normal(param: Token, type_: Type) -> ParamPair {
        ParamPair::Normal { param, type_ }
    }
    pub fn label(type_: Type) -> ParamPair {
        ParamPair::Label { type_ }
    }
    pub fn is_this(&self) -> bool {
        matches!(self, ParamPair::This { param: _, type_: _ })
    }
    pub fn is_normal(&self) -> bool {
        matches!(self, ParamPair::Normal { param: _, type_: _ })
    }
    pub fn is_label(&self) -> bool {
        matches!(self, ParamPair::Label { type_: _ })
    }
    pub fn get_param(&self) -> Option<&Token> {
        match self {
            ParamPair::This { param, type_: _ } => Some(param),
            ParamPair::Normal { param, type_: _ } => Some(param),
            _ => None,
        }
    }
    pub fn get_type(&self) -> &Type {
        match self {
            ParamPair::Normal { param: _, type_ } => type_,
            ParamPair::Label { type_ } => type_,
            ParamPair::This { param: _, type_ } => type_,
        }
    }
    pub fn set_type(&mut self, new_type: Type) {
        match self {
            ParamPair::Normal { param: _, type_ } => *type_ = new_type,
            ParamPair::Label { type_ } => *type_ = new_type,
            ParamPair::This { param: _, type_ } => *type_ = new_type,
        }
    }
    pub fn parse<F: FromParamPair>(&self) -> Result<F, F::Err> {
        FromParamPair::from_param_pair(self)
    }
    pub fn parse_ref<F: FromParamPair>(&self) -> Result<&F, F::Err> {
        FromParamPair::from_param_pair_ref(self)
    }
    pub fn this_with_parse(parser: &mut Parser, class: &Token) -> Result<ParamPair, JokerError> {
        Ok(ParamPair::This {
            param: parser.consume(
                &[TokenType::This],
                String::from("Expect class method 'this' name."),
            )?,
            type_: Type::UserDefined(class.clone()),
        })
    }
    pub fn normal_with_parse(parser: &mut Parser) -> Result<ParamPair, JokerError> {
        let param: Token = parser.consume(
            &[TokenType::Identifier],
            String::from("Expect parameter name."),
        )?;
        parser.consume(
            &[TokenType::Colon],
            String::from("Expect ':' in parameter name after."),
        )?;
        let type_: Type = TypeInferrer::parse_type(parser)?;

        Ok(ParamPair::Normal { param, type_ })
    }
    pub fn label_with_parse(parser: &mut Parser) -> Result<ParamPair, JokerError> {
        let type_: Type = TypeInferrer::parse_type(parser)?;
        Ok(ParamPair::Label { type_ })
    }
    pub fn eq_type(&self, other: &Self) -> bool {
        self.get_type().eq_type(other.get_type())
    }
    pub fn as_ref(&self) -> &Self {
        self
    }
}

pub trait FromParamPair: Sized {
    type Err;
    fn from_param_pair(param_pair: &ParamPair) -> Result<Self, Self::Err>;
    fn from_param_pair_ref(param_pair: &ParamPair) -> Result<&Self, Self::Err>;
}
