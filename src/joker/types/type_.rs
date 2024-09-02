use std::fmt::Display;

use crate::joker::{
    error::JokerError,
    parse::Parser,
    token::{Token, TokenType},
};

use super::TypeInferrer;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F64,
    Str,
    Bool,
    Null,
    Fn {
        params: Option<Vec<ParamPair>>,
        return_type: Option<Box<Type>>,
    },
    Class {
        name: Token,
    },
    Instance {
        name: Token,
    },
    UserDefined(Token),
}

impl Type {
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
                    } else if ps1
                        .iter()
                        .zip(ps2)
                        .all(|(p1, p2)| p1.get_type().eq(p2.get_type()))
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
            (Type::Class { name: name1 }, Type::Class { name: name2 }) => name1 == name2,
            (Type::Instance { name: name1 }, Type::Instance { name: name2 }) => name1 == name2,
            (Type::UserDefined(name1), Type::UserDefined(name2)) => name1 == name2,
            _ => false,
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
            Type::Class { name } => write!(f, "class({})", name.lexeme),
            Type::Instance { name } => write!(f, "instance({})", name.lexeme),
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
    pub fn parse<F: FromParamPair>(&self) -> Result<F, F::Err> {
        FromParamPair::from_param_pair(self)
    }
    pub fn parse_ref<F: FromParamPair>(&self) -> Result<&F, F::Err> {
        FromParamPair::from_param_pair_ref(self)
    }
    pub fn this_with_parse(parser: &mut Parser) -> Result<ParamPair, JokerError> {
        Ok(ParamPair::This {
            param: parser.consume(
                &[TokenType::This],
                String::from("Expect class method 'this' name."),
            )?,
            type_: Type::Null,
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
    pub fn as_ref(&self) -> &Self {
        self
    }
}

pub trait FromParamPair: Sized {
    type Err;
    fn from_param_pair(param_pair: &ParamPair) -> Result<Self, Self::Err>;
    fn from_param_pair_ref(param_pair: &ParamPair) -> Result<&Self, Self::Err>;
}
