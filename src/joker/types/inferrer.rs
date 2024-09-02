//! This mod is joker type infer rs
//!
//!
//!

use crate::joker::{
    ast::{
        Assign, Binary, Call, Expr, Grouping, Lambda, Literal, Logical, Trinomial, Unary, Variable,
    },
    callable::StructError,
    error::JokerError,
    object::{Caller, Function, Literal as ObL, Object as OEnum},
    parse::Parser,
    resolver::{Resolver, ResolverError},
    token::{Token, TokenType},
};

use super::type_::{ParamPair, Type};

pub struct TypeInferrer;

impl TypeInferrer {
    pub fn param_pair_type(param_pair: &ParamPair) -> Type {
        match param_pair {
            ParamPair::Normal { param: _, type_ } => type_.clone(),
            ParamPair::Label { type_ } => type_.clone(),
            ParamPair::This { param: _, type_ } => type_.clone(),
        }
    }
    // parse time:
    pub fn parse_type(parser: &mut Parser) -> Result<Type, JokerError> {
        let type_name: Token = parser.consume(
            &[TokenType::Identifier],
            String::from("Expect type. but this not is."),
        )?;
        println!("type_name: {type_name:?}");
        match type_name.lexeme.as_str() {
            "i32" => Ok(Type::I32),
            "f64" => Ok(Type::F64),
            "str" => Ok(Type::Str),
            "bool" => Ok(Type::Bool),
            "null" => Ok(Type::Null),
            "Fn" => {
                let params: Option<Vec<ParamPair>> = if parser.is_match(&[TokenType::LeftParen]) {
                    let mut params: Vec<ParamPair> = vec![ParamPair::label_with_parse(parser)?];
                    while parser.is_match(&[TokenType::Comma]) {
                        params.push(ParamPair::label_with_parse(parser)?);
                    }
                    Some(params)
                } else {
                    None
                };
                parser.consume(
                    &[TokenType::RightParen],
                    String::from("Expect ')' after label parameters."),
                )?;

                let return_type = if parser.is_match(&[TokenType::Arrow]) {
                    Some(Box::new(TypeInferrer::parse_type(parser)?))
                } else {
                    None
                };

                Ok(Type::Fn {
                    params,
                    return_type,
                })
            }
            // parse don't parse, move to static resolve parse.
            _ => Ok(Type::UserDefined(type_name)),
        }
    }
    // resolve time:
    pub fn infer_type(resolver: &Resolver, expr: &Expr) -> Result<Type, JokerError> {
        // println!("expr:\t{:?}", expr);
        match expr {
            Expr::Literal(Literal { value }) => match value {
                OEnum::Literal(ObL::I32(_)) => Ok(Type::I32),
                OEnum::Literal(ObL::F64(_)) => Ok(Type::F64),
                OEnum::Literal(ObL::Bool(_)) => Ok(Type::Bool),
                OEnum::Literal(ObL::Str(_)) => Ok(Type::Str),
                OEnum::Literal(ObL::Null) => Ok(Type::Null),
                OEnum::Caller(Caller::Func(Function::Native(_))) => Ok(Type::Fn {
                    params: None,
                    return_type: None,
                }),
                OEnum::Caller(Caller::Func(Function::User(user))) => Ok(Type::Fn {
                    params: user.stmt.params.clone(),
                    return_type: user.stmt.return_type.clone(),
                }),
                OEnum::Caller(Caller::Func(Function::Method(method))) => Ok(Type::Fn {
                    params: method.stmt.params.clone(),
                    return_type: method.stmt.return_type.clone(),
                }),
                OEnum::Caller(Caller::Lambda(lambda)) => Ok(Type::Fn {
                    params: lambda.expr.params.clone(),
                    return_type: lambda.expr.return_type.clone(),
                }),
                OEnum::Caller(Caller::Class(class)) => Ok(Type::Class {
                    name: class.name.clone(),
                }),
                OEnum::Instance(instance) => Ok(Type::Instance {
                    name: instance.class.borrow().name.clone(),
                }),
            },
            Expr::Unary(Unary { l_opera, r_expr }) => {
                let right_type: Type = TypeInferrer::infer_type(resolver, r_expr)?;
                if matches!(right_type, Type::I32 | Type::F64) {
                    Ok(right_type)
                } else {
                    Err(JokerError::Resolver(ResolverError::Struct(
                        StructError::report_error(
                            l_opera,
                            format!(
                                "Type mismatch in unary expression, '{}' don't impl operator '{}'.",
                                right_type, l_opera.lexeme
                            ),
                        ),
                    )))
                }
            }
            Expr::Binary(Binary {
                l_expr,
                m_opera,
                r_expr,
            }) => {
                let left_type: Type = TypeInferrer::infer_type(resolver, l_expr)?;
                let right_type: Type = TypeInferrer::infer_type(resolver, r_expr)?;
                if left_type.eq_type(&right_type) {
                    Ok(left_type)
                } else {
                    Err(JokerError::Resolver(ResolverError::Struct(
                        StructError::report_error(
                            m_opera,
                            format!("Type mismatch in binary expression, left type '{}' and right type '{}'",
                                left_type,
                                right_type
                            ),
                        ),
                    )))
                }
            }
            Expr::Grouping(Grouping { expr }) => TypeInferrer::infer_type(resolver, expr),
            Expr::Assign(Assign { name, value: _ }) => {
                Err(JokerError::Resolver(ResolverError::Struct(
                    StructError::report_error(name, String::from("Assign don't inferrer type.")),
                )))
            }
            Expr::Logical(Logical {
                l_expr,
                m_opera,
                r_expr,
            }) => {
                let left_type: Type = TypeInferrer::infer_type(resolver, l_expr)?;
                let right_type: Type = TypeInferrer::infer_type(resolver, r_expr)?;
                if left_type.eq_type(&right_type) {
                    Ok(left_type)
                } else {
                    Err(JokerError::Resolver(ResolverError::Struct(
                        StructError::report_error(
                            m_opera,
                            format!("Type mismatch in logical expression, left type '{}' and right type '{}'",
                                left_type,
                                right_type
                            ),
                        ),
                    )))
                }
            }
            Expr::Trinomial(Trinomial {
                condition,
                l_expr,
                r_expr,
            }) => {
                let condition: Type = TypeInferrer::infer_type(resolver, condition)?;
                if condition.eq_type(&Type::Bool) {
                    let left_type: Type = TypeInferrer::infer_type(resolver, l_expr)?;
                    let right_type: Type = TypeInferrer::infer_type(resolver, r_expr)?;
                    if left_type.eq_type(&right_type) {
                        Ok(left_type)
                    } else {
                        Err(JokerError::Resolver(ResolverError::Struct(StructError::report_error(
                            &Token::eof(0),
                            format!("Type mismatch in binary expression, left type '{}' and right type '{}'",
                                left_type,
                                right_type,
                            ),
                        ))))
                    }
                } else {
                    Err(JokerError::Resolver(ResolverError::Struct(
                        StructError::report_error(
                            &Token::eof(0),
                            String::from(
                                "Type mismatch in trinomial expression, but condition don't Bool.",
                            ),
                        ),
                    )))
                }
            }
            Expr::Lambda(Lambda {
                pipe: _,
                params,
                return_type,
                body: _,
            }) => Ok(Type::Fn {
                params: params.clone(),
                return_type: return_type.clone(),
            }),
            Expr::Call(Call {
                callee,
                paren: _,
                arguments: _,
            }) => TypeInferrer::infer_type(resolver, callee),
            Expr::Variable(Variable { name }) => resolver.get_type(name),
            _ => Err(JokerError::Resolver(ResolverError::Struct(
                StructError::report_error(
                    &Token::eof(0),
                    String::from("Unsupported type inference"),
                ),
            ))),
        }
    }
}
