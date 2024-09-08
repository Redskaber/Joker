//! This mod is joker type infer rs
//!
//!
//!

use std::collections::HashMap;

use crate::joker::{
    ast::{
        Assign, Binary, Call, ClassStmt, Expr, Getter, Grouping, Lambda, Literal, Logical, Stmt,
        Super, Trinomial, Unary, Variable,
    },
    callable::StructError,
    error::JokerError,
    object::{literal_null, Caller, Function, Literal as ObL, Object as OEnum},
    parse::Parser,
    resolver::{Resolver, ResolverError},
    token::{Token, TokenType},
};

use super::{
    type_::{ParamPair, Type},
    DeepClone, IsInstance,
};

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
            String::from("[TypeInferrer::parse_type] Expect type. but this not is."),
        )?;
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
                    String::from("[TypeInferrer::parse_type] Expect ')' after label parameters."),
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
                OEnum::Caller(Caller::Class(class)) => {
                    let name: Token = class.name.clone();
                    let super_class: Option<Box<Type>> =
                        if let Some(super_class) = class.super_class.as_ref() {
                            Some(Box::new(TypeInferrer::infer_type(
                                resolver,
                                &Expr::Literal(Literal {
                                    value: OEnum::Caller(Caller::Class(super_class.clone())),
                                }),
                            )?))
                        } else {
                            None
                        };
                    let fields: Option<HashMap<String, Type>> =
                        if let Some(fields) = class.fields.as_ref() {
                            let mut fields_type: HashMap<String, Type> = HashMap::new();
                            for (field, value) in fields {
                                let value_type = if let Some(value) = value {
                                    TypeInferrer::infer_type(
                                        resolver,
                                        &Expr::Literal(Literal {
                                            value: value.get().clone(),
                                        }),
                                    )?
                                } else {
                                    resolver.get_type(&Token::new(
                                        TokenType::Identifier,
                                        field.clone(),
                                        literal_null(),
                                        0,
                                    ))?
                                };
                                fields_type.insert(field.clone(), value_type);
                            }
                            Some(fields_type)
                        } else {
                            None
                        };

                    let methods: Option<HashMap<String, Type>> =
                        if let Some(methods) = class.methods.as_ref() {
                            let mut methods_type: HashMap<String, Type> = HashMap::new();
                            for (method, value) in methods {
                                let value_type: Type = TypeInferrer::infer_type(
                                    resolver,
                                    &Expr::Literal(Literal {
                                        value: OEnum::Caller(Caller::Func(Function::Method(
                                            value.clone(),
                                        ))),
                                    }),
                                )?;

                                methods_type.insert(method.clone(), value_type);
                            }
                            Some(methods_type)
                        } else {
                            None
                        };

                    let functions: Option<HashMap<String, Type>> =
                        if let Some(functions) = class.functions.as_ref() {
                            let mut functions_type: HashMap<String, Type> = HashMap::new();
                            for (func, value) in functions {
                                let value_type: Type = TypeInferrer::infer_type(
                                    resolver,
                                    &Expr::Literal(Literal {
                                        value: OEnum::Caller(Caller::Func(Function::User(
                                            value.clone(),
                                        ))),
                                    }),
                                )?;

                                functions_type.insert(func.clone(), value_type);
                            }
                            Some(functions_type)
                        } else {
                            None
                        };

                    Ok(Type::Class {
                        name,
                        super_class,
                        fields,
                        methods,
                        functions,
                    })
                }
                OEnum::Instance(instance) => {
                    let class: Box<Type> = Box::new(TypeInferrer::infer_type(
                        resolver,
                        &Expr::Literal(Literal {
                            value: OEnum::Caller(Caller::Class(
                                instance.class.borrow().deep_clone(),
                            )),
                        }),
                    )?);

                    let fields: Option<HashMap<String, Type>> = {
                        let mut fields: HashMap<String, Type> = HashMap::new();
                        for (key, value) in instance.fields.borrow().iter() {
                            let value_type = TypeInferrer::infer_type(
                                resolver,
                                &Expr::Literal(Literal {
                                    value: value.get().clone(),
                                }),
                            )?;
                            fields.insert(key.clone(), value_type);
                        }
                        Some(fields)
                    };

                    Ok(Type::Instance { class, fields })
                }
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
                                "[TypeInferrer::infer_type] Type mismatch in unary expression, '{}' don't impl operator '{}'.",
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
                            format!("[TypeInferrer::infer_type] Type mismatch in binary expression, left type '{}' and right type '{}'",
                                left_type,
                                right_type
                            ),
                        ),
                    )))
                }
            }
            Expr::Grouping(Grouping { expr }) => TypeInferrer::infer_type(resolver, expr),
            Expr::Assign(Assign { name, value: _ }) => Err(JokerError::Resolver(
                ResolverError::Struct(StructError::report_error(
                    name,
                    String::from("[TypeInferrer::infer_type] Assign don't inferrer type."),
                )),
            )),
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
                            format!("[TypeInferrer::infer_type] Type mismatch in logical expression, left type '{}' and right type '{}'",
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
                            format!("[TypeInferrer::infer_type] Type mismatch in binary expression, left type '{}' and right type '{}'",
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
                                "[TypeInferrer::infer_type] Type mismatch in trinomial expression, but condition don't Bool.",
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
            Expr::Getter(Getter { expr, name }) => {
                // var instance: class = class();
                // class.callable();
                // instance.callable();
                // through Getter expr type, judgement who handle getter name type, and return type.
                // inner type need translate: UserDefined to base type.
                let caller_type: Type = TypeInferrer::infer_type(resolver, expr)?;
                let caller_type: Type = if let Type::UserDefined(token) = caller_type {
                    TypeInferrer::infer_type(resolver, &Expr::Variable(Variable { name: token }))?
                } else {
                    caller_type
                };

                match caller_type {
                    Type::Class {
                        name: _,
                        super_class: _,
                        fields: _,
                        methods: _,
                        functions: _,
                    } => {
                        if let Some(sub_type) = caller_type.get_type(name)? {
                            Ok(sub_type.clone())
                        } else {
                            Err(JokerError::Resolver(ResolverError::Struct(
                                StructError::report_error(
                                    name,
                                    format!(
                                        "[TypeInferrer::infer_type] Expected getter class type, found get '{}'.",
                                        name.lexeme
                                    ),
                                ),
                            )))
                        }
                    }
                    Type::Instance {
                        class: _,
                        fields: _,
                    } => {
                        if let Some(sub_type) = caller_type.get_type(name)? {
                            Ok(sub_type.clone())
                        } else {
                            Err(JokerError::Resolver(ResolverError::Struct(
                                StructError::report_error(
                                    name,
                                    format!(
                                        "[TypeInferrer::infer_type] Expected getter instance type, found get '{}'.",
                                        name.lexeme
                                    ),
                                ),
                            )))
                        }
                    }
                    _ => Err(JokerError::Resolver(ResolverError::Struct(
                        StructError::report_error(
                            name,
                            String::from("[TypeInferrer::infer_type] Expected getter need left variable is impl getter."),
                        ),
                    ))),
                }
            }
            Expr::Super(Super { keyword, method }) => {
                let super_type: Type = resolver.get_type(keyword)?;
                if super_type.is_class() {
                    if let Some(method_type) = IsInstance::get_type(&super_type, method)? {
                        Ok(method_type.clone())
                    } else {
                        Err(JokerError::Resolver(ResolverError::Struct(
                            StructError::report_error(
                                keyword,
                                format!(
                                    "[TypeInferrer::infer_type] This keyword super: '{}', not getter method: '{}'.",
                                    super_type, method
                                ),
                            ),
                        )))
                    }
                } else {
                    Err(JokerError::Resolver(ResolverError::Struct(
                        StructError::report_error(
                            keyword,
                            format!("[TypeInferrer::infer_type] This keyword is not super class object. '{}'", super_type),
                        ),
                    )))
                }
            }
            _ => Err(JokerError::Resolver(ResolverError::Struct(
                StructError::report_error(
                    &Token::eof(0),
                    format!(
                        "[TypeInferrer::infer_type] Unsupported type inference: '{}'.",
                        expr
                    ),
                ),
            ))),
        }
    }
    pub fn infer_class_stmt(resolver: &Resolver, stmt: &ClassStmt) -> Result<Type, JokerError> {
        let name: Token = stmt.name.clone();
        let super_class: Option<Box<Type>> = if let Some(super_class) = stmt.super_class.as_ref() {
            Some(Box::new(TypeInferrer::infer_type(resolver, super_class)?))
        } else {
            None
        };

        let fields: Option<HashMap<String, Type>> = if let Some(fields) = stmt.fields.as_ref() {
            let mut fields_type: HashMap<String, Type> = HashMap::new();
            for stmt in fields {
                if let Stmt::VarStmt(var_stmt) = stmt {
                    let value_type: Type = if let Some(value) = var_stmt.value.as_ref() {
                        TypeInferrer::infer_type(resolver, value)?
                    } else if let Some(declared_type) = var_stmt.type_.as_ref() {
                        declared_type.clone()
                    } else {
                        return Err(JokerError::Resolver(ResolverError::Struct(
                            StructError::report_error(
                                &var_stmt.name,
                                format!(
                                    "class fields variable need some type, but this '{}' not.",
                                    var_stmt.name.lexeme
                                ),
                            ),
                        )));
                    };
                    fields_type.insert(var_stmt.name.lexeme.clone(), value_type);
                } else {
                    unreachable!("[TypeInferrer::infer_class_stmt]: unreachable this arm.")
                }
            }

            Some(fields_type)
        } else {
            None
        };

        let methods: Option<HashMap<String, Type>> = if let Some(methods) = stmt.methods.as_ref() {
            let mut methods_type: HashMap<String, Type> = HashMap::new();
            for stmt in methods {
                if let Stmt::FnStmt(fn_stmt) = stmt {
                    let value_type: Type = Type::Fn {
                        params: fn_stmt.params.clone(),
                        return_type: fn_stmt.return_type.clone(),
                    };
                    methods_type.insert(fn_stmt.name.lexeme.clone(), value_type);
                } else {
                    unreachable!("[TypeInferrer::infer_class_stmt]: unreachable this arm.")
                }
            }
            Some(methods_type)
        } else {
            None
        };

        let functions: Option<HashMap<String, Type>> =
            if let Some(functions) = stmt.functions.as_ref() {
                let mut functions_type: HashMap<String, Type> = HashMap::new();
                for stmt in functions {
                    if let Stmt::FnStmt(fn_stmt) = stmt {
                        let value_type: Type = Type::Fn {
                            params: fn_stmt.params.clone(),
                            return_type: fn_stmt.return_type.clone(),
                        };
                        functions_type.insert(fn_stmt.name.lexeme.clone(), value_type);
                    } else {
                        unreachable!("[TypeInferrer::infer_class_stmt]: unreachable this arm.")
                    }
                }
                Some(functions_type)
            } else {
                None
            };

        Ok(Type::Class {
            name,
            super_class,
            fields,
            methods,
            functions,
        })
    }
}
