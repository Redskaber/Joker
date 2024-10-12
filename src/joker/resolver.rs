//! This file is joker resolver rs
//!
//! - Resolver
//!     This struct used static resolve all maybe stmt and expr. handle closure env wait question.  
//!
//!

use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    fmt::Display,
    hash::Hash,
    rc::Rc,
};

use super::{
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Call, ClassStmt, ContinueStmt, Expr, ExprAcceptor,
        ExprStmt, ExprVisitor, FnStmt, ForStmt, Getter, Grouping, IfStmt, Lambda, Literal, Logical,
        PrintStmt, ReturnStmt, Setter, Stmt, StmtAcceptor, StmtVisitor, Super, This, Trinomial,
        Unary, VarStmt, Variable, WhileStmt,
    },
    callable::StructError,
    env::EnvError,
    error::{JokerError, ReportError},
    interpreter::Interpreter,
    token::{Token, TokenType},
    types::{IsInstance, ParamPair, Type, TypeEnv, TypeInferrer},
};

pub trait StmtResolver<T> {
    fn resolve(&self, stmt: &Stmt) -> Result<T, JokerError>;
    fn resolve_block(&self, stmts: &[Stmt]) -> Result<T, JokerError>;
    fn resolve_function(&self, stmt: &FnStmt) -> Result<T, JokerError>;
    fn resolve_lambda(&self, pipe: &Token, stmt: &Stmt) -> Result<T, JokerError>;
    fn resolve_class(&self, stmt: &ClassStmt) -> Result<T, JokerError>;
    fn resolve_method(&self, stmt: &FnStmt) -> Result<T, JokerError>;
    fn resolve_var(&self, stmt: &VarStmt) -> Result<T, JokerError>;
}

pub trait ExprResolver<T> {
    fn resolve(&self, expr: &Expr) -> Result<T, JokerError>;
    fn resolve_local(&self, expr: Expr, name: &Token) -> Result<T, JokerError>;
    fn resolve_lambda(&self, expr: &Lambda) -> Result<T, JokerError>;
    fn resolve_call(&self, expr: &Call) -> Result<T, JokerError>;
}

#[derive(Debug, PartialEq, Eq)]
pub enum ContextStatus {
    Class(ClassStatus),
    Fn(ReturnType),
    Loop,
}

impl ContextStatus {
    pub fn is_fn(&self) -> bool {
        matches!(
            self,
            ContextStatus::Fn(_) | ContextStatus::Class(ClassStatus::Fn(_))
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClassStatus {
    Class,
    SuperClass,
    Method(ReturnType),
    Fn(ReturnType),
}

#[derive(Debug, Clone)]
pub enum ReturnType<T = Option<Box<Type>>> {
    Any,
    Specific(T),
}

impl PartialEq for ReturnType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ReturnType::Any, _) | (_, ReturnType::Any) => true,
            (ReturnType::Specific(s1), ReturnType::Specific(s2)) => s1.eq(s2),
        }
    }
}

impl Eq for ReturnType {}

impl ReturnType {
    pub fn eq_type(&self, other: &Type) -> bool {
        match self {
            ReturnType::Any => true,
            ReturnType::Specific(s) => s.as_ref().map_or(false, |t| t.eq_type(other)),
        }
    }
}

impl Display for ReturnType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnType::Any => write!(f, "Any"),
            ReturnType::Specific(spec) => match spec {
                Some(type_) => Display::fmt(type_, f),
                None => write!(f, "None"),
            },
        }
    }
}

#[derive(Debug)]
pub struct Key(pub Token);
impl Key {
    pub fn token(&self) -> &Token {
        &self.0
    }
}
impl Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (&self.0.ttype, &self.0.lexeme, &self.0.literal).hash(state)
    }
}

impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        self.0.ttype == other.0.ttype
            && self.0.lexeme == other.0.lexeme
            && self.0.literal == other.0.literal
    }
}

impl Eq for Key {}

#[derive(Debug, PartialEq, Eq)]
pub enum VarStatus {
    Declare,
    Define,
    Used,
}

pub struct Resolver {
    interpreter: Rc<Interpreter>,
    scopes_stack: RefCell<Vec<RefCell<HashMap<Key, VarStatus>>>>,
    context_status_stack: RefCell<Vec<ContextStatus>>,
    pub type_env: RefCell<TypeEnv>,
}

impl Resolver {
    pub fn new(interpreter: Rc<Interpreter>) -> Resolver {
        Resolver {
            interpreter,
            scopes_stack: RefCell::new(Vec::new()),
            context_status_stack: RefCell::new(Vec::new()),
            type_env: RefCell::new(TypeEnv::new_global()),
        }
    }
    pub fn resolve(&self, stmts: &[Stmt]) -> Result<(), JokerError> {
        self.resolve_block(stmts)?;
        Ok(())
    }
    fn begin_scope(&self) {
        self.scopes_stack
            .borrow_mut()
            .push(RefCell::new(HashMap::new()));
        self.type_env.borrow_mut().begin_scope();
    }
    fn end_scope(&self) {
        self.type_env.borrow_mut().end_scope();
        self.scopes_stack.borrow_mut().pop();
    }
    fn declare(&self, name: &Token) -> Result<(), JokerError> {
        if let Some(scope) = self.scopes_stack.borrow().last() {
            match scope.borrow_mut().entry(Key(name.clone())) {
                Entry::Occupied(_) => {
                    return Err(JokerError::Resolver(Error::Var(VarError::Redefine(
                        RedefineError::report_error(
                            name,
                            format!(
                                "Variable '{}' is already declared in this scope.",
                                name.lexeme
                            ),
                        ),
                    ))));
                }
                Entry::Vacant(entry) => {
                    entry.insert(VarStatus::Declare);
                    return Ok(());
                }
            }
        }
        Ok(())
    }
    fn define(&self, name: &Token) -> Result<(), JokerError> {
        if let Some(scope) = self.scopes_stack.borrow().last() {
            scope
                .borrow_mut()
                .insert(Key(name.clone()), VarStatus::Define);
        }
        Ok(())
    }
    fn used(&self, name: &Token) -> Result<(), JokerError> {
        if let Some(scope) = self.scopes_stack.borrow().last() {
            scope
                .borrow_mut()
                .insert(Key(name.clone()), VarStatus::Used);
        }
        Ok(())
    }
    fn check_var_status(&self, name: &Key, value_status: &VarStatus) -> Result<(), JokerError> {
        match value_status {
            VarStatus::Declare | VarStatus::Define => {
                let message = match value_status {
                    VarStatus::Declare => "Variable declared but not define",
                    VarStatus::Define => "Variable defined but not used",
                    _ => unreachable!(),
                };
                StatusError::report_error(name.token(), message.to_string());
            }
            VarStatus::Used => {}
        }
        Ok(())
    }
    fn check_vars_status(&self) -> Result<(), JokerError> {
        match self.scopes_stack.borrow().last() {
            Some(current_scope) => current_scope
                .borrow()
                .iter()
                .try_for_each(|(name, value_status)| self.check_var_status(name, value_status)),
            None => Err(JokerError::Resolver(Error::Env(EnvError::report_error(
                &Token::eof(0),
                String::from("No current environment to check variables"),
            )))),
        }
    }
    fn contains_any(&self, items: &[ContextStatus]) -> bool {
        items
            .iter()
            .any(|item| self.context_status_stack.borrow().contains(item))
    }
    fn last_previous_any(&self, items: &[ContextStatus]) -> bool {
        let stack = self.context_status_stack.borrow();
        if stack.len() < 2 {
            false
        } else {
            items.iter().any(|item| stack[stack.len() - 2].eq(item))
        }
    }
    fn last_any(&self, items: &[ContextStatus]) -> bool {
        let stack = self.context_status_stack.borrow();
        if stack.len() < 2 {
            items
                .iter()
                .any(|item| self.context_status_stack.borrow().first().eq(&Some(item)))
        } else {
            items
                .iter()
                .any(|item| self.context_status_stack.borrow().last().eq(&Some(item)))
        }
    }
    // wait type keyword
    // type name = expression;
    pub fn declare_type(&self, name: &Token, ty: Type) -> Result<(), JokerError> {
        self.type_env.borrow_mut().declare_type(name, ty)
    }
    pub fn get_type(&self, name: &Token) -> Result<Type, JokerError> {
        self.type_env.borrow().get_type(name)
    }
    pub fn assign_type(&self, name: &Token, ty: Type) -> Result<(), JokerError> {
        self.type_env.borrow_mut().assign_type(name, ty)
    }
    pub fn last_fn_return_type(&self) -> Option<ReturnType> {
        self.context_status_stack
            .borrow()
            .last()
            .and_then(|context| match context {
                ContextStatus::Fn(v)
                | ContextStatus::Class(ClassStatus::Fn(v))
                | ContextStatus::Class(ClassStatus::Method(v)) => Some(v.clone()),
                _ => None,
            })
    }
}

// Resolver
impl StmtResolver<()> for Resolver {
    fn resolve(&self, stmt: &Stmt) -> Result<(), JokerError> {
        stmt.accept(self)
    }
    fn resolve_block(&self, stmts: &[Stmt]) -> Result<(), JokerError> {
        for stmt in stmts {
            StmtResolver::resolve(self, stmt)?;
        }
        Ok(())
    }
    fn resolve_function(&self, stmt: &FnStmt) -> Result<(), JokerError> {
        if self.last_any(&[
            ContextStatus::Class(ClassStatus::Class),
            ContextStatus::Class(ClassStatus::SuperClass),
        ]) {
            self.context_status_stack
                .borrow_mut()
                .push(ContextStatus::Class(ClassStatus::Fn(ReturnType::Specific(
                    stmt.return_type.clone(),
                ))));
        } else {
            self.context_status_stack
                .borrow_mut()
                .push(ContextStatus::Fn(ReturnType::Specific(
                    stmt.return_type.clone(),
                )));
        }

        self.begin_scope();
        if let Some(tokens) = stmt.params.as_ref() {
            for param in tokens {
                if let ParamPair::Normal { param, type_ } = param {
                    // value check
                    self.declare(param)?;
                    self.define(param)?;
                    // type check
                    let type_: Type = if let Type::UserDefined(token) = type_ {
                        TypeInferrer::infer_type(
                            self,
                            &Expr::Variable(Variable {
                                name: token.clone(),
                            }),
                        )?
                    } else {
                        type_.clone()
                    };
                    self.declare_type(param, type_)?;
                } else {
                    return Err(JokerError::Resolver(Error::Struct(
                        StructError::report_error(
                            &stmt.name,
                            String::from("Invalid parameter type"),
                        ),
                    )));
                }
            }
        }
        StmtResolver::resolve_block(self, &stmt.body)?;

        // check local var used status
        self.check_vars_status()?;
        self.end_scope();

        self.context_status_stack.borrow_mut().pop();
        Ok(())
    }
    fn resolve_lambda(&self, pipe: &Token, stmt: &Stmt) -> Result<(), JokerError> {
        match stmt {
            Stmt::BlockStmt(block) => StmtResolver::resolve_block(self, &block.stmts),
            Stmt::ExprStmt(expr) => ExprResolver::resolve(self, &expr.expr),
            _ => Err(JokerError::Resolver(Error::Struct(
                StructError::report_error(
                    pipe,
                    String::from("lambda structure: | params | expr '1' or block '{}'."),
                ),
            ))),
        }
    }
    fn resolve_class(&self, stmt: &ClassStmt) -> Result<(), JokerError> {
        let exist_super: bool = if let Some(super_expr) = stmt.super_class.as_ref() {
            if let Expr::Variable(super_variable) = super_expr {
                if super_variable.name.lexeme.eq(&stmt.name.lexeme) {
                    return Err(JokerError::Resolver(Error::Env(EnvError::report_error(
                        &stmt.name,
                        String::from("a class can't inherit from itself."),
                    ))));
                }
            }
            ExprResolver::resolve(self, super_expr)?;
            // local -> this(instance) -> >super class< -> global
            self.begin_scope();
            // value check
            self.declare(&Token::super_(stmt.name.line))?;
            self.define(&Token::super_(stmt.name.line))?;
            self.used(&Token::super_(stmt.name.line))?;
            // type check
            let super_type: Type = TypeInferrer::infer_type(self, super_expr)?;
            self.declare_type(&Token::super_(stmt.name.line), super_type)?;

            true
        } else {
            false
        };

        self.context_status_stack.borrow_mut().push(if exist_super {
            ContextStatus::Class(ClassStatus::SuperClass)
        } else {
            ContextStatus::Class(ClassStatus::Class)
        });

        // this(instance env)
        self.begin_scope();
        // type check
        // used This && class name do Type name.
        let class_type: Type = TypeInferrer::infer_class_stmt(self, stmt)?;
        self.declare_type(&stmt.name, Type::This(Box::new(class_type)))?;

        // TODO: add This Type ?
        // self.declare_type(
        //     &Token::this_type(stmt.name.line),
        //     Type::This(Box::new(class_type)),
        // )?;

        if let Some(stmts) = stmt.fields.as_ref() {
            for stmt in stmts {
                StmtResolver::resolve(self, stmt)?;
            }
        }
        if let Some(stmts) = stmt.methods.as_ref() {
            // value check
            self.declare(&Token::this(stmt.name.line))?;
            self.define(&Token::this(stmt.name.line))?;
            self.used(&Token::this(stmt.name.line))?;
            // type check
            self.declare_type(
                &Token::this(stmt.name.line),
                Type::Instance {
                    class: Box::new(self.get_type(&stmt.name)?),
                    methods: None, // dynamic function store
                    fields: None,  // dynamic fields store
                },
            )?;
            for stmt in stmts {
                if let Stmt::FnStmt(func) = stmt {
                    StmtResolver::resolve_method(self, func)?;
                }
            }
        }
        if let Some(stmts) = stmt.functions.as_ref() {
            for stmt in stmts {
                if let Stmt::FnStmt(func) = stmt {
                    StmtResolver::resolve_function(self, func)?;
                }
            }
        }
        // check local var used status
        self.check_vars_status()?;
        self.end_scope();
        // super
        if stmt.super_class.is_some() {
            self.end_scope();
        }

        self.context_status_stack.borrow_mut().pop();
        Ok(())
    }
    fn resolve_method(&self, stmt: &FnStmt) -> Result<(), JokerError> {
        if self.contains_any(&[
            ContextStatus::Class(ClassStatus::Class),
            ContextStatus::Class(ClassStatus::SuperClass),
        ]) {
            self.context_status_stack
                .borrow_mut()
                .push(ContextStatus::Class(ClassStatus::Method(
                    ReturnType::Specific(stmt.return_type.clone()),
                )));

            self.begin_scope();
            if let Some(params) = stmt.params.as_ref() {
                // resolve fun init(this, ...) {...}
                // this 'this' can shadow outer this.
                if !params[0].is_this() {
                    return Err(JokerError::Resolver(Error::Struct(
                        StructError::report_error(
                            &stmt.name,
                            format!(
                                "class method first 'this', but '{}' not is.",
                                stmt.name.lexeme
                            ),
                        ),
                    )));
                }
                for param in params[1..].iter() {
                    if let ParamPair::Normal { param, type_ } = param {
                        // value check
                        self.declare(param)?;
                        self.define(param)?;
                        // type check
                        let type_: Type = if let Type::UserDefined(token) = type_ {
                            TypeInferrer::infer_type(
                                self,
                                &Expr::Variable(Variable {
                                    name: token.clone(),
                                }),
                            )?
                        } else {
                            type_.clone()
                        };
                        self.declare_type(param, type_)?;
                    } else {
                        return Err(JokerError::Resolver(Error::Struct(
                            StructError::report_error(
                                &stmt.name,
                                String::from("Invalid parameter type."),
                            ),
                        )));
                    }
                }
            }
            StmtResolver::resolve_block(self, &stmt.body)?;

            // check local var used status
            self.check_vars_status()?;
            self.end_scope();

            self.context_status_stack.borrow_mut().pop();
            Ok(())
        } else {
            Err(JokerError::Resolver(Error::Env(EnvError::report_error(
                &stmt.name,
                String::from("class instance method is need inside class."),
            ))))
        }
    }
    // var a: declared_type = value;
    // var b: declared_type = caller.value;
    // var c: declared_type = caller.callable();
    fn resolve_var(&self, stmt: &VarStmt) -> Result<(), JokerError> {
        let value_type: Option<Type> = if let Some(expr) = stmt.value.as_ref() {
            if let Expr::Call(call) = expr {
                let type_: Type = TypeInferrer::infer_type(self, &call.callee)?;
                match type_ {
                    // function return type
                    Type::Fn {
                        params: _,
                        return_type,
                    } => return_type.map(|return_type| *return_type),
                    // class return instance base, dynamic (methods && fields => {None})
                    Type::Class {
                        name: _,
                        super_class: _,
                        fields: _,
                        methods: _,
                        functions: _,
                    } => Some(Type::Instance {
                        class: Box::new(type_),
                        methods: None,
                        fields: None,
                    }),
                    _ => Some(type_),
                }
            } else {
                Some(TypeInferrer::infer_type(self, expr)?)
            }
        } else {
            None
        };

        let declared_type: Option<Type> = if let Some(declared_type) = stmt.type_.clone().as_mut() {
            if let Type::UserDefined(token) = declared_type {
                Some(TypeInferrer::infer_type(
                    self,
                    &Expr::Variable(Variable::new(token.clone())),
                )?)
            } else if let Type::Fn {
                params,
                return_type: _,
            } = declared_type
            {
                if let Some(params) = params {
                    for pair in params {
                        if let Type::UserDefined(label) = pair.get_type() {
                            pair.set_type(self.get_type(label)?);
                        }
                    }
                }
                Some(declared_type.clone())
            } else {
                Some(declared_type.clone())
            }
        } else {
            None
        };

        if let Some(value_type) = value_type {
            if let Some(declared_type) = declared_type {
                if !declared_type.eq_type(&value_type)
                    && !IsInstance::is_instance(&value_type, &declared_type)?
                {
                    return Err(JokerError::Resolver(Error::Struct(
                        StructError::report_error(
                            &stmt.name,
                            format!(
                                "Type mismatch: expected {}, found {}",
                                declared_type, value_type
                            ),
                        ),
                    )));
                }
            }
            self.declare_type(&stmt.name, value_type)?;
            return Ok(());
        } else if let Some(declared_type) = declared_type {
            self.declare_type(&stmt.name, declared_type)?;
            return Ok(());
        }
        Err(JokerError::Resolver(Error::Struct(
            StructError::report_error(&stmt.name, String::from("Missing type information")),
        )))
    }
}
impl ExprResolver<()> for Resolver {
    fn resolve(&self, expr: &Expr) -> Result<(), JokerError> {
        expr.accept(self)?;
        Ok(())
    }
    // getter: expr.name
    // setter| assign: name = expr
    fn resolve_local(&self, expr: Expr, name: &Token) -> Result<(), JokerError> {
        for (layer, scope) in self.scopes_stack.borrow().iter().rev().enumerate() {
            if let Entry::Occupied(mut entry) = scope.borrow_mut().entry(Key(name.clone())) {
                self.interpreter.resolve(expr, layer);
                match entry.get() {
                    VarStatus::Declare => entry.insert(VarStatus::Define),
                    VarStatus::Define | VarStatus::Used => entry.insert(VarStatus::Used),
                };
                return Ok(());
            }
        }
        Ok(())
    }
    fn resolve_lambda(&self, expr: &Lambda) -> Result<(), JokerError> {
        self.context_status_stack
            .borrow_mut()
            .push(ContextStatus::Fn(ReturnType::Specific(
                expr.return_type.clone(),
            )));

        self.begin_scope();
        if let Some(tokens) = expr.params.as_ref() {
            for param in tokens {
                if let ParamPair::Normal { param, type_ } = param {
                    // value check
                    self.declare(param)?;
                    self.define(param)?;
                    // type check
                    let type_: Type = if let Type::UserDefined(token) = type_ {
                        TypeInferrer::infer_type(
                            self,
                            &Expr::Variable(Variable {
                                name: token.clone(),
                            }),
                        )?
                    } else {
                        type_.clone()
                    };
                    self.declare_type(param, type_)?;
                } else {
                    return Err(JokerError::Resolver(Error::Struct(
                        StructError::report_error(
                            &expr.pipe,
                            String::from("Invalid parameter type"),
                        ),
                    )));
                }
            }
        }
        StmtResolver::resolve_lambda(self, &expr.pipe, &expr.body)?;

        // check local var used status
        self.check_vars_status()?;
        self.end_scope();

        self.context_status_stack.borrow_mut().pop();
        Ok(())
    }
    fn resolve_call(&self, expr: &Call) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.callee)?;
        expr.arguments
            .iter()
            .try_for_each(|arg| ExprResolver::resolve(self, arg))?;

        let callee_type: Type = TypeInferrer::infer_type(self, &expr.callee)?;
        match callee_type {
            Type::Fn {
                params,
                return_type: _,
            } => {
                if params.as_ref().map_or(false, |p| {
                    if p[0].is_this() {
                        p.len() - 1 != expr.arguments.len()
                    } else {
                        p.len() != expr.arguments.len()
                    }
                }) {
                    return Err(JokerError::Resolver(Error::Struct(
                        StructError::report_error(
                            &expr.paren,
                            format!(
                                "Expected {} arguments but got {}.",
                                params.as_ref().unwrap().len(),
                                expr.arguments.len()
                            ),
                        ),
                    )));
                }

                if let Some(param_types) = params {
                    if param_types[0].is_this() {
                        for (arg, param_pair) in expr.arguments.iter().zip(param_types[1..].iter())
                        {
                            // TODO: update param pair logical
                            if let ParamPair::Normal { param: _, type_ } = param_pair {
                                let arg_type: Type = TypeInferrer::infer_type(self, arg)?;
                                let param_type: Type = if let Type::UserDefined(token) = type_ {
                                    TypeInferrer::infer_type(
                                        self,
                                        &Expr::Variable(Variable {
                                            name: token.clone(),
                                        }),
                                    )?
                                } else {
                                    type_.clone()
                                };
                                if !arg_type.eq_type(&param_type)
                                    && !IsInstance::is_instance(&arg_type, &param_type)?
                                {
                                    return Err(JokerError::Resolver(Error::Struct(
                                        StructError::report_error(
                                            &expr.paren,
                                            format!(
                                                "Expected argument of type '{}' but got '{}'.",
                                                type_, arg_type
                                            ),
                                        ),
                                    )));
                                }
                            } else {
                                return Err(JokerError::Resolver(Error::Struct(
                                    StructError::report_error(
                                        &expr.paren,
                                        String::from("Invalid parameter type in method call."),
                                    ),
                                )));
                            }
                        }
                    } else {
                        for (arg, param_pair) in expr.arguments.iter().zip(param_types.iter()) {
                            // Fn { params: Some([Label { type_: I32 }, Label { type_: I32 }]), return_type: Some(I32) }
                            let executer = |arg: &Expr, type_: &Type| -> Result<(), JokerError> {
                                let arg_type: Type = TypeInferrer::infer_type(self, arg)?;
                                let param_type: Type = if let Type::UserDefined(token) = type_ {
                                    TypeInferrer::infer_type(
                                        self,
                                        &Expr::Variable(Variable {
                                            name: token.clone(),
                                        }),
                                    )?
                                } else {
                                    type_.clone()
                                };
                                if !arg_type.eq_type(&param_type)
                                    && !IsInstance::is_instance(&arg_type, &param_type)?
                                {
                                    return Err(JokerError::Resolver(Error::Struct(
                                        StructError::report_error(
                                            &expr.paren,
                                            format!(
                                                "Expected argument of type '{}' but got '{}'.",
                                                type_, arg_type
                                            ),
                                        ),
                                    )));
                                }
                                Ok(())
                            };
                            match param_pair {
                                ParamPair::Label { type_ } => executer(arg, type_)?,
                                ParamPair::Normal { param: _, type_ } => executer(arg, type_)?,
                                ParamPair::This { param: _, type_ } => executer(arg, type_)?,
                            }
                        }
                    }
                }
                Ok(())
            }
            Type::Class {
                name,
                super_class: _,
                fields: _,
                methods: _,
                functions: _,
            } => {
                let _class_type: Type = self.get_type(&name)?;
                Ok(())
            }
            _ => Err(JokerError::Resolver(Error::Struct(
                StructError::report_error(
                    &expr.paren,
                    String::from("Can only call functions and classes."),
                ),
            ))),
        }
    }
}

// Visitor
impl StmtVisitor<()> for Resolver {
    fn visit_if(&self, stmt: &IfStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.condition)?;
        StmtResolver::resolve(self, &stmt.then_branch)?;
        if let Some(else_branch) = stmt.else_branch.as_ref() {
            StmtResolver::resolve(self, else_branch)?;
        }
        Ok(())
    }
    fn visit_var(&self, stmt: &VarStmt) -> Result<(), JokerError> {
        if self.contains_any(&[
            ContextStatus::Fn(ReturnType::Any),
            ContextStatus::Loop,
            ContextStatus::Class(ClassStatus::Class),
            ContextStatus::Class(ClassStatus::SuperClass),
            ContextStatus::Class(ClassStatus::Method(ReturnType::Any)),
            ContextStatus::Class(ClassStatus::Fn(ReturnType::Any)),
        ]) {
            StmtResolver::resolve_var(self, stmt)?;

            self.declare(&stmt.name)?;
            if let Some(expr) = stmt.value.as_ref() {
                ExprResolver::resolve(self, expr)?;
                self.define(&stmt.name)?;
            }
            Ok(())
        } else {
            Err(JokerError::Resolver(Error::Env(EnvError::report_error(
                &stmt.name,
                String::from("var need in block."),
            ))))
        }
    }
    fn visit_for(&self, stmt: &ForStmt) -> Result<(), JokerError> {
        if let Some(initializer) = stmt.initializer.as_ref() {
            StmtResolver::resolve(self, initializer)?;
        }
        ExprResolver::resolve(self, &stmt.condition)?;
        if let Some(increment) = stmt.increment.as_ref() {
            ExprResolver::resolve(self, increment)?;
        }

        self.context_status_stack
            .borrow_mut()
            .push(ContextStatus::Loop);

        StmtResolver::resolve(self, &stmt.body)?;

        self.context_status_stack.borrow_mut().pop();

        Ok(())
    }
    fn visit_fn(&self, stmt: &FnStmt) -> Result<(), JokerError> {
        self.declare(&stmt.name)?;
        self.define(&stmt.name)?;
        StmtResolver::resolve_function(self, stmt)?;
        self.declare_type(
            &stmt.name,
            Type::Fn {
                params: stmt.params.clone(),
                return_type: stmt.return_type.clone(),
            },
        )?;
        Ok(())
    }
    fn visit_class(&self, stmt: &ClassStmt) -> Result<(), JokerError> {
        self.declare(&stmt.name)?;
        self.define(&stmt.name)?;
        StmtResolver::resolve_class(self, stmt)?;
        self.declare_type(&stmt.name, TypeInferrer::infer_class_stmt(self, stmt)?)?;
        Ok(())
    }
    fn visit_expr(&self, stmt: &ExprStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.expr)?;
        Ok(())
    }
    fn visit_print(&self, stmt: &PrintStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.expr)?;
        Ok(())
    }
    fn visit_block(&self, stmt: &BlockStmt) -> Result<(), JokerError> {
        if self.contains_any(&[
            ContextStatus::Fn(ReturnType::Any),
            ContextStatus::Class(ClassStatus::Method(ReturnType::Any)),
            ContextStatus::Class(ClassStatus::Fn(ReturnType::Any)),
        ]) {
            self.begin_scope();
            self.resolve_block(&stmt.stmts)?;

            // check local var used status
            self.check_vars_status()?;
            self.end_scope();
            return Ok(());
        }
        Err(JokerError::Resolver(Error::KeyWord(KeyWordError::Pos(
            PosError::report_error(
                &Token::eof(0),
                String::from("Cannot use 'block' outside of a fun statement."),
            ),
        ))))
    }
    fn visit_while(&self, stmt: &WhileStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.condition)?;

        self.context_status_stack
            .borrow_mut()
            .push(ContextStatus::Loop);

        StmtResolver::resolve(self, &stmt.body)?;

        self.context_status_stack.borrow_mut().pop();
        Ok(())
    }
    fn visit_break(&self, stmt: &BreakStmt) -> Result<(), JokerError> {
        if self.contains_any(&[ContextStatus::Loop]) {
            return Ok(());
        }
        Err(JokerError::Resolver(Error::KeyWord(KeyWordError::Pos(
            PosError::report_error(
                &stmt.name,
                String::from("Cannot use 'break' outside of a loop statement."),
            ),
        ))))
    }
    fn visit_return(&self, stmt: &ReturnStmt) -> Result<(), JokerError> {
        if self.last_any(&[
            ContextStatus::Fn(ReturnType::Any),
            ContextStatus::Class(ClassStatus::Method(ReturnType::Any)),
            ContextStatus::Class(ClassStatus::Fn(ReturnType::Any)),
        ]) || self.contains_any(&[ContextStatus::Fn(ReturnType::Any)])
        {
            match (stmt.value.as_ref(), self.last_fn_return_type()) {
                (Some(expr), Some(expected_type)) => {
                    // value check
                    ExprResolver::resolve(self, expr)?;
                    // type check
                    let found_type: Type = TypeInferrer::infer_type(self, expr)?;
                    // is call?
                    if let Type::Fn { params: _, return_type } = found_type {
                        match return_type {
                            Some(return_type) => {
                                if !expected_type.eq_type(&return_type) {
                                    return Err(JokerError::Resolver(Error::Struct(
                                        StructError::report_error(
                                            &stmt.keyword,
                                            format!(
                                                "Return type mismatch: Expected type '{}', Found type '{}'.",
                                                expected_type, return_type
                                            ),
                                        ),
                                    )));
                                }
                                // auto(call other resolver check): check inner function parameters type.
                            },
                            None => return Err(JokerError::Resolver(Error::Struct(
                                StructError::report_error(
                                    &stmt.keyword,
                                    format!(
                                        "Return type mismatch: Expected type '{}', But not found type.",
                                        expected_type
                                    ),
                                ),
                            )))
                        }
                    } else {
                        if !expected_type.eq_type(&found_type) {
                            return Err(JokerError::Resolver(Error::Struct(
                                StructError::report_error(
                                    &stmt.keyword,
                                    format!(
                                        "Return type mismatch: Expected type '{}', Found type '{}'.",
                                        expected_type, found_type
                                    ),
                                ),
                            )));
                        }
                    }
                    Ok(())
                }
                (Some(expr), None) => {
                    // value check
                    ExprResolver::resolve(self, expr)?;
                    // type check
                    let found_type: Type = TypeInferrer::infer_type(self, expr)?;
                    Err(JokerError::Resolver(Error::Struct(
                        StructError::report_error(
                            &stmt.keyword,
                            format!(
                                "Return type mismatch: Expected don't type, Found type '{}'.",
                                found_type
                            ),
                        ),
                    )))
                }
                (None, Some(expected_type)) => Err(JokerError::Resolver(Error::Struct(
                    StructError::report_error(
                        &stmt.keyword,
                        format!(
                            "Return type mismatch: Expected type '{}', But not found type.",
                            expected_type
                        ),
                    ),
                ))),
                // TODO: Function default return 'null' or 'None' ?
                (None, None) => Ok(()),
            }
        } else {
            Err(JokerError::Resolver(Error::KeyWord(KeyWordError::Pos(
                PosError::report_error(
                    &stmt.keyword,
                    String::from("Cannot use 'return' outside of a function statement."),
                ),
            ))))
        }
    }
    fn visit_continue(&self, stmt: &ContinueStmt) -> Result<(), JokerError> {
        if self.last_any(&[ContextStatus::Loop]) {
            Ok(())
        } else {
            Err(JokerError::Resolver(Error::KeyWord(KeyWordError::Pos(
                PosError::report_error(
                    &stmt.name,
                    String::from("Cannot use 'continue' outside of a loop statement."),
                ),
            ))))
        }
    }
}

impl ExprVisitor<()> for Resolver {
    fn visit_call(&self, expr: &Call) -> Result<(), JokerError> {
        ExprResolver::resolve_call(self, expr)?;
        Ok(())
    }
    fn visit_getter(&self, expr: &Getter) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.expr)?;
        ExprResolver::resolve_local(self, Expr::Getter(expr.clone()), &expr.name)?;
        Ok(())
    }
    fn visit_setter(&self, expr: &Setter) -> Result<(), JokerError> {
        if self.last_any(&[ContextStatus::Class(ClassStatus::Method(ReturnType::Any))])
            || matches!(*expr.l_expr, Expr::Variable(_))
        {
            // value check
            ExprResolver::resolve(self, &expr.r_expr)?;
            ExprResolver::resolve(self, &expr.l_expr)?;
            // TODO: type check: class instance
            // Type(enum) -> Type(struct{enum}) ?
            match expr.l_expr.as_ref() {
                // class outside setter variable.
                Expr::Variable(Variable { name }) => {
                    let caller_type: Type = self.get_type(name)?;
                    let key: String = expr.name.lexeme.clone();
                    let value_type: Type = TypeInferrer::infer_type(self, &expr.r_expr)?;

                    if caller_type.is_instance() {
                        // find instance parameter exit?
                        match caller_type.get_type(&expr.name)? {
                            Some(expected_type) => {
                                if !value_type.eq_type(expected_type) {
                                    return Err(JokerError::Resolver(Error::Struct(StructError::report_error(
                                        &expr.name,
                                        format!("Setter type mismatch: Expected type '{}', Found type '{}'.",
                                            expected_type, value_type,
                                        )
                                    ))));
                                }
                            }
                            // TODO: class instance type update new parameter type, {fields, method}.
                            // add: new parameter
                            None => {
                                if let Type::Instance {
                                    class,
                                    fields,
                                    methods,
                                } = caller_type
                                {
                                    // first check parameter is ? Fn type
                                    if value_type.is_fn() {
                                        match methods {
                                            Some(mut methods) => {
                                                methods
                                                    .insert(key, value_type);
                                                self.assign_type(
                                                    name,
                                                    Type::Instance {
                                                        class,
                                                        fields,
                                                        methods: Some(methods),
                                                    },
                                                )?;
                                                return Ok(());
                                            }
                                            None => {
                                                let methods = HashMap::from([(
                                                    key,
                                                    value_type,
                                                )]);
                                                self.assign_type(
                                                    name,
                                                    Type::Instance {
                                                        class,
                                                        fields,
                                                        methods: Some(methods),
                                                    },
                                                )?;
                                                return Ok(());
                                            }
                                        }
                                    }
                                    // other type add to fields
                                    match fields {
                                        Some(mut fields) => {
                                            fields.insert(expr.name.lexeme.clone(), value_type);
                                            self.assign_type(
                                                name,
                                                Type::Instance {
                                                    class,
                                                    fields: Some(fields),
                                                    methods,
                                                },
                                            )?;
                                            return Ok(());
                                        }
                                        None => {
                                            let fields: HashMap<String, Type> = HashMap::from([(
                                                expr.name.lexeme.clone(),
                                                value_type,
                                            )]);
                                            self.assign_type(
                                                name,
                                                Type::Instance {
                                                    class,
                                                    fields: Some(fields),
                                                    methods,
                                                },
                                            )?;
                                            return Ok(());
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        println!("[Resolve::visit_setter] Expr::Variable caller_type not instance.")
                    }
                },
                // class inside setter variable.
                // first, check class parameters, after instance parameters.
                Expr::This(This { keyword }) => {
                    let caller_type: Type = self.get_type(keyword)?;
                    let key: String = expr.name.lexeme.clone();
                    let value_type: Type = TypeInferrer::infer_type(self, &expr.r_expr)?;

                    if caller_type.is_instance() {
                        // find instance parameter exit?
                        match caller_type.get_type(&expr.name)? {
                            Some(expected_type) => {
                                if !value_type.eq_type(expected_type) {
                                    return Err(JokerError::Resolver(Error::Struct(StructError::report_error(
                                        &expr.name,
                                        format!("Setter type mismatch: Expected type '{}', Found type '{}'.",
                                            expected_type, value_type,
                                        )
                                    ))));
                                }
                            }
                            // TODO: class instance type update new parameter type, {fields, method}.
                            // add: new parameter
                            None => {
                                if let Type::Instance {
                                    class,
                                    fields,
                                    methods,
                                } = caller_type
                                {
                                    // first check parameter is ? Fn type
                                    if value_type.is_fn() {
                                        match methods {
                                            Some(mut methods) => {
                                                methods
                                                    .insert(key, value_type);
                                                self.assign_type(
                                                    keyword,
                                                    Type::Instance {
                                                        class,
                                                        fields,
                                                        methods: Some(methods),
                                                    },
                                                )?;
                                                return Ok(());
                                            }
                                            None => {
                                                let methods = HashMap::from([(
                                                    key,
                                                    value_type,
                                                )]);
                                                self.assign_type(
                                                    keyword,
                                                    Type::Instance {
                                                        class,
                                                        fields,
                                                        methods: Some(methods),
                                                    },
                                                )?;
                                                return Ok(());
                                            }
                                        }
                                    }
                                    // other type add to fields
                                    match fields {
                                        Some(mut fields) => {
                                            fields.insert(expr.name.lexeme.clone(), value_type);
                                            self.assign_type(
                                                keyword,
                                                Type::Instance {
                                                    class,
                                                    fields: Some(fields),
                                                    methods,
                                                },
                                            )?;
                                            return Ok(());
                                        }
                                        None => {
                                            let fields: HashMap<String, Type> = HashMap::from([(
                                                expr.name.lexeme.clone(),
                                                value_type,
                                            )]);
                                            self.assign_type(
                                                keyword,
                                                Type::Instance {
                                                    class,
                                                    fields: Some(fields),
                                                    methods,
                                                },
                                            )?;
                                            return Ok(());
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        return Err(JokerError::Resolver(Error::Struct(StructError::report_error(
                            keyword,
                            format!(
                                "[Resolve::visit_setter] Setter type mismatch: Expected left type 'Instance', Found type '{}'.",
                                caller_type
                            )
                        ))))
                    }
                },
                _ => return Err(JokerError::Resolver(Error::Struct(StructError::report_error(
                    &expr.name,
                    format!(
                        "[Resolve::visit_setter] Setter type mismatch: Expected  'Expr::Variable' or ' Expr::This', Found type '{}'.", 
                        expr.l_expr
                    ),
                ))))
            }
            // label
            ExprResolver::resolve_local(self, Expr::Setter(expr.clone()), &expr.name)?;
            Ok(())
        } else {
            Err(JokerError::Resolver(Error::Env(EnvError::report_error(
                &expr.name,
                String::from("Setter scope mismatch: Expected 'in method | used variable'."),
            ))))
        }
    }
    fn visit_this(&self, expr: &This) -> Result<(), JokerError> {
        if self.last_previous_any(&[
            ContextStatus::Class(ClassStatus::Class),
            ContextStatus::Class(ClassStatus::SuperClass),
        ]) && self.last_any(&[ContextStatus::Class(ClassStatus::Method(ReturnType::Any))])
        {
            ExprResolver::resolve_local(self, Expr::This(expr.clone()), &expr.keyword)?;
            Ok(())
        } else {
            Err(JokerError::Resolver(Error::Env(EnvError::report_error(
                &expr.keyword,
                String::from("can't use 'this' outside of a class."),
            ))))
        }
    }
    fn visit_unary(&self, expr: &Unary) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.r_expr)?;
        Ok(())
    }
    fn visit_binary(&self, expr: &Binary) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.l_expr)?;
        ExprResolver::resolve(self, &expr.r_expr)?;
        Ok(())
    }
    fn visit_assign(&self, expr: &Assign) -> Result<(), JokerError> {
        // value check
        ExprResolver::resolve(self, &expr.value)?;
        // type check
        let assign_type: Type = self.get_type(&expr.name)?;
        let value_type: Type = TypeInferrer::infer_type(self, &expr.value)?;
        if !assign_type.eq(&value_type) {
            return Err(JokerError::Resolver(Error::Struct(
                StructError::report_error(
                    &expr.name,
                    format!(
                        "Assign type mismatch: Expected type '{}', Found type '{}'.",
                        assign_type, value_type,
                    ),
                ),
            )));
        }

        ExprResolver::resolve_local(self, Expr::Assign(expr.clone()), &expr.name)?;
        Ok(())
    }
    fn visit_lambda(&self, expr: &Lambda) -> Result<(), JokerError> {
        ExprResolver::resolve_lambda(self, expr)?;
        Ok(())
    }
    fn visit_literal(&self, _expr: &Literal) -> Result<(), JokerError> {
        Ok(())
    }
    fn visit_logical(&self, expr: &Logical) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.l_expr)?;
        ExprResolver::resolve(self, &expr.r_expr)?;
        Ok(())
    }
    fn visit_grouping(&self, expr: &Grouping) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.expr)?;
        Ok(())
    }
    fn visit_variable(&self, expr: &Variable) -> Result<(), JokerError> {
        if let Some(scope) = self.scopes_stack.borrow().last() {
            if let Some(VarStatus::Declare) = scope.borrow().get(&Key(expr.name.clone())) {
                return Err(JokerError::Resolver(Error::Var(VarError::Init(
                    InitError::report_error(
                        &expr.name,
                        String::from("Can't read local variable in its own initializer."),
                    ),
                ))));
            }
        }
        self.resolve_local(Expr::Variable(expr.clone()), &expr.name)?;
        Ok(())
    }
    fn visit_trinomial(&self, expr: &Trinomial) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.condition)?;
        ExprResolver::resolve(self, &expr.l_expr)?;
        ExprResolver::resolve(self, &expr.r_expr)?;
        Ok(())
    }
    fn visit_super(&self, expr: &Super) -> Result<(), JokerError> {
        if self.last_previous_any(&[ContextStatus::Class(ClassStatus::SuperClass)])
            && self.last_any(&[ContextStatus::Class(ClassStatus::Method(ReturnType::Any))])
        {
            ExprResolver::resolve_local(self, Expr::Super(expr.clone()), &expr.keyword)?;
            Ok(())
        } else {
            Err(JokerError::Resolver(Error::Env(EnvError::report_error(
                &expr.keyword,
                String::from("super keyword need in inherit class instance function used."),
            ))))
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Env(EnvError),
    Var(VarError),
    KeyWord(KeyWordError),
    Struct(StructError),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Env(env) => Display::fmt(env, f),
            Error::Var(var) => Display::fmt(var, f),
            Error::KeyWord(keyword) => Display::fmt(keyword, f),
            Error::Struct(struct_) => Display::fmt(struct_, f),
        }
    }
}

impl std::error::Error for Error {}

impl ReportError for Error {
    fn report(&self) {
        match self {
            Error::Env(env) => ReportError::report(env),
            Error::Var(var) => ReportError::report(var),
            Error::KeyWord(keyword) => ReportError::report(keyword),
            Error::Struct(struct_) => ReportError::report(struct_),
        }
    }
}

#[derive(Debug)]
pub enum VarError {
    Init(InitError),
    Redefine(RedefineError),
}

impl Display for VarError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarError::Init(init) => Display::fmt(init, f),
            VarError::Redefine(redefine) => Display::fmt(redefine, f),
        }
    }
}

impl std::error::Error for VarError {}

impl ReportError for VarError {
    fn report(&self) {
        match self {
            VarError::Init(init) => ReportError::report(init),
            VarError::Redefine(redefine) => ReportError::report(redefine),
        }
    }
}

#[derive(Debug)]
pub struct InitError {
    line: usize,
    where_: String,
    msg: String,
}

impl InitError {
    pub fn new(token: &Token, msg: String) -> InitError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        InitError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> InitError {
        let arg_limit = InitError::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for InitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "InitError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl std::error::Error for InitError {}

impl ReportError for InitError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[derive(Debug)]
pub struct RedefineError {
    line: usize,
    where_: String,
    msg: String,
}

impl RedefineError {
    pub fn new(token: &Token, msg: String) -> RedefineError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        RedefineError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> RedefineError {
        let arg_limit = RedefineError::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for RedefineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "RedefineError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl std::error::Error for RedefineError {}

impl ReportError for RedefineError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[derive(Debug)]
pub enum KeyWordError {
    Pos(PosError),
}

impl Display for KeyWordError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyWordError::Pos(pos) => Display::fmt(pos, f),
        }
    }
}

impl std::error::Error for KeyWordError {}

impl ReportError for KeyWordError {
    fn report(&self) {
        match self {
            KeyWordError::Pos(pos) => ReportError::report(pos),
        }
    }
}

#[derive(Debug)]
pub struct PosError {
    line: usize,
    where_: String,
    msg: String,
}

impl PosError {
    pub fn new(token: &Token, msg: String) -> PosError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        PosError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> PosError {
        let arg_limit = PosError::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for PosError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PosError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl std::error::Error for PosError {}

impl ReportError for PosError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[derive(Debug)]
pub struct StatusError {
    line: usize,
    where_: String,
    msg: String,
}

impl StatusError {
    pub fn new(token: &Token, msg: String) -> StatusError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        StatusError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> StatusError {
        let arg_limit = StatusError::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for StatusError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PosError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl std::error::Error for StatusError {}

impl ReportError for StatusError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
