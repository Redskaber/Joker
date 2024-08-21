//! This file is joker resolver rs
//!
//! - Resolver
//!     This struct used static resolve all maybe stmt and expr. handle closure env wait question.  
//!
//!

use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    error::Error,
    fmt::Display,
    hash::Hash,
    rc::Rc,
};

use super::{
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Call, ClassStmt, ContinueStmt, Expr, ExprAcceptor,
        ExprStmt, ExprVisitor, ForStmt, FunStmt, Getter, Grouping, IfStmt, Lambda, Literal,
        Logical, PrintStmt, ReturnStmt, Setter, Stmt, StmtAcceptor, StmtVisitor, This, Trinomial,
        Unary, VarStmt, Variable, WhileStmt,
    },
    callable::StructError,
    env::EnvError,
    error::{JokerError, ReportError},
    interpreter::Interpreter,
    token::{Token, TokenType},
};

pub trait StmtResolver<T> {
    fn resolve(&self, stmt: &Stmt) -> Result<T, JokerError>;
    fn resolve_block(&self, stmts: &[Stmt]) -> Result<T, JokerError>;
    fn resolve_function(&self, stmt: &FunStmt) -> Result<T, JokerError>;
    fn resolve_lambda(&self, pipe: &Token, stmt: &Stmt) -> Result<T, JokerError>;
    fn resolve_class(&self, stmt: &ClassStmt) -> Result<T, JokerError>;
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
    Fun,
    Loop,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClassStatus {
    Default,
    Init,
    Method,
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
    pub scopes_stack: RefCell<Vec<RefCell<HashMap<Key, VarStatus>>>>,
    context_status_stack: RefCell<Vec<ContextStatus>>,
}

impl Resolver {
    pub fn new(interpreter: Rc<Interpreter>) -> Resolver {
        Resolver {
            interpreter,
            scopes_stack: RefCell::new(Vec::new()),
            context_status_stack: RefCell::new(Vec::new()),
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
    }
    fn end_scope(&self) {
        self.scopes_stack.borrow_mut().pop();
    }
    fn declare(&self, name: &Token) -> Result<(), JokerError> {
        if let Some(scope) = self.scopes_stack.borrow().last() {
            match scope.borrow_mut().entry(Key(name.clone())) {
                Entry::Occupied(_) => {
                    return Err(JokerError::Resolver(ResolverError::Var(
                        VarError::Redefine(RedefineError::report_error(
                            name,
                            format!(
                                "Variable '{}' is already declared in this scope.",
                                name.lexeme
                            ),
                        )),
                    )));
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
    fn check_var_status(&self, name: &Key, value_status: &VarStatus) -> Result<(), JokerError> {
        match value_status {
            VarStatus::Declare | VarStatus::Define => {
                let message = match value_status {
                    VarStatus::Declare => "Variable declared but not used",
                    VarStatus::Define => "Variable defined but not used",
                    _ => unreachable!(),
                };
                Err(JokerError::Resolver(ResolverError::Var(VarError::Status(
                    StatusError::report_error(name.token(), message.to_string()),
                ))))
            }
            VarStatus::Used => Ok(()),
        }
    }
    fn check_vars_status(&self) -> Result<(), JokerError> {
        match self.scopes_stack.borrow().last() {
            Some(current_scope) => current_scope
                .borrow()
                .iter()
                .try_for_each(|(name, value_status)| self.check_var_status(name, value_status)),
            None => Err(JokerError::Resolver(ResolverError::Env(
                EnvError::report_error(
                    &Token::eof(0),
                    String::from("No current environment to check variables"),
                ),
            ))),
        }
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
    fn resolve_function(&self, stmt: &FunStmt) -> Result<(), JokerError> {
        if matches!(
            self.context_status_stack.borrow().last(),
            Some(ContextStatus::Class(ClassStatus::Default))
        ) {
            if stmt.name.lexeme == "init" {
                self.context_status_stack
                    .borrow_mut()
                    .push(ContextStatus::Class(ClassStatus::Init));
            } else {
                self.context_status_stack
                    .borrow_mut()
                    .push(ContextStatus::Class(ClassStatus::Method));
            }
        } else {
            self.context_status_stack
                .borrow_mut()
                .push(ContextStatus::Fun);
        }

        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_function", self.context_status_stack
        );

        self.begin_scope();
        for param in &stmt.params {
            self.declare(param)?;
            self.define(param)?;
        }
        StmtResolver::resolve_block(self, &stmt.body)?;

        // check local var used status
        self.check_vars_status()?;
        self.end_scope();

        self.context_status_stack.borrow_mut().pop();

        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_function", self.context_status_stack
        );
        Ok(())
    }
    fn resolve_lambda(&self, pipe: &Token, stmt: &Stmt) -> Result<(), JokerError> {
        match stmt {
            Stmt::BlockStmt(block) => StmtResolver::resolve_block(self, &block.stmts),
            Stmt::ExprStmt(expr) => ExprResolver::resolve(self, &expr.expr),
            _ => Err(JokerError::Resolver(ResolverError::Struct(
                StructError::report_error(
                    pipe,
                    String::from("lambda structure: | params | expr '1' or block '{}'."),
                ),
            ))),
        }
    }
    fn resolve_class(&self, stmt: &ClassStmt) -> Result<(), JokerError> {
        self.context_status_stack
            .borrow_mut()
            .push(ContextStatus::Class(ClassStatus::Default));
        self.begin_scope();
        self.scopes_stack
            .borrow()
            .last()
            .unwrap()
            .borrow_mut()
            .insert(Key(Token::this(0)), VarStatus::Define);

        if let Some(stmts) = &stmt.methods {
            for stmt in stmts {
                if let Stmt::FunStmt(fun) = stmt {
                    StmtResolver::resolve_function(self, fun)?;
                }
            }
        }
        self.end_scope();
        self.context_status_stack.borrow_mut().pop();
        Ok(())
    }
}
impl ExprResolver<()> for Resolver {
    fn resolve(&self, expr: &Expr) -> Result<(), JokerError> {
        expr.accept(self)?;
        Ok(())
    }
    fn resolve_local(&self, expr: Expr, name: &Token) -> Result<(), JokerError> {
        for (layer, scope) in self.scopes_stack.borrow().iter().rev().enumerate() {
            println!(
                "[{:>10}][{:>20}]:\tlayer: {},\tscope: {:?}",
                "resolve", "resolve_local", layer, scope
            );
            if let Entry::Occupied(mut entry) = scope.borrow_mut().entry(Key(name.clone())) {
                self.interpreter.resolve(expr, layer);
                entry.insert(VarStatus::Used);
                return Ok(());
            }
        }
        Ok(())
    }
    fn resolve_lambda(&self, expr: &Lambda) -> Result<(), JokerError> {
        self.context_status_stack
            .borrow_mut()
            .push(ContextStatus::Fun);
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_lambda", self.context_status_stack
        );

        self.begin_scope();
        for param in &expr.params {
            self.declare(param)?;
            self.define(param)?;
        }
        StmtResolver::resolve_lambda(self, &expr.pipe, &expr.body)?;

        // check local var used status
        self.check_vars_status()?;
        self.end_scope();

        self.context_status_stack.borrow_mut().pop();

        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_lambda", self.context_status_stack
        );
        Ok(())
    }
    fn resolve_call(&self, expr: &Call) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?},\tcallee: {:?}",
            "resolve", "resolve_call", self.context_status_stack, expr.callee
        );
        ExprResolver::resolve(self, &expr.callee)?;
        expr.arguments
            .iter()
            .try_for_each(|arg| ExprResolver::resolve(self, arg))?;
        Ok(())
    }
}

// Visitor
impl StmtVisitor<()> for Resolver {
    fn visit_if(&self, stmt: &IfStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.condition)?;
        StmtResolver::resolve(self, &stmt.then_branch)?;
        if let Some(else_branch) = &stmt.else_branch {
            StmtResolver::resolve(self, else_branch)?;
        }
        Ok(())
    }
    fn visit_var(&self, stmt: &VarStmt) -> Result<(), JokerError> {
        self.declare(&stmt.name)?;
        if let Some(expr) = &stmt.value {
            ExprResolver::resolve(self, expr)?;
            self.define(&stmt.name)?;
        }
        Ok(())
    }
    fn visit_for(&self, stmt: &ForStmt) -> Result<(), JokerError> {
        if let Some(initializer) = &stmt.initializer {
            StmtResolver::resolve(self, initializer)?;
        }
        ExprResolver::resolve(self, &stmt.condition)?;
        if let Some(increment) = &stmt.increment {
            ExprResolver::resolve(self, increment)?;
        }

        self.context_status_stack
            .borrow_mut()
            .push(ContextStatus::Loop);
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_for", self.context_status_stack
        );

        StmtResolver::resolve(self, &stmt.body)?;

        self.context_status_stack.borrow_mut().pop();
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_for", self.context_status_stack
        );

        Ok(())
    }
    fn visit_fun(&self, stmt: &FunStmt) -> Result<(), JokerError> {
        self.declare(&stmt.name)?;
        self.define(&stmt.name)?;
        StmtResolver::resolve_function(self, stmt)?;
        Ok(())
    }
    fn visit_class(&self, stmt: &ClassStmt) -> Result<(), JokerError> {
        self.declare(&stmt.name)?;
        self.define(&stmt.name)?;
        StmtResolver::resolve_class(self, stmt)?;
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
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_block", self.context_status_stack
        );
        if self
            .context_status_stack
            .borrow()
            .contains(&ContextStatus::Fun)
        {
            self.begin_scope();
            self.resolve_block(&stmt.stmts)?;

            // check local var used status
            self.check_vars_status()?;
            self.end_scope();
            return Ok(());
        }
        Err(JokerError::Resolver(ResolverError::KeyWord(
            KeyWordError::Pos(PosError::report_error(
                &Token::eof(0),
                String::from("Cannot use 'block' outside of a fun statement."),
            )),
        )))
    }
    fn visit_while(&self, stmt: &WhileStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.condition)?;

        self.context_status_stack
            .borrow_mut()
            .push(ContextStatus::Loop);
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_while", self.context_status_stack
        );

        StmtResolver::resolve(self, &stmt.body)?;

        self.context_status_stack.borrow_mut().pop();
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_while", self.context_status_stack
        );

        Ok(())
    }
    fn visit_break(&self, stmt: &BreakStmt) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_break", self.context_status_stack
        );
        if self
            .context_status_stack
            .borrow()
            .contains(&ContextStatus::Loop)
        {
            return Ok(());
        }
        Err(JokerError::Resolver(ResolverError::KeyWord(
            KeyWordError::Pos(PosError::report_error(
                &stmt.name,
                String::from("Cannot use 'break' outside of a loop statement."),
            )),
        )))
    }
    fn visit_return(&self, stmt: &ReturnStmt) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_return", self.context_status_stack
        );
        if matches!(
            self.context_status_stack.borrow().last(),
            Some(&ContextStatus::Fun)
                | Some(&ContextStatus::Class(ClassStatus::Init))
                | Some(&ContextStatus::Class(ClassStatus::Method))
        ) || self
            .context_status_stack
            .borrow()
            .contains(&ContextStatus::Fun)
        {
            if let Some(expr) = &stmt.value {
                ExprResolver::resolve(self, expr)?;
            }
            Ok(())
        } else {
            Err(JokerError::Resolver(ResolverError::KeyWord(
                KeyWordError::Pos(PosError::report_error(
                    &stmt.keyword,
                    String::from("Cannot use 'return' outside of a function statement."),
                )),
            )))
        }
    }
    fn visit_continue(&self, stmt: &ContinueStmt) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}\n",
            "resolve", "visit_continue", self.context_status_stack
        );
        if matches!(
            self.context_status_stack.borrow().last(),
            Some(&ContextStatus::Loop)
        ) {
            return Ok(());
        }
        Err(JokerError::Resolver(ResolverError::KeyWord(
            KeyWordError::Pos(PosError::report_error(
                &stmt.name,
                String::from("Cannot use 'continue' outside of a loop statement."),
            )),
        )))
    }
}

impl ExprVisitor<()> for Resolver {
    fn visit_call(&self, expr: &Call) -> Result<(), JokerError> {
        ExprResolver::resolve_call(self, expr)?;
        Ok(())
    }
    fn visit_getter(&self, expr: &Getter) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}\n",
            "resolve", "visit_getter", self.context_status_stack
        );
        ExprResolver::resolve(self, &expr.expr)?;
        Ok(())
    }
    fn visit_setter(&self, expr: &Setter) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}\n",
            "resolve", "visit_setter", self.context_status_stack
        );
        if matches!(
            self.context_status_stack.borrow().last(),
            Some(&ContextStatus::Class(ClassStatus::Init))
        ) {
            ExprResolver::resolve(self, &expr.r_expr)?;
            ExprResolver::resolve(self, &expr.l_expr)?;
            Ok(())
        } else {
            Err(JokerError::Resolver(ResolverError::Env(
                EnvError::report_error(
                    &expr.name,
                    String::from("setter class instance attribute not exist."),
                ),
            )))
        }
    }
    fn visit_this(&self, expr: &This) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}\n",
            "resolve", "visit_this", self.context_status_stack
        );
        if self
            .context_status_stack
            .borrow()
            .contains(&ContextStatus::Class(ClassStatus::Default))
        {
            ExprResolver::resolve_local(self, Expr::This(expr.clone()), &expr.keyword)?;
            Ok(())
        } else {
            Err(JokerError::Resolver(ResolverError::Env(
                EnvError::report_error(
                    &expr.keyword,
                    String::from("can't use 'this' outside of a class."),
                ),
            )))
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
        ExprResolver::resolve(self, &expr.value)?;
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
                return Err(JokerError::Resolver(ResolverError::Var(VarError::Init(
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
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_trinomial", self.context_status_stack
        );

        ExprResolver::resolve(self, &expr.condition)?;
        ExprResolver::resolve(self, &expr.l_expr)?;
        ExprResolver::resolve(self, &expr.r_expr)?;

        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_trinomial", self.context_status_stack
        );
        Ok(())
    }
}

#[derive(Debug)]
pub enum ResolverError {
    Env(EnvError),
    Var(VarError),
    KeyWord(KeyWordError),
    Struct(StructError),
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolverError::Env(env) => Display::fmt(env, f),
            ResolverError::Var(var) => Display::fmt(var, f),
            ResolverError::KeyWord(keyword) => Display::fmt(keyword, f),
            ResolverError::Struct(struct_) => Display::fmt(struct_, f),
        }
    }
}

impl Error for ResolverError {}

impl ReportError for ResolverError {
    fn report(&self) {
        match self {
            ResolverError::Env(env) => ReportError::report(env),
            ResolverError::Var(var) => ReportError::report(var),
            ResolverError::KeyWord(keyword) => ReportError::report(keyword),
            ResolverError::Struct(struct_) => ReportError::report(struct_),
        }
    }
}

#[derive(Debug)]
pub enum VarError {
    Init(InitError),
    Redefine(RedefineError),
    Status(StatusError),
}

impl Display for VarError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarError::Init(init) => Display::fmt(init, f),
            VarError::Redefine(redefine) => Display::fmt(redefine, f),
            VarError::Status(status) => Display::fmt(status, f),
        }
    }
}

impl Error for VarError {}

impl ReportError for VarError {
    fn report(&self) {
        match self {
            VarError::Init(init) => ReportError::report(init),
            VarError::Redefine(redefine) => ReportError::report(redefine),
            VarError::Status(status) => ReportError::report(status),
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

impl Error for InitError {}

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

impl Error for RedefineError {}

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

impl Error for KeyWordError {}

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

impl Error for PosError {}

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

impl Error for StatusError {}

impl ReportError for StatusError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
