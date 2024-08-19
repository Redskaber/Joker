//! This is file resolve rs
//!
//!
//!

use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, hash::Hash, rc::Rc};

use super::store::{Key, VarBlock, VarItem, VarLayer, VarStatus, VarStore, VarStoreError};
use crate::joker::{
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Call, ContinueStmt, Expr, ExprAcceptor, ExprStmt,
        ExprVisitor, ForStmt, FunStmt, Grouping, IfStmt, Lambda, Literal, Logical, PrintStmt,
        ReturnStmt, Stmt, StmtAcceptor, StmtVisitor, Trinomial, Unary, VarStmt, Variable,
        WhileStmt,
    },
    callable::StructError,
    env::EnvError,
    error::{JokerError, ReportError},
    interpreter::Interpreter,
    token::{Token, TokenType},
};

// ------------------------------------------------------------------------------
impl PartialEq for Key {
    fn eq(&self, other: &Self) -> bool {
        self.0.ttype == other.0.ttype
            && self.0.lexeme == other.0.lexeme
            && self.0.literal == other.0.literal
    }
}

impl Eq for Key {}

impl Hash for Key {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (&self.0.ttype, &self.0.lexeme, &self.0.literal).hash(state)
    }
}
// ------------------------------------------------------------------------------
pub trait StmtResolver<T> {
    fn resolve(&self, stmt: &Stmt) -> Result<T, JokerError>;
    fn resolve_block(&self, stmts: &[Stmt]) -> Result<T, JokerError>;
    fn resolve_function(&self, stmt: &FunStmt) -> Result<T, JokerError>;
    fn resolve_lambda(&self, pipe: &Token, stmt: &Stmt) -> Result<T, JokerError>;
}

pub trait ExprResolver<T> {
    fn resolve(&self, expr: &Expr) -> Result<T, JokerError>;
    fn resolve_local(&self, expr: Expr, name: &Key) -> Result<T, JokerError>;
    fn resolve_lambda(&self, expr: &Lambda) -> Result<T, JokerError>;
    fn resolve_call(&self, expr: &Call) -> Result<T, JokerError>;
}

#[derive(Debug, PartialEq, Eq)]
pub enum ContextStatus {
    Fun,
    Loop,
}

#[derive(Debug)]
pub struct Resolver {
    interpreter: Rc<Interpreter>,
    var_stores: RefCell<VarStore>,
    context_stack: RefCell<Vec<ContextStatus>>,
}

impl Resolver {
    pub fn new(interpreter: Rc<Interpreter>) -> Self {
        Resolver {
            interpreter,
            var_stores: RefCell::new(VarStore::Block(VarBlock {
                stores: Vec::new(),      // g_store
                indexes: HashMap::new(), // g_map
            })),
            context_stack: RefCell::new(Vec::new()),
        }
    }
    fn begin_scope(&self) -> Result<(), JokerError> {
        self.var_stores.borrow_mut().try_block_borrow_mut()?.stores.push(VarStore::Block(VarBlock {
            stores: Vec::new(),
            indexes: HashMap::new(),
        }));
        Ok(())
    }
    fn end_scope(&self) -> Result<(), JokerError> {
        self.var_stores.borrow_mut().pop()?;
        Ok(())
    }
    pub fn resolve(&self, stmts: &[Stmt]) -> Result<(), JokerError> {
        self.resolve_block(stmts)?;
        Ok(())
    }
    fn declare(&self, name: &VarStore) -> Result<(), JokerError> {
        let mut g_borrow = self.var_stores.borrow_mut();
        let parent_store = g_borrow.try_block_borrow_mut()?;
        let last_store = parent_store.last_mut().unwrap().try_block_borrow_mut()?;
        match last_store.indexes.get(&name.try_var_borrow()?.key) {
            Some(_index) => {
                return Err(JokerError::Resolver(ResolverError::Var(
                    VarError::Redefine(RedefineError::report_error(
                        name.get_token().unwrap(),
                        format!(
                            "Variable '{}' is already declared in this scope.",
                            name.get_token().unwrap().lexeme
                        ),
                    )),
                )));
            }
            None => {
                let last_index = last_store.len();
                last_store.stores.push(name.clone());
                last_store.indexes.insert(name.try_var_borrow()?.key.clone(), last_index);
                parent_store.indexes.insert(
                    name.try_var_borrow()?.key.clone(),
                    parent_store.len() - 1,
                );
            }
        }
        Ok(())
    }
    fn define(&self, name: &VarStore) -> Result<(), JokerError> {
        let mut g_borrow = self.var_stores.borrow_mut();
        let parent_store = g_borrow.try_block_borrow_mut()?;
        let last_store = parent_store.last_mut().unwrap().try_block_borrow_mut()?;
        match last_store.indexes.get(&name.try_var_borrow()?.key) {
            Some(index) => {
                match self.var_stores.borrow_mut().try_block_borrow_mut()?.stores.get_mut(*index) {
                    Some(var_store) => {
                        var_store.try_var_borrow_mut()?.status = VarStatus::Define;
                    },
                    None => return Err(JokerError::Resolver(ResolverError::Var(VarError::Init(InitError::report_error(
                            name.get_token().unwrap(),
                            format!(
                                "Variable '{}' is declared in scope, index: '{}', but VarStore is not get.",
                                name.get_token().unwrap(),
                                index,
                            )
                        )))))
                }
            }
            None => {
                return Err(JokerError::Resolver(ResolverError::Env(
                    EnvError::report_error(
                        name.get_token().unwrap(),
                        format!(
                            "Variable '{}' is not declare in scope, index: 'None'.",
                            name.get_token().unwrap(),
                        ),
                    ),
                )))
            }
        }
        Ok(())
    }
    fn check_var_status(&self, var_store: &VarStore) -> Result<(), JokerError> {
        let var_store = var_store.try_var_borrow()?;
        match var_store.status {
            VarStatus::Declare | VarStatus::Define => {
                let message = match var_store.status {
                    VarStatus::Declare => "Variable declared but not used",
                    VarStatus::Define => "Variable defined but not used",
                    _ => unreachable!(),
                };
                Err(JokerError::Resolver(ResolverError::Var(VarError::Status(
                    StatusError::report_error(var_store.key.inner(), message.to_string()),
                ))))
            }
            VarStatus::Used => Ok(()),
        }
    }
    fn check_vars_status(&self) -> Result<(), JokerError> {
        match self.var_stores.borrow().try_block_borrow()?.stores.last() {
            Some(last_store) => last_store
                .try_block_borrow()?
                .stores
                .iter()
                .try_for_each(|var_store| self.check_var_status(var_store)),
            None => Err(JokerError::Resolver(ResolverError::Env(
                EnvError::report_error(
                    &Token::eof(0),
                    String::from("No current environment to check variables"),
                ),
            ))),
        }
    }
}

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
        self.context_stack.borrow_mut().push(ContextStatus::Fun);
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_function", self.context_stack
        );

        self.begin_scope()?;
        for (index, param) in stmt.params.iter().enumerate() {
            let var_store = VarStore::Var(VarItem {
                key: Key(param.clone()),
                layer: VarLayer { layer: 0, index },
                status: VarStatus::default(),
            });
            self.declare(&var_store)?;
            self.define(&var_store)?;
        }
        StmtResolver::resolve_block(self, &stmt.body)?;
        // check local var used status
        self.check_vars_status()?;
        self.end_scope()?;

        self.context_stack.borrow_mut().pop();

        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_function", self.context_stack
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
}

impl ExprResolver<()> for Resolver {
    fn resolve(&self, expr: &Expr) -> Result<(), JokerError> {
        expr.accept(self)
    }
    fn resolve_call(&self, expr: &Call) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?},\tcallee: {:?}",
            "resolve", "resolve_call", self.context_stack, expr.callee
        );
        ExprResolver::resolve(self, &expr.callee)?;
        expr.arguments
            .iter()
            .try_for_each(|arg| ExprResolver::resolve(self, arg))?;
        Ok(())
    }
    fn resolve_lambda(&self, expr: &Lambda) -> Result<(), JokerError> {
        self.context_stack.borrow_mut().push(ContextStatus::Fun);
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_lambda", self.context_stack
        );

        self.begin_scope()?;
        for (index, param) in expr.params.iter().enumerate() {
            let var_store = VarStore::Var(VarItem {
                key: Key(param.clone()),
                layer: VarLayer { layer: 0, index },
                status: VarStatus::default(),
            });
            self.declare(&var_store)?;
            self.define(&var_store)?;
        }
        StmtResolver::resolve_lambda(self, &expr.pipe, &expr.body)?;
        // check local var used status
        self.check_vars_status()?;
        self.end_scope()?;

        self.context_stack.borrow_mut().pop();

        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "resolve_lambda", self.context_stack
        );
        Ok(())
    }
    fn resolve_local(&self, expr: Expr, name: &Key) -> Result<(), JokerError> {
        let mut g_borrow = self.var_stores.borrow_mut();
        let parent_store = g_borrow.try_block_borrow_mut()?;
        for (layer, block_store) in parent_store.stores.iter_mut().rev().enumerate()
        {
            let block_mut: &mut VarBlock = block_store.try_block_borrow_mut()?;
            if let Some(index) = block_mut.indexes.get(name) {
                println!(
                    "[{:>10}][{:>20}]:\tlayer: {},\tscope: {:?}",
                    "resolve", "resolve_local", layer, block_mut
                );
                match block_mut.stores.get_mut(*index) {
                    Some(var_store) => {
                        self.interpreter.resolve(expr, layer);
                        var_store.try_var_borrow_mut()?.layer.layer = layer;
                        var_store.try_var_borrow_mut()?.status = VarStatus::Used;
                        return Ok(());
                    }
                    None => {
                        return Err(JokerError::Resolver(ResolverError::Var(VarError::Init(
                            InitError::report_error(
                                name.inner(),
                                format!(
                                    "Variable '{}' is in scope, index: '{}', but find error.",
                                    name.inner(),
                                    index,
                                ),
                            ),
                        ))))
                    }
                }
            }
        }

        Ok(())
    }
}

impl StmtVisitor<()> for Resolver {
    fn visit_expr(&self, stmt: &ExprStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.expr)?;
        Ok(())
    }
    fn visit_print(&self, stmt: &PrintStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.expr)?;
        Ok(())
    }
    fn visit_var(&self, stmt: &VarStmt) -> Result<(), JokerError> {
        let var_store = VarStore::Var(VarItem {
            key: Key(stmt.name.clone()),
            layer: VarLayer::default(),
            status: VarStatus::default(),
        });
        self.declare(&var_store)?;
        if let Some(expr) = &stmt.value {
            ExprResolver::resolve(self, expr)?;
            self.define(&var_store)?;
        }
        Ok(())
    }
    fn visit_block(&self, stmt: &BlockStmt) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_block", self.context_stack
        );
        if self.context_stack.borrow().contains(&ContextStatus::Fun) {
            self.begin_scope()?;
            self.resolve_block(&stmt.stmts)?;
            // check local var used status
            self.check_vars_status()?;
            self.end_scope()?;
            return Ok(());
        }
        Err(JokerError::Resolver(ResolverError::KeyWord(
            KeyWordError::Pos(PosError::report_error(
                &Token::eof(0),
                String::from("Cannot use 'block' outside of a fun statement."),
            )),
        )))
    }
    fn visit_if(&self, stmt: &IfStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.condition)?;
        StmtResolver::resolve(self, &stmt.then_branch)?;
        if let Some(else_branch) = &stmt.else_branch {
            StmtResolver::resolve(self, else_branch)?;
        }
        Ok(())
    }
    fn visit_while(&self, stmt: &WhileStmt) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &stmt.condition)?;

        self.context_stack.borrow_mut().push(ContextStatus::Loop);
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_while", self.context_stack
        );

        StmtResolver::resolve(self, &stmt.body)?;

        self.context_stack.borrow_mut().pop();
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_while", self.context_stack
        );
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

        self.context_stack.borrow_mut().push(ContextStatus::Loop);
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_for", self.context_stack
        );

        StmtResolver::resolve(self, &stmt.body)?;

        self.context_stack.borrow_mut().pop();
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_for", self.context_stack
        );

        Ok(())
    }
    fn visit_break(&self, stmt: &BreakStmt) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_break", self.context_stack
        );
        if self.context_stack.borrow().contains(&ContextStatus::Loop) {
            return Ok(());
        }
        Err(JokerError::Resolver(ResolverError::KeyWord(
            KeyWordError::Pos(PosError::report_error(
                &stmt.name,
                String::from("Cannot use 'break' outside of a loop statement."),
            )),
        )))
    }
    fn visit_continue(&self, stmt: &ContinueStmt) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}\n",
            "resolve", "visit_continue", self.context_stack
        );
        if matches!(
            self.context_stack.borrow().last(),
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
    fn visit_fun(&self, stmt: &FunStmt) -> Result<(), JokerError> {
        let var_store = VarStore::Var(VarItem {
            key: Key(stmt.name.clone()),
            layer: VarLayer::default(),
            status: VarStatus::default(),
        });
        self.declare(&var_store)?;
        self.define(&var_store)?;
        StmtResolver::resolve_function(self, stmt)?;
        Ok(())
    }
    fn visit_return(&self, stmt: &ReturnStmt) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_return", self.context_stack
        );
        if self.context_stack.borrow().contains(&ContextStatus::Fun) {
            if let Some(expr) = &stmt.value {
                ExprResolver::resolve(self, expr)?;
            }
            return Ok(());
        }
        Err(JokerError::Resolver(ResolverError::KeyWord(
            KeyWordError::Pos(PosError::report_error(
                &stmt.keyword,
                String::from("Cannot use 'return' outside of a function statement."),
            )),
        )))
    }
}

impl ExprVisitor<()> for Resolver {
    fn visit_literal(&self, _expr: &Literal) -> Result<(), JokerError> {
        Ok(())
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
    fn visit_grouping(&self, expr: &Grouping) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.expr)?;
        Ok(())
    }
    fn visit_variable(&self, expr: &Variable) -> Result<(), JokerError> {
        let key = Key(expr.name.clone());
        match self.var_stores.borrow().try_block_borrow()?.stores.last() {
            Some(block_store) => {
                if let Some(_index) = block_store.try_block_borrow()?.indexes.get(&key) {
                    return Err(JokerError::Resolver(ResolverError::Var(VarError::Init(
                        InitError::report_error(
                            &expr.name,
                            String::from("Can't read local variable in its own initializer."),
                        ),
                    ))));
                }
            }
            None => {
                return Err(JokerError::Resolver(ResolverError::Var(VarError::Init(
                    InitError::report_error(&expr.name, String::from("find error block store.")),
                ))))
            }
        };
        self.resolve_local(Expr::Variable(expr.clone()), &key)?;
        Ok(())
    }
    fn visit_assign(&self, expr: &Assign) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.value)?;
        ExprResolver::resolve_local(self, Expr::Assign(expr.clone()), &Key(expr.name.clone()))?;
        Ok(())
    }
    fn visit_logical(&self, expr: &Logical) -> Result<(), JokerError> {
        ExprResolver::resolve(self, &expr.l_expr)?;
        ExprResolver::resolve(self, &expr.r_expr)?;
        Ok(())
    }
    fn visit_trinomial(&self, expr: &Trinomial) -> Result<(), JokerError> {
        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_trinomial", self.context_stack
        );

        ExprResolver::resolve(self, &expr.condition)?;
        ExprResolver::resolve(self, &expr.l_expr)?;
        ExprResolver::resolve(self, &expr.r_expr)?;

        println!(
            "[{:>10}][{:>20}]:\tstack: {:?}",
            "resolve", "visit_trinomial", self.context_stack
        );
        Ok(())
    }
    fn visit_call(&self, expr: &Call) -> Result<(), JokerError> {
        ExprResolver::resolve_call(self, expr)?;
        Ok(())
    }
    fn visit_lambda(&self, expr: &Lambda) -> Result<(), JokerError> {
        ExprResolver::resolve_lambda(self, expr)?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum ResolverError {
    Store(VarStoreError),
    Env(EnvError),
    Var(VarError),
    KeyWord(KeyWordError),
    Struct(StructError),
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolverError::Store(store) => Display::fmt(store, f),
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
            ResolverError::Store(store) => ReportError::report(store),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::joker::{object::literal_null, token::TokenType};

    #[test]
    fn test_var_store() {
        let mut var_stores: VarStore = VarStore::Block(VarBlock {
            stores: vec![
                VarStore::Var(VarItem {
                    key: Key(Token::new(
                        TokenType::Str,
                        String::from("var_0"),
                        literal_null(),
                        0,
                    )),
                    layer: VarLayer { layer: 0, index: 0 },
                    status: VarStatus::Declare,
                }),
                VarStore::Var(VarItem {
                    key: Key(Token::new(
                        TokenType::Str,
                        String::from("var_1"),
                        literal_null(),
                        1,
                    )),
                    layer: VarLayer { layer: 0, index: 1 },
                    status: VarStatus::Declare,
                }),
                VarStore::Var(VarItem {
                    key: Key(Token::new(
                        TokenType::Str,
                        String::from("var_2"),
                        literal_null(),
                        2,
                    )),
                    layer: VarLayer { layer: 0, index: 2 },
                    status: VarStatus::Declare,
                }),
                VarStore::Block(VarBlock {
                    stores: vec![
                        VarStore::Var(VarItem {
                            key: Key(Token::new(
                                TokenType::Str,
                                String::from("var_3_0"),
                                literal_null(),
                                3,
                            )),
                            layer: VarLayer { layer: 1, index: 0 },
                            status: VarStatus::Declare,
                        }),
                        VarStore::Var(VarItem {
                            key: Key(Token::new(
                                TokenType::Str,
                                String::from("var_3_1"),
                                literal_null(),
                                4,
                            )),
                            layer: VarLayer { layer: 1, index: 1 },
                            status: VarStatus::Declare,
                        }),
                        VarStore::Var(VarItem {
                            key: Key(Token::new(
                                TokenType::Str,
                                String::from("var_3_2"),
                                literal_null(),
                                5,
                            )),
                            layer: VarLayer { layer: 1, index: 2 },
                            status: VarStatus::Declare,
                        }),
                        VarStore::Block(VarBlock {
                            stores: vec![
                                VarStore::Var(VarItem {
                                    key: Key(Token::new(
                                        TokenType::Str,
                                        String::from("var_3_3_0"),
                                        literal_null(),
                                        6,
                                    )),
                                    layer: VarLayer { layer: 2, index: 0 },
                                    status: VarStatus::Declare,
                                }),
                                VarStore::Var(VarItem {
                                    key: Key(Token::new(
                                        TokenType::Str,
                                        String::from("var_3_3_1"),
                                        literal_null(),
                                        7,
                                    )),
                                    layer: VarLayer { layer: 2, index: 1 },
                                    status: VarStatus::Declare,
                                }),
                                VarStore::Var(VarItem {
                                    key: Key(Token::new(
                                        TokenType::Str,
                                        String::from("var_3_3_2"),
                                        literal_null(),
                                        8,
                                    )),
                                    layer: VarLayer { layer: 2, index: 2 },
                                    status: VarStatus::Declare,
                                }),
                            ],
                            indexes: HashMap::new(),
                        }),
                    ],
                    indexes: HashMap::new(),
                }),
            ],
            indexes: HashMap::new(),
        });

        // get
        println!("get var_stores:");
        println!("var_stores: {:?}", var_stores);
        println!("var_stores[3][1]: {:?}", var_stores[3][1]);
        // set
        println!("set var_stores:");
        var_stores[3][1] = VarStore::Var(VarItem {
            key: Key(Token::new(
                TokenType::Str,
                String::from("var_3_1"),
                literal_null(),
                4,
            )),
            layer: VarLayer { layer: 1, index: 1 },
            status: VarStatus::Declare,
        });
        println!("var_stores[3][1] = VarStore::Var(Token {{ ttype: TokenType::Eof, lexeme: String::from(\"update_3_1\"), line: 4 }});");
        println!("get var_stores:");
        println!("var_stores[3][1]: {:?}", var_stores[3][1]);
        // push
        println!("get var_stores len: {}", var_stores.len());
        if let Err(err) = var_stores.push(VarStore::Var(VarItem {
            key: Key(Token::new(
                TokenType::Str,
                String::from("var_9"),
                literal_null(),
                0,
            )),
            layer: VarLayer { layer: 0, index: 4 },
            status: VarStatus::Declare,
        })) {
            println!("push error: {}", err);
        };
        // get
        println!("get var_stores:");
        println!("get var_stores len: {}", var_stores.len());
        println!("var_stores: {:?}", var_stores);
        // pop
        println!("get var_stores len: {}", var_stores.len());
        match var_stores.pop() {
            Ok(op_store) => println!("pop ok: {:?}", op_store),
            Err(err) => println!("pop error: {}", err),
        }
        // get
        println!("get var_stores:");
        println!("get var_stores len: {}", var_stores.len());
        println!("var_stores: {:?}", var_stores);
        // var store get fn
        println!("fn get var_stores:");
        println!("fn get var_stores len: {}", var_stores.len());
        println!(
            "fn get last key: {:?}",
            var_stores.get(var_stores.len() - 1)
        );
        println!("var_stores: {:?}", var_stores);
    }

    #[test]
    fn test_resolve_base() -> Result<(), JokerError> {
        let interpreter = Rc::new(Interpreter::new());
        let resolve = Resolver {
            interpreter,
            var_stores: RefCell::new(VarStore::Block(VarBlock {
                stores: vec![
                    VarStore::Block(VarBlock {
                        stores: vec![VarStore::Var(VarItem {
                            key: Key(Token::new(
                                TokenType::Str,
                                String::from("var_0"),
                                literal_null(),
                                0,
                            )),
                            layer: VarLayer::default(),
                            status: VarStatus::default(),
                        })],
                        indexes: HashMap::from([(
                            Key(Token::new(
                                TokenType::Str,
                                String::from("var_0"),
                                literal_null(),
                                0,
                            )),
                            0,
                        )]),
                    }),
                    VarStore::Var(VarItem {
                        key: Key(Token::new(
                            TokenType::Str,
                            String::from("var_1"),
                            literal_null(),
                            1,
                        )),
                        layer: VarLayer::default(),
                        status: VarStatus::default(),
                    }),
                ],
                indexes: HashMap::from([
                    (
                        Key(Token::new(
                            TokenType::Str,
                            String::from("var_0"),
                            literal_null(),
                            0,
                        )),
                        0,
                    ),
                    (
                        Key(Token::new(
                            TokenType::Str,
                            String::from("var_1"),
                            literal_null(),
                            1,
                        )),
                        1,
                    ),
                ]),
            })),
            context_stack: RefCell::new(Vec::new()),
        };

        println!("resolve: {:#?}", resolve);
        // find
        println!(
            "resolve: {:#?}",
            resolve.var_stores.borrow().try_block_borrow()?.get(
                *resolve
                    .var_stores
                    .borrow()
                    .try_block_borrow()?
                    .indexes
                    .get(&Key(Token::new(
                        TokenType::Str,
                        String::from("var_0"),
                        literal_null(),
                        0
                    )))
                    .unwrap()
            )
        );
        // push
        let next_pos = resolve.var_stores.borrow().try_block_borrow()?.len();
        resolve
            .var_stores
            .borrow_mut()
            .try_block_borrow_mut()?
            .push(VarStore::Var(VarItem {
                key: Key(Token::new(
                    TokenType::Str,
                    String::from("var_1"),
                    literal_null(),
                    1,
                )),
                layer: VarLayer {
                    layer: 0,
                    index: next_pos,
                },
                status: VarStatus::Declare,
            }));
        resolve
            .var_stores
            .borrow_mut()
            .try_block_borrow_mut()?
            .indexes
            .insert(
                Key(Token::new(
                    TokenType::Str,
                    String::from("var_1"),
                    literal_null(),
                    1,
                )),
                next_pos,
            );
        // show
        println!("resolve: {:#?}", resolve);
        // update
        let index = *resolve
            .var_stores
            .borrow()
            .try_block_borrow()?
            .indexes
            .get(&Key(Token::new(
                TokenType::Str,
                String::from("var_1"),
                literal_null(),
                1,
            )))
            .unwrap();
        match resolve
            .var_stores
            .borrow_mut()
            .try_block_borrow_mut()?
            .stores
            .get_mut(index)
        {
            Some(var_store) => {
                var_store.try_var_borrow_mut()?.status = VarStatus::Used;
                println!("var_store: {:?}", var_store);
            }
            None => {
                eprintln!("resolve update error.")
            }
        }
        Ok(())
    }
}
