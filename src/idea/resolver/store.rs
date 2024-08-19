//! This is resolver store rs
//!
//!
//!

use std::{
    collections::HashMap,
    error::Error,
    fmt::Display,
    ops::{Index, IndexMut},
};

use crate::joker::{
    error::{JokerError, ReportError},
    token::{Token, TokenType},
};

use super::ResolverError;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum VarStore {
    Var(VarItem<Key, VarLayer, VarStatus>),
    Block(VarBlock),
}

impl VarStore {
    pub fn get_token(&self) -> Option<&Token> {
        match self {
            VarStore::Var(var) => Some(var.key.inner()),
            VarStore::Block(_block) => {
                eprintln!("VarStore::Block get token is error!");
                None
            }
        }
    }
    pub fn len(&self) -> usize {
        match self {
            VarStore::Var(_) => 1,
            VarStore::Block(store) => store.len(),
        }
    }
    pub fn push(&mut self, var_store: VarStore) -> Result<(), JokerError> {
        match self {
            VarStore::Var(var) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Push(PushError::report_error(
                    var.key.inner(),
                    String::from("Don't VarStore::Var push VarStore!"),
                )),
            ))),
            VarStore::Block(store) => {
                store.push(var_store);
                Ok(())
            }
        }
    }
    pub fn pop(&mut self) -> Result<Option<VarStore>, JokerError> {
        match self {
            VarStore::Var(var) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Pop(PopError::report_error(
                    var.key.inner(),
                    String::from("Don't VarStore::Var pop VarStore!"),
                )),
            ))),
            VarStore::Block(store) => Ok(store.pop()),
        }
    }
    pub fn get(&self, index: usize) -> Result<Option<&VarStore>, JokerError> {
        match self {
            VarStore::Var(var) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    var.key.inner(),
                    String::from("Don't VarStore::Var get VarStore!"),
                )),
            ))),
            VarStore::Block(store) => Ok(store.get(index)),
        }
    }
    pub fn get_mut(&mut self, index: usize) -> Result<Option<&mut VarStore>, JokerError> {
        match self {
            VarStore::Var(var) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    var.key.inner(),
                    String::from("Don't VarStore::Var get VarStore!"),
                )),
            ))),
            VarStore::Block(store) => Ok(store.get_mut(index)),
        }
    }
    pub fn contains(&self, name: &VarStore) -> bool {
        match self {
            VarStore::Var(var) => {
                eprintln!("VarStore contains:\n\tvar: {:?}", var);
                false
            }
            VarStore::Block(block) => block.contains(name),
        }
    }
    pub fn last(&self) -> Option<&VarStore> {
        match self {
            VarStore::Var(var) => {
                eprintln!("VarStore last:\n\tvar: {:?}", var);
                None
            }
            VarStore::Block(block) => block.last(),
        }
    }
    pub fn last_mut(&mut self) -> Option<&mut VarStore> {
        match self {
            VarStore::Var(var) => {
                eprintln!("VarStore last_mut:\n\tvar: {:?}", var);
                None
            }
            VarStore::Block(block) => block.last_mut(),
        }
    }
    pub fn try_var_borrow(&self) -> Result<&VarItem<Key, VarLayer, VarStatus>, JokerError> {
        match self {
            VarStore::Var(var) => Ok(var),
            VarStore::Block(_) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    self.get_token().unwrap(),
                    String::from("try from: VarStore into VarItem error."),
                )),
            ))),
        }
    }
    pub fn try_var_borrow_mut(
        &mut self,
    ) -> Result<&mut VarItem<Key, VarLayer, VarStatus>, JokerError> {
        match self {
            VarStore::Var(var) => Ok(var),
            VarStore::Block(_) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    self.get_token().unwrap(),
                    String::from("try from: VarStore into VarItem error."),
                )),
            ))),
        }
    }
    pub fn try_block_borrow(&self) -> Result<&VarBlock, JokerError> {
        match self {
            VarStore::Var(_) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    self.get_token().unwrap(),
                    String::from("try from: VarStore into VarBlock error."),
                )),
            ))),
            VarStore::Block(block) => Ok(block),
        }
    }
    pub fn try_block_borrow_mut(&mut self) -> Result<&mut VarBlock, JokerError> {
        match self {
            VarStore::Var(_) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    self.get_token().unwrap(),
                    String::from("try from: VarStore into VarBlock error."),
                )),
            ))),
            VarStore::Block(block) => Ok(block),
        }
    }
}

impl Index<usize> for VarStore {
    type Output = VarStore;
    fn index(&self, index: usize) -> &Self::Output {
        match self {
            VarStore::Var(_var) => panic!("VarStore::Var can't used index!"),
            VarStore::Block(store) => &store[index],
        }
    }
}

impl IndexMut<usize> for VarStore {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match self {
            VarStore::Var(_var) => panic!("VarStore::Var can't used index mut!"),
            VarStore::Block(store) => &mut store[index],
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub enum VarStatus {
    #[default]
    Declare,
    Define,
    Used,
}

impl Display for VarStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarStatus::Declare => write!(f, "VarStatus::Declare"),
            VarStatus::Define => write!(f, "VarStatus::Define"),
            VarStatus::Used => write!(f, "VarStatus::Used"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct VarLayer {
    pub layer: usize,
    pub index: usize,
}

impl Display for VarLayer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "VarLayer(layer: {}, index: {})", self.layer, self.index)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarItem<T, L, S> {
    pub key: T,
    pub layer: L,
    pub status: S,
}

impl<T, L, S> Display for VarItem<T, L, S>
where
    T: Display,
    L: Display,
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "VarItem(var: ")?;
        Display::fmt(&self.key, f)?;
        write!(f, ", layer: ")?;
        Display::fmt(&self.layer, f)?;
        write!(f, ", status: ")?;
        Display::fmt(&self.status, f)?;
        write!(f, ")")
    }
}

impl TryFrom<VarStore> for VarItem<Key, VarLayer, VarStatus> {
    type Error = JokerError;
    fn try_from(value: VarStore) -> Result<Self, Self::Error> {
        match value {
            VarStore::Var(var) => Ok(var),
            VarStore::Block(_) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    value.get_token().unwrap(),
                    String::from("try from: VarStore into VarItem error."),
                )),
            ))),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarBlock {
    pub stores: Vec<VarStore>,
    pub indexes: HashMap<Key, usize>,
}
impl VarBlock {
    pub fn len(&self) -> usize {
        self.stores.len()
    }
    pub fn push(&mut self, var_store: VarStore) {
        self.stores.push(var_store);
    }
    pub fn pop(&mut self) -> Option<VarStore> {
        self.stores.pop()
    }
    pub fn get(&self, index: usize) -> Option<&VarStore> {
        self.stores.get(index)
    }
    pub fn get_mut(&mut self, index: usize) -> Option<&mut VarStore> {
        self.stores.get_mut(index)
    }
    pub fn contains(&self, name: &VarStore) -> bool {
        self.stores.contains(name)
    }
    pub fn last(&self) -> Option<&VarStore> {
        self.stores.last()
    }
    pub fn last_mut(&mut self) -> Option<&mut VarStore> {
        self.stores.last_mut()
    }
}
impl Index<usize> for VarBlock {
    type Output = VarStore;
    fn index(&self, index: usize) -> &Self::Output {
        &self.stores[index]
    }
}

impl IndexMut<usize> for VarBlock {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.stores[index]
    }
}

impl TryFrom<VarStore> for VarBlock {
    type Error = JokerError;
    fn try_from(value: VarStore) -> Result<Self, Self::Error> {
        match value {
            VarStore::Var(_) => Err(JokerError::Resolver(ResolverError::Store(
                VarStoreError::Get(GetError::report_error(
                    value.get_token().unwrap(),
                    String::from("try from: VarStore into VarItem error."),
                )),
            ))),
            VarStore::Block(block) => Ok(block),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Key(pub Token);
impl Key {
    pub fn inner(&self) -> &Token {
        &self.0
    }
    pub fn downcast(self) -> Token {
        self.0
    }
}

impl Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Key({})", self.0)
    }
}

#[derive(Debug)]
pub enum VarStoreError {
    Push(PushError),
    Pop(PopError),
    Get(GetError),
}

impl Display for VarStoreError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarStoreError::Push(push) => Display::fmt(push, f),
            VarStoreError::Pop(pop) => Display::fmt(pop, f),
            VarStoreError::Get(get) => Display::fmt(get, f),
        }
    }
}

impl ReportError for VarStoreError {
    fn report(&self) {
        match self {
            VarStoreError::Push(push) => ReportError::report(push),
            VarStoreError::Pop(pop) => ReportError::report(pop),
            VarStoreError::Get(get) => ReportError::report(get),
        }
    }
}

#[derive(Debug)]
pub struct PushError {
    line: usize,
    where_: String,
    msg: String,
}

impl PushError {
    pub fn new(token: &Token, msg: String) -> PushError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        PushError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> PushError {
        let arg_limit = PushError::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for PushError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PosError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl Error for PushError {}

impl ReportError for PushError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[derive(Debug)]
pub struct PopError {
    line: usize,
    where_: String,
    msg: String,
}

impl PopError {
    pub fn new(token: &Token, msg: String) -> PopError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        PopError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> PopError {
        let arg_limit = PopError::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for PopError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PosError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl Error for PopError {}

impl ReportError for PopError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[derive(Debug)]
pub struct GetError {
    line: usize,
    where_: String,
    msg: String,
}

impl GetError {
    pub fn new(token: &Token, msg: String) -> GetError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        GetError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> GetError {
        let arg_limit = GetError::new(token, msg);
        arg_limit.report();
        arg_limit
    }
}

impl Display for GetError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PosError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl Error for GetError {}

impl ReportError for GetError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
