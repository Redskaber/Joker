use super::super::{
    r#type::Object,
    error::JokerError,
    token::Token,
};


pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
}

pub struct Literal {
    pub value: Option<Object>,
}

pub struct Unary {
    pub l_opera: Token,
    pub r_expr: Box<Expr>,
}

pub struct Binary {
    pub l_expr: Box<Expr>,
    pub m_opera: Token,
    pub r_expr: Box<Expr>,
}

pub struct Grouping {
    pub expr: Box<Expr>,
}

impl<T> ExprVisitor<T> for Expr {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        match self {
            Expr::Literal(literal) => literal.accept(visitor),
            Expr::Unary(unary) => unary.accept(visitor),
            Expr::Binary(binary) => binary.accept(visitor),
            Expr::Grouping(grouping) => grouping.accept(visitor),
        }
    }
}

pub trait ExprVisitor<T> {
    fn visit_literal(&self, expr: &Literal) -> Result<T, JokerError>;
    fn visit_unary(&self, expr: &Unary) -> Result<T, JokerError>;
    fn visit_binary(&self, expr: &Binary) -> Result<T, JokerError>;
    fn visit_grouping(&self, expr: &Grouping) -> Result<T, JokerError>;
}

pub trait ExprAccept<T> {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError>;
}

impl<T> ExprAccept<T> for Literal {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_literal(self)
    }
}

impl<T> ExprAccept<T> for Unary {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_unary(self)
    }
}

impl<T> ExprAccept<T> for Binary {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_binary(self)
    }
}

impl<T> ExprAccept<T> for Grouping {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_grouping(self)
    }
}

