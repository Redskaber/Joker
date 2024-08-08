use super::object::Object;
use super::token::Token;
use super::error::JokerError;


pub enum Expr {
    Literal(Literal),
    Unary(Unary),
    Binary(Binary),
    Grouping(Grouping),
    Variable(Variable),
    Assign(Assign),
    Logical(Logical),
    Trinomial(Trinomial),
}

pub struct Literal {
    pub value: Object,
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

pub struct Variable {
    pub name: Token,
}

pub struct Assign {
    pub name: Token,
    pub value: Box<Expr>,
}

pub struct Logical {
    pub l_expr: Box<Expr>,
    pub m_opera: Token,
    pub r_expr: Box<Expr>,
}

pub struct Trinomial {
    pub condition: Box<Expr>,
    pub l_expr: Box<Expr>,
    pub r_expr: Box<Expr>,
}

impl<T> ExprVisitor<T> for Expr {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        match self {
            Expr::Literal(literal) => literal.accept(visitor),
            Expr::Unary(unary) => unary.accept(visitor),
            Expr::Binary(binary) => binary.accept(visitor),
            Expr::Grouping(grouping) => grouping.accept(visitor),
            Expr::Variable(variable) => variable.accept(visitor),
            Expr::Assign(assign) => assign.accept(visitor),
            Expr::Logical(logical) => logical.accept(visitor),
            Expr::Trinomial(trinomial) => trinomial.accept(visitor),
        }
    }
}

pub trait ExprVisitor<T> {
    fn visit_literal(&self, expr: &Literal) -> Result<T, JokerError>;
    fn visit_unary(&self, expr: &Unary) -> Result<T, JokerError>;
    fn visit_binary(&self, expr: &Binary) -> Result<T, JokerError>;
    fn visit_grouping(&self, expr: &Grouping) -> Result<T, JokerError>;
    fn visit_variable(&self, expr: &Variable) -> Result<T, JokerError>;
    fn visit_assign(&self, expr: &Assign) -> Result<T, JokerError>;
    fn visit_logical(&self, expr: &Logical) -> Result<T, JokerError>;
    fn visit_trinomial(&self, expr: &Trinomial) -> Result<T, JokerError>;
}

pub trait ExprAcceptor<T> {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError>;
}

impl<T> ExprAcceptor<T> for Literal {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_literal(self)
    }
}

impl<T> ExprAcceptor<T> for Unary {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_unary(self)
    }
}

impl<T> ExprAcceptor<T> for Binary {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_binary(self)
    }
}

impl<T> ExprAcceptor<T> for Grouping {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_grouping(self)
    }
}

impl<T> ExprAcceptor<T> for Variable {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_variable(self)
    }
}

impl<T> ExprAcceptor<T> for Assign {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_assign(self)
    }
}

impl<T> ExprAcceptor<T> for Logical {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_logical(self)
    }
}

impl<T> ExprAcceptor<T> for Trinomial {
    fn accept(&self, visitor: &dyn ExprVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_trinomial(self)
    }
}

