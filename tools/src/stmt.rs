use super::ast::Expr;
use super::error::JokerError;


pub enum Stmt {
    ExprStmt(ExprStmt),
    PrintStmt(PrintStmt),
}

pub struct ExprStmt {
    pub expr: Expr,
}

pub struct PrintStmt {
    pub expr: Expr,
}

impl<T> StmtVisitor<T> for Stmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        match self {
            Stmt::ExprStmt(exprstmt) => exprstmt.accept(visitor),
            Stmt::PrintStmt(printstmt) => printstmt.accept(visitor),
        }
    }
}

pub trait StmtVisitor<T> {
    fn visit_exprstmt(&self, expr: &ExprStmt) -> Result<T, JokerError>;
    fn visit_printstmt(&self, expr: &PrintStmt) -> Result<T, JokerError>;
}

pub trait StmtAcceptor<T> {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError>;
}

impl<T> StmtAcceptor<T> for ExprStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_exprstmt(self)
    }
}

impl<T> StmtAcceptor<T> for PrintStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_printstmt(self)
    }
}

