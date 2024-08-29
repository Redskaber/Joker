use super::ast::Expr;
use super::types::Type;
use super::token::Token;
use super::error::JokerError;


pub enum Stmt {
    ExprStmt(ExprStmt),
    PrintStmt(PrintStmt),
    VarStmt(VarStmt),
    BlockStmt(BlockStmt),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    ForStmt(ForStmt),
    BreakStmt(BreakStmt),
    ContinueStmt(ContinueStmt),
    FnStmt(FnStmt),
    ReturnStmt(ReturnStmt),
    ClassStmt(ClassStmt),
}

pub struct ExprStmt {
    pub expr: Expr,
}

pub struct PrintStmt {
    pub expr: Expr,
}

pub struct VarStmt {
    pub name: Token,
    pub type_: Option<Type>,
    pub value: Expr,
}

pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Box<Stmt>,
}

pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

pub struct ForStmt {
    pub initializer: Option<Box<Stmt>>,
    pub condition: Expr,
    pub increment: Option<Expr>,
    pub body: Box<Stmt>,
}

pub struct BreakStmt {
    pub name: Token,
}

pub struct ContinueStmt {
    pub name: Token,
}

pub struct FnStmt {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Expr,
}

pub struct ClassStmt {
    pub name: Token,
    pub super_class: Option<Variable>,
    pub fields: Option<Vec<Stmt>>,
    pub class_methods: Option<Vec<Stmt>>,
    pub instance_methods: Option<Vec<Stmt>>,
    pub static_methods: Option<Vec<Stmt>>,
}

impl<T> StmtVisitor<T> for Stmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        match self {
            Stmt::ExprStmt(exprstmt) => exprstmt.accept(visitor),
            Stmt::PrintStmt(printstmt) => printstmt.accept(visitor),
            Stmt::VarStmt(varstmt) => varstmt.accept(visitor),
            Stmt::BlockStmt(blockstmt) => blockstmt.accept(visitor),
            Stmt::IfStmt(ifstmt) => ifstmt.accept(visitor),
            Stmt::WhileStmt(whilestmt) => whilestmt.accept(visitor),
            Stmt::ForStmt(forstmt) => forstmt.accept(visitor),
            Stmt::BreakStmt(breakstmt) => breakstmt.accept(visitor),
            Stmt::ContinueStmt(continuestmt) => continuestmt.accept(visitor),
            Stmt::FnStmt(fnstmt) => fnstmt.accept(visitor),
            Stmt::ReturnStmt(returnstmt) => returnstmt.accept(visitor),
            Stmt::ClassStmt(classstmt) => classstmt.accept(visitor),
        }
    }
}

pub trait StmtVisitor<T> {
    fn visit_exprstmt(&self, expr: &ExprStmt) -> Result<T, JokerError>;
    fn visit_printstmt(&self, expr: &PrintStmt) -> Result<T, JokerError>;
    fn visit_varstmt(&self, expr: &VarStmt) -> Result<T, JokerError>;
    fn visit_blockstmt(&self, expr: &BlockStmt) -> Result<T, JokerError>;
    fn visit_ifstmt(&self, expr: &IfStmt) -> Result<T, JokerError>;
    fn visit_whilestmt(&self, expr: &WhileStmt) -> Result<T, JokerError>;
    fn visit_forstmt(&self, expr: &ForStmt) -> Result<T, JokerError>;
    fn visit_breakstmt(&self, expr: &BreakStmt) -> Result<T, JokerError>;
    fn visit_continuestmt(&self, expr: &ContinueStmt) -> Result<T, JokerError>;
    fn visit_fnstmt(&self, expr: &FnStmt) -> Result<T, JokerError>;
    fn visit_returnstmt(&self, expr: &ReturnStmt) -> Result<T, JokerError>;
    fn visit_classstmt(&self, expr: &ClassStmt) -> Result<T, JokerError>;
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

impl<T> StmtAcceptor<T> for VarStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_varstmt(self)
    }
}

impl<T> StmtAcceptor<T> for BlockStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_blockstmt(self)
    }
}

impl<T> StmtAcceptor<T> for IfStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_ifstmt(self)
    }
}

impl<T> StmtAcceptor<T> for WhileStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_whilestmt(self)
    }
}

impl<T> StmtAcceptor<T> for ForStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_forstmt(self)
    }
}

impl<T> StmtAcceptor<T> for BreakStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_breakstmt(self)
    }
}

impl<T> StmtAcceptor<T> for ContinueStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_continuestmt(self)
    }
}

impl<T> StmtAcceptor<T> for FnStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_fnstmt(self)
    }
}

impl<T> StmtAcceptor<T> for ReturnStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_returnstmt(self)
    }
}

impl<T> StmtAcceptor<T> for ClassStmt {
    fn accept(&self, visitor: &dyn StmtVisitor<T>) -> Result<T, JokerError> {
        visitor.visit_classstmt(self)
    }
}

