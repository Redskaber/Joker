//! This file is print ast expr!
//!
//!

use super::{
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Expr, ExprAcceptor, ExprStmt, ExprVisitor, ForStmt,
        Grouping, IfStmt, Literal, Logical, PrintStmt, Stmt, StmtAcceptor, StmtVisitor, Trinomial,
        Unary, VarStmt, Variable, WhileStmt,
    },
    error::{JokerError, ReportError},
    object::Object,
};

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> AstPrinter {
        AstPrinter
    }
    pub fn println(&self, stmt: &Stmt) {
        match self.ast_stmt(stmt) {
            Ok(expr) => println!("{expr}"),
            Err(err) => err.report(),
        }
    }
    fn ast_stmt(&self, stmt: &Stmt) -> Result<String, JokerError> {
        stmt.accept(self)
    }
    fn parenthesize(&self, label: &str, exprs: &[&Expr]) -> Result<String, JokerError> {
        let mut result = String::new();

        result.push('(');
        result.push_str(label);
        for expr in exprs {
            result.push(' ');
            match expr.accept(self) {
                Ok(sub_string) => result.push_str(&sub_string),
                Err(err) => return Err(err),
            }
        }
        result.push(')');

        Ok(result)
    }
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_expr(&self, stmt: &ExprStmt) -> Result<String, JokerError> {
        stmt.expr.accept(self)
    }
    fn visit_print(&self, stmt: &PrintStmt) -> Result<String, JokerError> {
        stmt.expr.accept(self)
    }
    fn visit_var(&self, stmt: &VarStmt) -> Result<String, JokerError> {
        match stmt.value.accept(self) {
            Ok(value) => Ok(format!("VarStmt({} = {})", stmt.name.lexeme, value)),
            Err(err) => Err(err),
        }
    }
    fn visit_block(&self, stmt: &BlockStmt) -> Result<String, JokerError> {
        let mut result: String = String::from("BlockStmt{ ");
        for st in &stmt.stmts {
            match st.accept(self) {
                Ok(value) => result.push_str(&value),
                Err(err) => return Err(err),
            }
            result.push(';');
        }
        result.push_str(" }");
        Ok(result)
    }
    fn visit_if(&self, stmt: &IfStmt) -> Result<String, JokerError> {
        Ok(format!(
            "IfStmt(cond: {}, then: {}, else: {})",
            stmt.condition.accept(self)?,
            stmt.then_branch.accept(self)?,
            match &stmt.else_branch {
                Some(value) => format!("Some({})", value.accept(self)?),
                None => String::from("None"),
            },
        ))
    }
    fn visit_while(&self, stmt: &WhileStmt) -> Result<String, JokerError> {
        Ok(format!(
            "WhileStmt(cond: {}, body: {})",
            stmt.condition.accept(self)?,
            stmt.body.accept(self)?,
        ))
    }
    fn visit_break(&self, _stmt: &BreakStmt) -> Result<String, JokerError> {
        Ok(String::from("BreakStmt"))
    }
    fn visit_continue(&self, _stmt: &super::ast::ContinueStmt) -> Result<String, JokerError> {
        Ok(String::from("ContinueStmt"))
    }
    fn visit_for(&self, stmt: &ForStmt) -> Result<String, JokerError> {
        Ok(format!(
            "ForStmt(initializer: {}, condition: {}, increment: {}, body: {})",
            match &stmt.initializer {
                Some(initializer) => format!("Some({})", initializer.accept(self)?),
                None => String::from("None"),
            },
            stmt.condition.accept(self)?,
            match &stmt.increment {
                Some(increment) => format!("Some({})", increment.accept(self)?),
                None => String::from("None"),
            },
            &stmt.body.accept(self)?,
        ))
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_literal(&self, expr: &Literal) -> Result<String, JokerError> {
        match &expr.value {
            Object::Literal(literal) => Ok(literal.to_string()),
        }
    }
    fn visit_unary(&self, expr: &Unary) -> Result<String, JokerError> {
        self.parenthesize(&expr.l_opera.lexeme, &[&expr.r_expr])
    }
    fn visit_binary(&self, expr: &Binary) -> Result<String, JokerError> {
        self.parenthesize(&expr.m_opera.lexeme, &[&expr.l_expr, &expr.r_expr])
    }
    fn visit_grouping(&self, expr: &Grouping) -> Result<String, JokerError> {
        self.parenthesize("Group", &[&expr.expr])
    }
    fn visit_variable(&self, expr: &Variable) -> Result<String, JokerError> {
        Ok(format!("Variable({})", expr.name.lexeme))
    }
    fn visit_assign(&self, expr: &Assign) -> Result<String, JokerError> {
        Ok(format!(
            "Assign({} = {})",
            expr.name.lexeme,
            expr.value.accept(self)?,
        ))
    }
    fn visit_logical(&self, expr: &Logical) -> Result<String, JokerError> {
        Ok(format!(
            "Logical({} {} {})",
            expr.l_expr.accept(self)?,
            expr.m_opera.lexeme,
            expr.r_expr.accept(self)?,
        ))
    }
    fn visit_trinomial(&self, stmt: &Trinomial) -> Result<String, JokerError> {
        Ok(format!(
            "TrinomialStmt(cond: {}, l_stmt: {}, r_stmt: {})",
            stmt.condition.accept(self)?,
            stmt.l_expr.accept(self)?,
            stmt.r_expr.accept(self)?,
        ))
    }
}

#[cfg(test)]
mod test {

    use crate::joker::object::literal_null;

    use super::{
        super::{
            object::{literal_f64, literal_i32},
            token::{Token, TokenType},
        },
        *,
    };

    #[test]
    fn create_some_expr() -> Result<(), JokerError> {
        let ast_printer: AstPrinter = AstPrinter::new();

        let binary: Expr = Expr::Binary(Binary::new(
            Box::new(Expr::Unary(Unary::new(
                Token::new(TokenType::Minus, String::from("-"), literal_null(), 0),
                Box::new(Expr::Literal(Literal::new(literal_f64(123.0)))),
            ))),
            Token::new(TokenType::Slash, String::from("/"), literal_null(), 0),
            Box::new(Expr::Grouping(Grouping::new(Box::new(Expr::Literal(
                Literal::new(literal_f64(123.0)),
            ))))),
        ));
        let stmt = Stmt::ExprStmt(ExprStmt::new(binary));
        println!("stmt: {stmt}");
        println!("ast: {}", ast_printer.ast_stmt(&stmt)?);
        assert_eq!("(/ (- 123) (group 123))", ast_printer.ast_stmt(&stmt)?);

        let other: Expr = Expr::Binary(Binary {
            l_expr: Box::new(Expr::Unary(Unary {
                l_opera: Token {
                    ttype: TokenType::Minus,
                    lexeme: String::from("-"),
                    literal: literal_null(),
                    line: 0,
                },
                r_expr: Box::new(Expr::Literal(Literal {
                    value: literal_i32(123),
                })),
            })),
            m_opera: Token {
                ttype: TokenType::Star,
                lexeme: String::from("*"),
                literal: literal_null(),
                line: 0,
            },
            r_expr: Box::new(Expr::Grouping(Grouping {
                expr: Box::new(Expr::Literal(Literal {
                    value: literal_f64(45.67),
                })),
            })),
        });
        let o_stmt = Stmt::ExprStmt(ExprStmt::new(other));
        println!("o_stmt: {o_stmt}");
        println!("ast: {}", ast_printer.ast_stmt(&o_stmt)?);
        assert_eq!("(* (- 123) (group 45.67))", ast_printer.ast_stmt(&o_stmt)?);

        Ok(())
    }
}
