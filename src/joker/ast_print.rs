//! This file is print ast expr!
//!
//!

use super::{
    ast::{
        Binary, Expr, ExprAcceptor, ExprVisitor, Grouping, Literal, Variable,
        Stmt, StmtAcceptor, StmtVisitor, Unary, ExprStmt, PrintStmt, VarStmt},
    error::{JokerError, ReportError}, object::Object,
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
    fn visit_expr(&self, stmt: &ExprStmt) -> Result<String,JokerError> {
        stmt.expr.accept(self)
    }
    fn visit_print(&self, stmt: &PrintStmt) -> Result<String,JokerError> {
        stmt.expr.accept(self)
    }
    fn visit_var(&self, stmt: &VarStmt) -> Result<String,JokerError> {
        match stmt.value.accept(self) {
            Ok(value) => Ok(format!("var {} = {};", stmt.name.lexeme, value)),
            Err(err) => Err(err),
        }
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_literal(&self, expr: &Literal) -> Result<String, JokerError> {
        match &expr.value {
            Object::Literal(literal) => Ok(literal.to_string())
        }
    }
    fn visit_unary(&self, expr: &Unary) -> Result<String, JokerError> {
        self.parenthesize(&expr.l_opera.lexeme, &[&expr.r_expr])
    }
    fn visit_binary(&self, expr: &Binary) -> Result<String, JokerError> {
        self.parenthesize(&expr.m_opera.lexeme, &[&expr.l_expr, &expr.r_expr])
    }
    fn visit_grouping(&self, expr: &Grouping) -> Result<String, JokerError> {
        self.parenthesize("group", &[&expr.expr])
    }
    fn visit_variable(&self,expr: &Variable) -> Result<String,JokerError> {
        self.parenthesize(&format!("variable({})", expr.name.lexeme), &[])
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
