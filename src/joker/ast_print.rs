//! This file is print ast expr!
//!
//!

use super::{
    ast::{Binary, Expr, ExprAcceptor, ExprVisitor, Grouping, Literal, Unary},
    error::{JokerError, ReportError}, object::Object,
};

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> AstPrinter {
        AstPrinter
    }
    pub fn println(&self, expr: &Expr) {
        match self.ast_expr(expr) {
            Ok(expr) => println!("{expr}"),
            Err(err) => err.report(),
        }
    } 
    pub fn ast_expr(&self, expr: &Expr) -> Result<String, JokerError> {
        expr.accept(self)
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
        println!("binary: {binary}");
        println!("ast: {}", ast_printer.ast_expr(&binary)?);
        assert_eq!("(/ (- 123) (group 123))", ast_printer.ast_expr(&binary)?);

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
        println!("other: {other}");
        println!("ast: {}", ast_printer.ast_expr(&other)?);
        assert_eq!("(* (- 123) (group 45.67))", ast_printer.ast_expr(&other)?);

        Ok(())
    }
}
