//! This file is parse.rs
//!
//!
//!

use super::{
    ast::{Binary, Expr, Grouping, Literal, Unary},
    error::{JokerError, ReportError},
    token::{Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, current: 0 }
    }
    fn is_at_end(&self) -> bool {
        self.peek().ttype == TokenType::Eof
    }
    fn peek(&self) -> Token {
        self.tokens.get(self.current).unwrap().clone()
    }
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    fn previous(&self) -> Token {
        self.tokens.get(self.current - 1).unwrap().clone()
    }
    fn check(&self, ttype: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            &self.peek().ttype == ttype
        }
    }
    fn is_match(&mut self, types: &[TokenType]) -> bool {
        for ttype in types {
            if self.check(ttype) {
                self.advance();
                return true;
            }
        }
        false
    }
    pub fn parse(&mut self) -> Option<Expr> {
        match self.expression() {
            Ok(expr) => Some(expr),
            Err(err) => {
                err.report();
                None
            }
        }
    }
    // stmt -> stmt ? stmt : stmt;
    //        | exprStmt;
    // let stmt = self.stmt()?;
    // if self.is_match(&[TokenType::Question]) {
    //     let m_stmt: Stmt = self.stmt()?;
    //     if self.is_match(&[TokenType::Colon]) {
    //         let r_stmt: Stmt = self.stmt()?;
    //         return Ok(Trinomial::into_expr(Box::new(stmt), Box::new(m_stmt), Box::new(r_stmt)));
    //     }
    //     return Err(ParseError::new(self.previous(), String::from("Expected ':' after '?'")));
    // }
    // Ok(expr)
    //
    // exprStmt -> equality
    fn expression(&mut self) -> Result<Expr, JokerError> {
        self.equality()
    }
    // equality -> comparison ( ( "!=" | "==")  comparison )*
    fn equality(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.comparison()?;
        while self.is_match(&[TokenType::BangEqual, TokenType::Equal]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.comparison()?;
            expr = Binary::into_expr(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // comparison -> term ( ( ">" | ">=" | "<" | "<=") term )*;
    fn comparison(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.term()?;
        while self.is_match(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.term()?;
            expr = Binary::into_expr(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // term -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.factor()?;
        while self.is_match(&[TokenType::Minus, TokenType::Plus]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.factor()?;
            expr = Binary::into_expr(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.unary()?;
        while self.is_match(&[TokenType::Slash, TokenType::Star]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.unary()?;
            expr = Binary::into_expr(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // unary -> ( "!" | "-" ) unary
    //          | grouping ;
    fn unary(&mut self) -> Result<Expr, JokerError> {
        if self.is_match(&[TokenType::Bang, TokenType::Minus]) {
            let l_opera: Token = self.previous();
            let r_expr: Expr = self.unary()?;
            return Ok(Unary::into_expr(l_opera, Box::new(r_expr)));
        }
        self.grouping()
    }
    // grouping -> "(" expression ")" ;
    //              | primary ;
    fn grouping(&mut self) -> Result<Expr, JokerError> {
        if self.is_match(&[TokenType::LeftParen]) {
            let expr: Expr = self.expression()?;
            self.consume(
                &TokenType::RightParen,
                String::from("Expect ')' after expression."),
            )?;
            return Ok(Grouping::into_expr(Box::new(expr)));
        }
        self.primary()
    }
    // primary -> I32| F64 | STRING | "true" | "false" | "null"
    //          | IDENTIFIER ;
    fn primary(&mut self) -> Result<Expr, JokerError> {
        match self.peek().ttype {
            TokenType::False => Ok(Literal::into_expr(self.advance().literal)),
            TokenType::True => Ok(Literal::into_expr(self.advance().literal)),
            TokenType::Null => Ok(Literal::into_expr(self.advance().literal)),
            TokenType::I32 => Ok(Literal::into_expr(self.advance().literal)),
            TokenType::F64 => Ok(Literal::into_expr(self.advance().literal)),
            TokenType::Str => Ok(Literal::into_expr(self.advance().literal)),
            _ => Err(JokerError::new(&self.peek(), String::from("not impl!"))),
        }
    }
    fn consume(&mut self, expected: &TokenType, msg: String) -> Result<Token, JokerError> {
        if self.check(expected) {
            Ok(self.advance())
        } else {
            Err(JokerError::new(&self.peek(), msg))
        }
    }
    fn synchronize(&mut self) {
        self.advance();
        while self.is_at_end() {
            if self.previous().ttype == TokenType::Semicolon {
                return;
            }
            match self.peek().ttype {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }
            self.advance();
        }
    }
}
