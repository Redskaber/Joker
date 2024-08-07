//! This file is parse.rs
//!
//!
//!

use super::{
    ast::{
        Assign, Binary, BlockStmt, Expr, ExprStmt, Grouping, IfStmt, Literal, PrintStmt, Stmt,
        Unary, VarStmt, Variable,
    },
    error::{JokerError, ReportError},
    object::literal_null,
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
    pub fn parse(&mut self) -> Option<Vec<Stmt>> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    err.report();
                    if self.is_at_end() {
                        break;
                    } // input: cc -> cc + Eof(now pos)
                    self.synchronize(); // upcast: declaration -> this
                }
            }
        }
        if stmts.is_empty() {
            return None;
        }
        Some(stmts)
    }

    // declaration -> stmt              （语句）
    //               | var_declaration  (声明)
    fn declaration(&mut self) -> Result<Stmt, JokerError> {
        if self.is_match(&[TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }
    fn var_declaration(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.consume(
            &TokenType::Identifier,
            String::from("Expect variable name."),
        )?;
        let value: Expr = if self.is_match(&[TokenType::Equal]) {
            self.expression()?
        } else {
            Expr::Literal(Literal::new(literal_null()))
        };
        self.consume(
            &TokenType::Semicolon,
            String::from("Expect ';' after variable declaration."),
        )?;
        Ok(VarStmt::upcast(name, value))
    }
    // stmt -> print_stmt
    //        | expr_stmt
    //        | block_stmt
    //        | if_stmt;
    fn statement(&mut self) -> Result<Stmt, JokerError> {
        if self.is_match(&[TokenType::If]) {
            return self.if_statement();
        }
        if self.is_match(&[TokenType::Print]) {
            return self.print_statement();
        }
        if self.is_match(&[TokenType::LeftBrace]) {
            return self.block_statement();
        }
        self.expr_statement()
    }
    //
    fn if_statement(&mut self) -> Result<Stmt, JokerError> {
        self.consume(
            &TokenType::LeftParen,
            String::from("Expect '(' after 'if'."),
        )?;
        let condition: Expr = self.expression()?;
        self.consume(
            &TokenType::RightParen,
            String::from("Expect ')' after if condition."),
        )?;
        let then_branch: Stmt = self.statement()?;
        let else_branch: Stmt = if self.is_match(&[TokenType::Else]) {
            self.statement()?
        } else {
            ExprStmt::upcast(Expr::Literal(Literal::new(literal_null())))
        };
        Ok(IfStmt::upcast(
            condition,
            Box::new(then_branch),
            Box::new(else_branch),
        ))
    }
    fn print_statement(&mut self) -> Result<Stmt, JokerError> {
        let expr: Expr = self.expression()?;
        self.consume(
            &TokenType::Semicolon,
            String::from("Expect ';' after value."),
        )?;
        Ok(PrintStmt::upcast(expr))
    }
    // block_stmt          → "{" declaration* "}" ;
    fn block_statement(&mut self) -> Result<Stmt, JokerError> {
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(
            &TokenType::RightBrace,
            String::from("Expect '}' after block."),
        )?;
        Ok(BlockStmt::upcast(stmts))
    }
    fn expr_statement(&mut self) -> Result<Stmt, JokerError> {
        let expr: Expr = self.expression()?;
        self.consume(
            &TokenType::Semicolon,
            String::from("Expect ';' after value."),
        )?;
        Ok(ExprStmt::upcast(expr))
    }
    // exprStmt     → assignment ;
    // assignment   → IDENTIFIER "=" assignment
    //               | equality ;
    fn expression(&mut self) -> Result<Expr, JokerError> {
        self.assignment()
    }
    fn assignment(&mut self) -> Result<Expr, JokerError> {
        let expr: Expr = self.equality()?;
        if self.is_match(&[TokenType::Equal]) {
            let equal: Token = self.previous();
            let value: Expr = self.assignment()?;
            match expr {
                Expr::Variable(variable) => {
                    return Ok(Assign::upcast(variable.name, Box::new(value)))
                }
                _ => {
                    return Err(JokerError::parse(
                        &equal,
                        String::from("Invalid assignment target."),
                    ))
                }
            }
        }
        Ok(expr)
    }
    // equality -> comparison ( ( "!=" | "==")  comparison )*
    fn equality(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.comparison()?;
        while self.is_match(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.comparison()?;
            expr = Binary::upcast(Box::new(expr), m_opera, Box::new(r_expr));
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
            expr = Binary::upcast(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // term -> factor ( ( "-" | "+" ) factor )* ;
    fn term(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.factor()?;
        while self.is_match(&[TokenType::Minus, TokenType::Plus]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.factor()?;
            expr = Binary::upcast(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // factor -> unary ( ( "/" | "*" ) unary )* ;
    fn factor(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.unary()?;
        while self.is_match(&[TokenType::Slash, TokenType::Star]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.unary()?;
            expr = Binary::upcast(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // unary -> ( "!" | "-" ) unary
    //          | grouping ;
    fn unary(&mut self) -> Result<Expr, JokerError> {
        if self.is_match(&[TokenType::Bang, TokenType::Minus]) {
            let l_opera: Token = self.previous();
            let r_expr: Expr = self.unary()?;
            return Ok(Unary::upcast(l_opera, Box::new(r_expr)));
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
            return Ok(Grouping::upcast(Box::new(expr)));
        }
        self.primary()
    }
    // primary -> I32| F64 | STRING | "true" | "false" | "null"
    //          | IDENTIFIER ;
    fn primary(&mut self) -> Result<Expr, JokerError> {
        match self.peek().ttype {
            TokenType::False => Ok(Literal::upcast(self.advance().literal)),
            TokenType::True => Ok(Literal::upcast(self.advance().literal)),
            TokenType::Null => Ok(Literal::upcast(self.advance().literal)),
            TokenType::I32 => Ok(Literal::upcast(self.advance().literal)),
            TokenType::F64 => Ok(Literal::upcast(self.advance().literal)),
            TokenType::Str => Ok(Literal::upcast(self.advance().literal)),
            TokenType::Identifier => Ok(Variable::upcast(self.advance())),
            _ => Err(JokerError::parse(
                &self.advance(),
                String::from("parse not impl!"),
            )), // jump
        }
    }
    fn consume(&mut self, expected: &TokenType, msg: String) -> Result<Token, JokerError> {
        if self.check(expected) {
            Ok(self.advance())
        } else {
            Err(JokerError::parse(&self.peek(), msg))
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
