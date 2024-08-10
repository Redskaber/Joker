//! This file is parse.rs
//!
//!
//!

use super::{
    abort::{AbortError, ArgLimitAbort, ArgumentAbort},
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Call, ContinueStmt, Expr, ExprStmt, ForStmt, FunStmt,
        Grouping, IfStmt, Literal, Logical, PrintStmt, Stmt, Trinomial, Unary, VarStmt, Variable,
        WhileStmt,
    },
    error::{JokerError, ReportError},
    object::{literal_bool, literal_null},
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
                Err(_err) => {
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
    //               | fun_declaration
    fn declaration(&mut self) -> Result<Stmt, JokerError> {
        if self.is_match(&[TokenType::Fun]) {
            return self.fun_declaration();
        }
        if self.is_match(&[TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }
    // fun_declaration        → "fun" function ;
    // function       → IDENTIFIER "(" parameters? ")" block ;
    fn fun_declaration(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.consume(
            &TokenType::Identifier,
            String::from("Expect function name."),
        )?;
        self.consume(
            &TokenType::LeftParen,
            String::from("Expect '(' after function name."),
        )?;

        let mut params: Vec<Token> = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    // warning:?
                    return Err(JokerError::Abort(AbortError::Argument(
                        ArgumentAbort::Limit(ArgLimitAbort::report_error(
                            &self.peek(),
                            String::from("Can't have more than 255 parameters."),
                        )),
                    )));
                }
                params.push(self.consume(
                    &TokenType::Identifier,
                    String::from("Expect parameter name."),
                )?);

                if self.is_match(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(
            &TokenType::RightParen,
            String::from("Expect ')' after parameters."),
        )?;
        self.consume(
            &TokenType::LeftBrace,
            String::from("Expect '{' before body."),
        )?;

        match self.block_statement() {
            Ok(Stmt::BlockStmt(body)) => Ok(FunStmt::upcast(name, params, body.stmts)),
            Ok(_) => Err(JokerError::Parser(ParserError::report_error(
                &self.peek(),
                String::from("func translation err!"),
            ))),
            Err(err) => Err(err),
        }
    }
    // parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
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
    //        | break_stmt
    //        | continue_stmt
    //        | for_stmt
    //        | while_stmt
    //        | expr_stmt
    //        | block_stmt
    //        | if_stmt;
    fn statement(&mut self) -> Result<Stmt, JokerError> {
        if self.is_match(&[TokenType::Continue]) {
            return self.continue_statement();
        }
        if self.is_match(&[TokenType::Break]) {
            return self.break_statement();
        }
        if self.is_match(&[TokenType::For]) {
            return self.for_statement();
        }
        if self.is_match(&[TokenType::If]) {
            return self.if_statement();
        }
        if self.is_match(&[TokenType::Print]) {
            return self.print_statement();
        }
        if self.is_match(&[TokenType::While]) {
            return self.while_statement();
        }
        if self.is_match(&[TokenType::LeftBrace]) {
            return self.block_statement();
        }
        self.expr_statement()
    }
    // continueStmt -> "continue" ";" ;
    fn continue_statement(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.previous();
        self.consume(
            &TokenType::Semicolon,
            String::from("Expect ';' after 'continue' statement."),
        )?;
        Ok(ContinueStmt::upcast(name))
    }
    // breakStmt -> "break" ";" ;
    fn break_statement(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.previous();
        self.consume(
            &TokenType::Semicolon,
            String::from("Expect ';' after 'break' statement."),
        )?;
        Ok(BreakStmt::upcast(name))
    }
    // forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
    //                  expression? ";"
    //                  expression? ")" statement ;
    // {
    //   var i = 0;
    //   while (i < 10) {
    //     print i;
    //     i = i + 1;
    //   }
    // }
    fn for_statement(&mut self) -> Result<Stmt, JokerError> {
        self.consume(
            &TokenType::LeftParen,
            String::from("Expect '(' after 'for'."),
        )?;

        let initializer: Option<Box<Stmt>> = if self.is_match(&[TokenType::Semicolon]) {
            None
        } else if self.is_match(&[TokenType::Var]) {
            Some(Box::new(self.var_declaration()?))
        } else {
            Some(Box::new(self.expr_statement()?))
        };

        let condition: Expr = if !self.check(&TokenType::Semicolon) {
            self.expression()?
        } else {
            Literal::upcast(literal_bool(true))
        };
        self.consume(
            &TokenType::Semicolon,
            String::from("Expect ';' after for loop condition."),
        )?;

        let increment: Option<Expr> = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            &TokenType::RightParen,
            String::from("Expect ')' after for loop clauses."),
        )?;

        let body: Stmt = self.statement()?;

        Ok(ForStmt::upcast(
            initializer,
            condition,
            increment,
            Box::new(body),
        ))
    }
    // whileStmt      → "while" "(" expression ")" statement ;
    fn while_statement(&mut self) -> Result<Stmt, JokerError> {
        self.consume(
            &TokenType::LeftParen,
            String::from("Expect '(' after 'while'."),
        )?;
        let condition: Expr = self.expression()?;
        self.consume(
            &TokenType::RightParen,
            String::from("Expect ')' after while condition."),
        )?;
        let body: Stmt = self.statement()?;
        Ok(WhileStmt::upcast(condition, Box::new(body)))
    }
    // if stmt  -> "if" "(" expression ")" statement ( "else" statement )?
    fn if_statement(&mut self) -> Result<Stmt, JokerError> {
        self.consume(
            &TokenType::LeftParen,
            String::from("Expect '(' after 'if' statement."),
        )?;
        let condition: Expr = self.expression()?;
        self.consume(
            &TokenType::RightParen,
            String::from("Expect ')' after if condition."),
        )?;
        let then_branch: Stmt = self.statement()?;
        let else_branch: Option<Box<Stmt>> = if self.is_match(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(IfStmt::upcast(
            condition,
            Box::new(then_branch),
            else_branch,
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
            String::from("Expect '}' after block statement."),
        )?;
        Ok(BlockStmt::upcast(stmts))
    }
    // exprStmt     → expression ";"
    fn expr_statement(&mut self) -> Result<Stmt, JokerError> {
        let expr: Expr = self.expression()?;
        self.consume(
            &TokenType::Semicolon,
            String::from("Expect ';' after value."),
        )?;
        Ok(ExprStmt::upcast(expr))
    }
    // expression   → assignment
    fn expression(&mut self) -> Result<Expr, JokerError> {
        self.assignment()
    }
    // assignment  → IDENTIFIER "=" assignment
    //              | trinomial ;
    fn assignment(&mut self) -> Result<Expr, JokerError> {
        let expr: Expr = self.trinomial()?;
        if self.is_match(&[TokenType::Equal]) {
            let equal: Token = self.previous();
            let value: Expr = self.assignment()?;
            match expr {
                Expr::Variable(variable) => {
                    return Ok(Assign::upcast(variable.name, Box::new(value)))
                }
                _ => {
                    return Err(JokerError::Parser(ParserError::report_error(
                        &equal,
                        String::from("Invalid assignment target."),
                    )))
                }
            }
        }
        Ok(expr)
    }
    // Trinomial    → expression "?" expression ":" expression ";"
    //              | logic_or ;
    fn trinomial(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.logic_or()?;
        if self.is_match(&[TokenType::Question]) && !self.is_at_end() {
            let question: Token = self.previous();
            let l_expr: Expr = self.trinomial()?;
            if self.is_match(&[TokenType::Colon]) && !self.is_at_end() {
                let r_expr: Expr = self.trinomial()?;
                expr = Trinomial::upcast(Box::new(expr), Box::new(l_expr), Box::new(r_expr));
            } else {
                return Err(JokerError::Parser(ParserError::report_error(
                    &question,
                    String::from("Expect ':' after expression."),
                )));
            }
        }
        Ok(expr)
    }
    // logic_or   → logic_and ( "or" logic_and )*
    fn logic_or(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.logic_and()?;
        if self.is_match(&[TokenType::Or]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.logic_or()?;
            expr = Logical::upcast(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // logic_and  → equality ( "and" equality )*
    fn logic_and(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.equality()?;
        if self.is_match(&[TokenType::And]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.logic_and()?;
            expr = Logical::upcast(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // equality -> comparison ( ( "!=" | "==")  comparison )?
    fn equality(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.comparison()?;
        while self.is_match(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let m_opera: Token = self.previous();
            let r_expr: Expr = self.comparison()?;
            expr = Binary::upcast(Box::new(expr), m_opera, Box::new(r_expr));
        }
        Ok(expr)
    }
    // comparison -> term ( ( ">" | ">=" | "<" | "<=") term )?;
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
    //          | call  ;
    fn unary(&mut self) -> Result<Expr, JokerError> {
        if self.is_match(&[TokenType::Bang, TokenType::Minus]) {
            let l_opera: Token = self.previous();
            let r_expr: Expr = self.unary()?;
            return Ok(Unary::upcast(l_opera, Box::new(r_expr)));
        }
        self.call()
    }
    // call           → grouping ( "(" arguments? ")" )* ;
    fn call(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.grouping()?;
        loop {
            if self.is_match(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }
    fn finish_call(&mut self, callee: Expr) -> Result<Expr, JokerError> {
        let mut arguments: Vec<Expr> = Vec::new();
        if !self.check(&TokenType::RightParen) {
            arguments.push(self.expression()?);
            while self.is_match(&[TokenType::Comma]) {
                if arguments.len() >= 255 {
                    // waring:?
                    return Err(JokerError::Abort(AbortError::Argument(
                        ArgumentAbort::Limit(ArgLimitAbort::report_error(
                            &self.peek(),
                            String::from("Can't have more than 255 arguments."),
                        )),
                    )));
                }
                arguments.push(self.expression()?);
            }
        }
        let paren = self.consume(
            &TokenType::RightParen,
            String::from("Expect ')' after arguments."),
        )?;
        Ok(Call::upcast(Box::new(callee), paren, arguments))
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
        if self.is_at_end() {
            return Err(JokerError::Parser(ParserError::report_error(
                &self.peek(),
                String::from("parse at Eof!"),
            )));
        }
        match self.peek().ttype {
            TokenType::False => Ok(Literal::upcast(self.advance().literal)),
            TokenType::True => Ok(Literal::upcast(self.advance().literal)),
            TokenType::Null => Ok(Literal::upcast(self.advance().literal)),
            TokenType::I32 => Ok(Literal::upcast(self.advance().literal)),
            TokenType::F64 => Ok(Literal::upcast(self.advance().literal)),
            TokenType::Str => Ok(Literal::upcast(self.advance().literal)),
            TokenType::Identifier => Ok(Variable::upcast(self.advance())),
            _ => Err(JokerError::Parser(ParserError::report_error(
                &self.advance(),
                String::from("parse not impl!"),
            ))), // jump
        }
    }
    fn consume(&mut self, expected: &TokenType, msg: String) -> Result<Token, JokerError> {
        if self.check(expected) {
            Ok(self.advance())
        } else {
            Err(JokerError::Parser(ParserError::report_error(
                &self.peek(),
                msg,
            )))
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

#[derive(Debug)]
pub struct ParserError {
    line: usize,
    where_: String,
    msg: String,
}
impl ParserError {
    pub fn new(token: &Token, msg: String) -> ParserError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        ParserError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> ParserError {
        let parser_err = ParserError::new(token, msg);
        parser_err.report();
        parser_err
    }
}
impl ReportError for ParserError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
