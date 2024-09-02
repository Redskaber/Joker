//! This file is parse.rs
//!
//!
//!

use std::{error::Error, fmt::Display};

use super::{
    abort::ArgLimitAbort,
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Call, ClassStmt, ContinueStmt, Expr, ExprStmt,
        FnStmt, ForStmt, Getter, Grouping, IfStmt, Lambda, Literal, Logical, PrintStmt, ReturnStmt,
        Setter, Stmt, Super, This, Trinomial, Unary, VarStmt, Variable, WhileStmt,
    },
    error::{JokerError, ReportError},
    object::{literal_bool, FuncType},
    token::{Token, TokenType},
    types::{ParamPair, Type, TypeInferrer},
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
    pub(crate) fn is_match(&mut self, types: &[TokenType]) -> bool {
        for ttype in types {
            if self.check(ttype) {
                self.advance();
                return true;
            }
        }
        false
    }
    pub fn parse(&mut self) -> Result<Vec<Stmt>, JokerError> {
        let mut stmts: Vec<Stmt> = Vec::new();
        let mut has_error: Option<JokerError> = None;
        while !self.is_at_end() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    has_error = Some(err);
                    if self.is_at_end() {
                        break;
                    } // input: cc -> cc + Eof(now pos)
                    self.synchronize(); // upcast: declaration -> this
                }
            }
        }
        match has_error {
            Some(err) => Err(err),
            None => Ok(stmts),
        }
    }

    // declaration -> stmt              （语句）
    //               | var_declaration  (声明)
    //               | fn_declaration
    //               | class_declaration
    fn declaration(&mut self) -> Result<Stmt, JokerError> {
        if self.is_match(&[TokenType::Class]) {
            return self.class_declaration();
        }
        if self.is_match(&[TokenType::Fn]) {
            return self.fn_declaration();
        }
        if self.is_match(&[TokenType::Var]) {
            return self.var_declaration();
        }
        self.statement()
    }
    // class_declaration      → "class" classStmt ;
    // classStmt      → "class" IDENTIFIER (":" IDENTIFIER )? "{"
    //                      var_decl* | fn_decl* | method_decl*
    //                  "}" ;
    fn class_declaration(&mut self) -> Result<Stmt, JokerError> {
        let name: Token =
            self.consume(&[TokenType::Identifier], String::from("expect class name."))?;

        let super_class: Option<Expr> = if self.is_match(&[TokenType::Colon]) {
            let super_class = self.consume(
                &[TokenType::Identifier],
                String::from("expect super class name."),
            )?;
            Some(Expr::Variable(Variable::new(super_class)))
        } else {
            None
        };
        self.consume(
            &[TokenType::LeftBrace],
            String::from("expect '{' before class body."),
        )?;

        let (fields, methods, functions) = if self.check(&TokenType::RightBrace) {
            (None, None, None)
        } else {
            let mut fields = Vec::new();
            let mut methods = Vec::new();
            let mut functions = Vec::new();
            loop {
                if self.is_match(&[TokenType::Var]) {
                    fields.push(self.var_declaration()?);
                } else if self.is_match(&[TokenType::Fn]) {
                    match self.class_fn_declaration()? {
                        FuncType::Method(method) => methods.push(method),
                        FuncType::Function(function) => functions.push(function),
                    }
                } else {
                    return Err(JokerError::Parser(ParserError::report_error(
                        &self.peek(),
                        String::from("class inside only have var and fun."),
                    )));
                }
                if self.check(&TokenType::RightBrace) {
                    break;
                }
            }
            (
                if fields.is_empty() {
                    None
                } else {
                    Some(fields)
                },
                if methods.is_empty() {
                    None
                } else {
                    Some(methods)
                },
                if functions.is_empty() {
                    None
                } else {
                    Some(functions)
                },
            )
        };
        self.consume(
            &[TokenType::RightBrace],
            String::from("expect '}' after class body."),
        )?;
        Ok(ClassStmt::upcast(
            name,
            super_class,
            fields,
            methods,
            functions,
        ))
    }
    fn class_fn_declaration(&mut self) -> Result<FuncType, JokerError> {
        if let Stmt::FnStmt(fn_stmt) = self.fn_declaration()? {
            match &fn_stmt.params {
                Some(params) => match params[0].as_ref() {
                    ParamPair::This { param, type_: _ } if param.lexeme.eq("this") => Ok(FuncType::Method(Stmt::FnStmt(fn_stmt))),
                    ParamPair::Normal { param: _, type_: _ } => Ok(FuncType::Function(Stmt::FnStmt(fn_stmt))),
                    ParamPair::This { param , type_: _ } => Err(JokerError::Parser(ParserError::report_error(
                        param,
                        format!("class method function first param is this keyword, but this is '{}'", param.lexeme),
                    ))),
                    ParamPair::Label { type_: _ } => Err(JokerError::Parser(ParserError::report_error(
                        &fn_stmt.name,
                        String::from("function not used var type label param pair.")
                    )))
                },
                None => Ok(FuncType::Function(Stmt::FnStmt(fn_stmt))),
            }
        } else {
            unreachable!("fn_declaration return Stmt::FnStmt, unreachable this.")
        }
    }
    // fn_declaration        → "fn" FnStmt ;
    // FnStmt        → "fn" IDENTIFIER  "("
    //                      (IDENTIFIER ":" IDENTIFIER (, IDENTIFIER ":" IDENTIFIER )*? )?
    //                  ")" ("->" IDENTIFIER)? statement ;
    fn fn_declaration(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.consume(
            &[TokenType::Identifier],
            String::from("Expect function name."),
        )?;
        self.consume(
            &[TokenType::LeftParen],
            String::from("Expect '(' after function name."),
        )?;

        let params: Option<Vec<ParamPair>> = if self.check(&TokenType::RightParen) {
            None
        } else {
            let mut params: Vec<ParamPair> = if self.is_match(&[TokenType::This]) {
                vec![ParamPair::this_with_parse(self)?]
            } else {
                vec![ParamPair::normal_with_parse(self)?]
            };
            while self.is_match(&[TokenType::Comma]) {
                params.push(ParamPair::normal_with_parse(self)?);
            }
            if params.len() >= 255 {
                // TODO: warning
                ArgLimitAbort::report_error(
                    &self.peek(),
                    String::from("Can't have more than 255 parameters."),
                );
            }
            Some(params)
        };
        self.consume(
            &[TokenType::RightParen],
            String::from("Expect ')' after parameters."),
        )?;

        let return_type = if self.is_match(&[TokenType::Arrow]) {
            Some(Box::new(TypeInferrer::parse_type(self)?))
        } else {
            None
        };

        self.consume(
            &[TokenType::LeftBrace],
            String::from("Expect '{' before body."),
        )?;

        match self.block_statement() {
            Ok(Stmt::BlockStmt(body)) => Ok(FnStmt::upcast(name, params, return_type, body.stmts)),
            Ok(_) => Err(JokerError::Parser(ParserError::report_error(
                &self.peek(),
                String::from("fn translation err!"),
            ))),
            Err(err) => Err(err),
        }
    }
    // varStmt → "var" IDENTIFIER (":" IDENTIFIER)?  ("=" expression )? ";" ;
    fn var_declaration(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.consume(
            &[TokenType::Identifier],
            String::from("Expect variable name."),
        )?;

        let type_: Option<Type> = if self.is_match(&[TokenType::Colon]) {
            Some(TypeInferrer::parse_type(self)?)
        } else {
            None
        };

        let value: Option<Expr> = if self.is_match(&[TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            &[TokenType::Semicolon],
            String::from("Expect ';' after variable declaration."),
        )?;
        Ok(VarStmt::upcast(name, type_, value))
    }
    // stmt -> print_stmt
    //        | return_stmt
    //        | break_stmt
    //        | continue_stmt
    //        | for_stmt
    //        | while_stmt
    //        | expr_stmt
    //        | block_stmt
    //        | if_stmt;
    fn statement(&mut self) -> Result<Stmt, JokerError> {
        if self.is_match(&[TokenType::Return]) {
            return self.return_statement();
        }
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
    // returnStmt -> "return" expression? ";" ;
    fn return_statement(&mut self) -> Result<Stmt, JokerError> {
        let keyword: Token = self.previous();
        let value: Option<Expr> = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            &[TokenType::Semicolon],
            String::from("Expect ';' after return statement value."),
        )?;
        Ok(ReturnStmt::upcast(keyword, value))
    }
    // continueStmt -> "continue" ";" ;
    fn continue_statement(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.previous();
        self.consume(
            &[TokenType::Semicolon],
            String::from("Expect ';' after 'continue' statement."),
        )?;
        Ok(ContinueStmt::upcast(name))
    }
    // breakStmt -> "break" ";" ;
    fn break_statement(&mut self) -> Result<Stmt, JokerError> {
        let name: Token = self.previous();
        self.consume(
            &[TokenType::Semicolon],
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
            &[TokenType::LeftParen],
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
            &[TokenType::Semicolon],
            String::from("Expect ';' after for loop condition."),
        )?;

        let increment: Option<Expr> = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(
            &[TokenType::RightParen],
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
            &[TokenType::LeftParen],
            String::from("Expect '(' after 'while'."),
        )?;
        let condition: Expr = self.expression()?;
        self.consume(
            &[TokenType::RightParen],
            String::from("Expect ')' after while condition."),
        )?;
        let body: Stmt = self.statement()?;
        Ok(WhileStmt::upcast(condition, Box::new(body)))
    }
    // if stmt  -> "if" "(" expression ")" statement ( "else" statement )?
    fn if_statement(&mut self) -> Result<Stmt, JokerError> {
        self.consume(
            &[TokenType::LeftParen],
            String::from("Expect '(' after 'if' statement."),
        )?;
        let condition: Expr = self.expression()?;
        self.consume(
            &[TokenType::RightParen],
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
            &[TokenType::Semicolon],
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
            &[TokenType::RightBrace],
            String::from("Expect '}' after block statement."),
        )?;
        Ok(BlockStmt::upcast(stmts))
    }
    // exprStmt     → expression ";"
    fn expr_statement(&mut self) -> Result<Stmt, JokerError> {
        let expr: Expr = self.expression()?;
        self.consume(
            &[TokenType::Semicolon],
            String::from("Expect ';' after value."),
        )?;
        Ok(ExprStmt::upcast(expr))
    }
    // expression   → assignment
    fn expression(&mut self) -> Result<Expr, JokerError> {
        self.assignment()
    }
    // assignment  → (call ".")? IDENTIFIER "=" (assignment "=")*
    //              | lambda ;
    fn assignment(&mut self) -> Result<Expr, JokerError> {
        let expr: Expr = self.lambda()?;
        if self.is_match(&[TokenType::Equal]) {
            let equal: Token = self.previous();
            let value: Expr = self.assignment()?;
            match expr {
                Expr::Variable(variable) => {
                    return Ok(Assign::upcast(variable.name, Box::new(value)));
                }
                Expr::Getter(getter) => {
                    return Ok(Setter::upcast(getter.expr, getter.name, Box::new(value)));
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
    // lambda_expr -> "|" parameters? "|" ("->" IDENTIFIER)? statement "(" parameters? ")"
    //                |  trinomial ;
    // parameters -> IDENTIFIER (, IDENTIFIER)*
    fn lambda(&mut self) -> Result<Expr, JokerError> {
        if self.is_match(&[TokenType::Pipeline]) {
            let pipe: Token = self.previous();

            let params: Option<Vec<ParamPair>> = if self.check(&TokenType::Pipeline) {
                None
            } else {
                let mut params: Vec<ParamPair> = vec![ParamPair::normal_with_parse(self)?];
                while self.is_match(&[TokenType::Comma]) {
                    params.push(ParamPair::normal_with_parse(self)?);
                }
                if params.len() >= 255 {
                    // TODO: warning
                    ArgLimitAbort::report_error(
                        &self.peek(),
                        String::from("Can't have more than 255 parameters."),
                    );
                }
                Some(params)
            };
            self.consume(
                &[TokenType::Pipeline],
                String::from("Expect '|' after parameters."),
            )?;

            let return_type: Option<Box<Type>> = if self.is_match(&[TokenType::Arrow]) {
                Some(Box::new(TypeInferrer::parse_type(self)?))
            } else {
                None
            };

            let body: Stmt = if self.is_match(&[TokenType::LeftBrace]) {
                self.block_statement()?
            } else {
                ExprStmt::upcast(self.expression()?)
            };

            let lambda: Expr = Lambda::upcast(pipe, params, return_type, Box::new(body));
            if self.is_match(&[TokenType::LeftParen]) {
                return self.finish_call(lambda);
            } else {
                return Ok(lambda);
            }
        }
        self.trinomial()
    }
    // Trinomial    → expression "?" expression ":" expression ";"
    //              | logic_or ;
    fn trinomial(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.logic_or()?;
        if self.is_match(&[TokenType::Question]) && !self.is_at_end() {
            let question: Token = self.previous();
            let l_expr: Expr = self.expression()?;
            if self.is_match(&[TokenType::Colon]) && !self.is_at_end() {
                let r_expr: Expr = self.expression()?;
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
    // call           → grouping ( "(" arguments? ")" | "." IDENTIFIER )* ;
    // arguments      → expression ( "," expression )* ;
    fn call(&mut self) -> Result<Expr, JokerError> {
        let mut expr: Expr = self.grouping()?;
        loop {
            if self.is_match(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.is_match(&[TokenType::Dot]) {
                let name: Token = self.consume(
                    &[TokenType::Identifier],
                    String::from("expect attribute name after '.'."),
                )?;
                expr = Getter::upcast(Box::new(expr), name);
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
                    ArgLimitAbort::report_error(
                        &self.peek(),
                        String::from("Can't have more than 255 arguments."),
                    );
                }
                arguments.push(self.expression()?);
            }
        }
        let paren = self.consume(
            &[TokenType::RightParen],
            String::from("Expect ')' after arguments."),
        )?;
        Ok(Call::upcast(Box::new(callee), paren, arguments))
    }
    // grouping -> "(" expression ")" ;
    //              | “super” "." IDENTIFIER
    fn grouping(&mut self) -> Result<Expr, JokerError> {
        if self.is_match(&[TokenType::LeftParen]) {
            let expr: Expr = self.expression()?;
            self.consume(
                &[TokenType::RightParen],
                String::from("Expect ')' after expression."),
            )?;
            return Ok(Grouping::upcast(Box::new(expr)));
        }
        self.super_()
    }
    // super        “super” "." IDENTIFIER
    //              | primary ;
    fn super_(&mut self) -> Result<Expr, JokerError> {
        if self.is_match(&[TokenType::Super]) {
            let keyword: Token = self.previous();
            self.consume(&[TokenType::Dot], String::from("expect '.' after 'super'."))?;
            let method: Token = self.consume(
                &[TokenType::Identifier],
                String::from("expect super class method name."),
            )?;
            return Ok(Expr::Super(Super::new(keyword, method)));
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
            TokenType::This => Ok(This::upcast(self.advance())),
            TokenType::Identifier => Ok(Variable::upcast(self.advance())),
            _ => Err(JokerError::Parser(ParserError::report_error(
                &self.advance(),
                String::from("parse not impl!"),
            ))), // jump
        }
    }
    pub(crate) fn consume(
        &mut self,
        expected: &[TokenType],
        msg: String,
    ) -> Result<Token, JokerError> {
        for exp in expected {
            if self.check(exp) {
                return Ok(self.advance());
            }
        }
        Err(JokerError::Parser(ParserError::report_error(
            &self.peek(),
            msg,
        )))
    }
    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().ttype == TokenType::Semicolon {
                return;
            }
            match self.peek().ttype {
                TokenType::Class
                | TokenType::Fn
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

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ParserError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl Error for ParserError {}

impl ReportError for ParserError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}
