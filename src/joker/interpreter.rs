//! This file is interpreters.rs
//!
//!

use std::{cell::RefCell, collections::HashMap, error::Error, fmt::Display, hash::Hash, rc::Rc};

use crate::joker::{object::Lambda, types::DeepClone};

use super::{
    abort::{ControlFlowAbort, Error::ControlFlow},
    ast::{
        Assign, Binary, BlockStmt, BreakStmt, Call, ClassStmt, ContinueStmt, Expr, ExprAcceptor,
        ExprStmt, ExprVisitor, FnStmt, ForStmt, Getter, Grouping, IfStmt, Lambda as LambdaExpr,
        Literal, Logical, PrintStmt, ReturnStmt, Setter, Stmt, StmtAcceptor, StmtVisitor, Super,
        This, Trinomial, Unary, VarStmt, Variable, WhileStmt,
    },
    callable::{
        ArgumentError, Callable,
        Error::{Argument, NonCallable},
        NonError,
    },
    env::Env,
    error::{JokerError, ReportError, SystemError, SystemTimeError},
    object::{
        Binder, Caller, Class, Function, Literal as ObL, MethodFunction, NativeFunction,
        Object as OEnum, UserFunction,
    },
    parse::ParserError,
    token::{Token, TokenType},
    types::Object,
};

#[derive(Debug)]
pub struct Interpreter {
    pub global: Rc<RefCell<Env>>,
    local_resolve: RefCell<HashMap<Expr, usize>>,
    pub run_env: RefCell<Rc<RefCell<Env>>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let global: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new()));
        global.borrow_mut().define(
            String::from("clock"),
            Some(Object::new(OEnum::Caller(Caller::Func(Function::Native(
                NativeFunction::new(|| {
                    use std::time::SystemTime;

                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct NativeClock;

                    impl Callable for NativeClock {
                        fn call(
                            &self,
                            _interpreter: &Interpreter,
                            _arguments: &[Object],
                        ) -> Result<Option<Object>, JokerError> {
                            match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                                Ok(duration) => Ok(Some(Object::new(OEnum::Literal(ObL::F64(
                                    duration.as_millis() as f64,
                                ))))),
                                Err(err) => Err(JokerError::System(SystemError::Time(
                                    SystemTimeError::report_error(format!(
                                        "Native clock return invalid duration: {:?}.",
                                        err
                                    )),
                                ))),
                            }
                        }
                        fn arity(&self) -> usize {
                            0
                        }
                    }
                    impl Display for NativeClock {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            write!(f, "clock")
                        }
                    }

                    NativeFunction {
                        fun: Rc::new(NativeClock {}),
                    }
                }),
            ))))),
        );

        Interpreter {
            global: Rc::clone(&global),
            local_resolve: RefCell::new(HashMap::new()),
            run_env: RefCell::new(Rc::clone(&global)),
        }
    }
    fn is_true(&self, object: &Object) -> bool {
        matches!(*object.get(), OEnum::Literal(ObL::Bool(true)))
    }
    fn execute(&self, stmt: &Stmt) -> Result<(), JokerError> {
        stmt.accept(self)
    }
    pub fn execute_block(&self, stmts: &[Stmt], block_env: Env) -> Result<(), JokerError> {
        let previous: Rc<RefCell<Env>> = self.run_env.replace(Rc::new(RefCell::new(block_env)));
        let result: Result<(), JokerError> = stmts.iter().try_for_each(|stmt| self.execute(stmt));
        self.run_env.replace(previous);
        result
    }
    pub fn evaluate(&self, expr: &Expr) -> Result<Option<Object>, JokerError> {
        expr.accept(self)
    }
    pub fn evaluate_local(&self, expr: &Expr, env: Env) -> Result<Option<Object>, JokerError> {
        let previous = self.run_env.replace(Rc::new(RefCell::new(env)));
        let result = expr.accept(self);
        self.run_env.replace(previous);
        result
    }
    pub fn resolve(&self, expr: Expr, depth: usize) {
        self.local_resolve.borrow_mut().insert(expr, depth);
    }
    fn look_up_variable(&self, name: &Token, expr: &Expr) -> Result<Option<Object>, JokerError> {
        match self.local_resolve.borrow().get(expr) {
            Some(depth) => self
                .run_env
                .borrow()
                .borrow_mut()
                .get_with_depth(*depth, name),
            None => self.global.borrow_mut().get(name),
        }
    }
    pub fn interpreter(&self, stmts: &[Stmt]) -> Result<(), JokerError> {
        // let printer: AstPrinter = AstPrinter::new();
        for stmt in stmts {
            // printer.println(stmt);
            self.execute(stmt)? // not jump
        }
        Ok(())
    }
    pub fn value_or_raise(
        &self,
        token: &Token,
        expr: &Expr,
        msg: String,
    ) -> Result<Object, JokerError> {
        match self.evaluate(expr)? {
            Some(object) => Ok(object),
            None => Err(JokerError::Interpreter(InterpreterError::report_error(
                token, msg,
            ))),
        }
    }
    pub fn println_local(&self) {
        println!("{:?}", self.run_env);
    }
}

impl StmtVisitor<()> for Interpreter {
    fn visit_expr(&self, stmt: &ExprStmt) -> Result<(), JokerError> {
        self.evaluate(&stmt.expr)?;
        Ok(())
    }
    fn visit_print(&self, stmt: &PrintStmt) -> Result<(), JokerError> {
        match self.evaluate(&stmt.expr) {
            Ok(value) => {
                if let Some(value) = value {
                    println!("{value}")
                }
                Ok(())
            }
            Err(err) => Err(err),
        }
    }
    fn visit_var(&self, stmt: &VarStmt) -> Result<(), JokerError> {
        // println!(
        //     "[{:>10}][{:>20}]:\t{:<5}: {}",
        //     "inter", "visit_var", "stmt", stmt
        // );
        let value: Option<Object> = match &stmt.value {
            Some(expr) => self.evaluate(expr)?.map(|value: Object| value.deep_clone()),
            None => None,
        };
        self.run_env
            .borrow()
            .borrow_mut()
            .define(stmt.name.lexeme.clone(), value);
        Ok(())
    }
    fn visit_block(&self, stmt: &BlockStmt) -> Result<(), JokerError> {
        let block_env: Env = Env::new_with_enclosing(Rc::clone(&self.run_env.borrow()));
        self.execute_block(&stmt.stmts, block_env)
    }
    fn visit_if(&self, stmt: &IfStmt) -> Result<(), JokerError> {
        if self.is_true(&self.evaluate(&stmt.condition)?.unwrap()) {
            self.execute(&stmt.then_branch)?;
        } else if let Some(box_stmt) = &stmt.else_branch {
            self.execute(box_stmt)?;
        }
        Ok(())
    }
    fn visit_while(&self, stmt: &WhileStmt) -> Result<(), JokerError> {
        while self.is_true(&self.evaluate(&stmt.condition)?.unwrap()) {
            if let Err(err) = self.execute(&stmt.body) {
                match err {
                    JokerError::Abort(ControlFlow(control_flow)) => match control_flow {
                        ControlFlowAbort::Break => break,
                        ControlFlowAbort::Continue => continue,
                        ControlFlowAbort::Return(return_value) => {
                            return Err(JokerError::Abort(ControlFlow(ControlFlowAbort::Return(
                                return_value,
                            ))));
                        }
                    },
                    _ => return Err(err),
                }
            }
        }

        Ok(())
    }
    fn visit_for(&self, stmt: &ForStmt) -> Result<(), JokerError> {
        if let Some(initializer) = &stmt.initializer {
            self.execute(initializer)?
        }
        while self.is_true(&self.evaluate(&stmt.condition)?.unwrap()) {
            if let Err(err) = self.execute(&stmt.body) {
                match err {
                    JokerError::Abort(ControlFlow(control_flow)) => match control_flow {
                        ControlFlowAbort::Break => break,
                        ControlFlowAbort::Continue => {
                            if let Some(increment) = &stmt.increment {
                                self.evaluate(increment)?;
                            }
                            continue;
                        }
                        ControlFlowAbort::Return(return_value) => {
                            return Err(JokerError::Abort(ControlFlow(ControlFlowAbort::Return(
                                return_value,
                            ))));
                        }
                    },
                    _ => return Err(err),
                }
            }
            if let Some(increment) = &stmt.increment {
                self.evaluate(increment)?;
            }
        }

        Ok(())
    }
    fn visit_break(&self, _stmt: &BreakStmt) -> Result<(), JokerError> {
        Err(JokerError::Abort(ControlFlow(ControlFlowAbort::Break)))
    }
    fn visit_continue(&self, _stmt: &ContinueStmt) -> Result<(), JokerError> {
        Err(JokerError::Abort(ControlFlow(ControlFlowAbort::Continue)))
    }
    fn visit_fn(&self, stmt: &FnStmt) -> Result<(), JokerError> {
        let func: Object = Object::new(OEnum::Caller(Caller::Func(Function::User(
            UserFunction::new(stmt, Rc::clone(&self.run_env.borrow())),
        ))));
        self.run_env
            .borrow()
            .borrow_mut()
            .define(stmt.name.lexeme.clone(), Some(func));
        Ok(())
    }
    fn visit_return(&self, stmt: &ReturnStmt) -> Result<(), JokerError> {
        let value: Option<Object> = match &stmt.value {
            Some(expr) => self.evaluate(expr)?,
            None => None,
        };
        Err(JokerError::Abort(ControlFlow(ControlFlowAbort::Return(
            value,
        ))))
    }
    fn visit_class(&self, stmt: &ClassStmt) -> Result<(), JokerError> {
        self.run_env
            .borrow()
            .borrow_mut()
            .define(stmt.name.lexeme.clone(), None);

        let super_class: Option<Box<Class>> = match &stmt.super_class {
            Some(super_expr) => {
                if let Expr::Variable(super_var) = super_expr {
                    if let Some(obj) = &self.evaluate(super_expr)? {
                        match &*obj.get() {
                            OEnum::Caller(Caller::Class(class)) => Some(class.clone()),
                            _ => {
                                return Err(JokerError::Interpreter(
                                    InterpreterError::report_error(
                                        &super_var.name,
                                        String::from("super class must be a class."),
                                    ),
                                ))
                            }
                        }
                    } else {
                        return Err(JokerError::Interpreter(InterpreterError::report_error(
                            &super_var.name,
                            String::from("variable is declare, but not define."),
                        )));
                    }
                } else {
                    unreachable!("class super must variable, but is not variable.")
                }
            }
            None => None,
        };

        // run_env -> super
        let previous = if let Some(super_class) = &super_class {
            let super_env: Rc<RefCell<Env>> = Rc::new(RefCell::new(Env::new_with_enclosing(
                Rc::clone(&self.run_env.borrow()),
            )));
            let previous: Rc<RefCell<Env>> = self.run_env.replace(super_env);
            let super_obj: Object = Object::new(OEnum::Caller(Caller::Class(super_class.clone())));
            self.run_env
                .borrow()
                .borrow_mut()
                .define(String::from("super"), Some(super_obj));
            Some(previous)
        } else {
            None
        };

        let fields: Option<HashMap<String, Option<Object>>> = match &stmt.fields {
            Some(stmts) => {
                let mut vars: HashMap<String, Option<Object>> = HashMap::new();
                for stmt in stmts {
                    if let Stmt::VarStmt(var_stmt) = stmt {
                        let value = match &var_stmt.value {
                            Some(expr) => self.evaluate(expr)?,
                            None => None,
                        };
                        vars.insert(var_stmt.name.lexeme.clone(), value);
                    }
                }
                Some(vars)
            }
            None => None,
        };

        let methods: Option<HashMap<String, MethodFunction>> = match &stmt.methods {
            Some(stmts) => {
                let mut methods: HashMap<String, MethodFunction> = HashMap::new();
                for stmt in stmts {
                    if let Stmt::FnStmt(fn_stmt) = stmt {
                        methods.insert(
                            fn_stmt.name.lexeme.clone(),
                            MethodFunction::new(fn_stmt, Rc::clone(&self.run_env.borrow())),
                        );
                    }
                }
                Some(methods)
            }
            None => None,
        };

        let functions: Option<HashMap<String, UserFunction>> = match &stmt.functions {
            Some(stmts) => {
                let mut functions: HashMap<String, UserFunction> = HashMap::new();
                for stmt in stmts {
                    if let Stmt::FnStmt(fn_stmt) = stmt {
                        functions.insert(
                            fn_stmt.name.lexeme.clone(),
                            UserFunction::new(fn_stmt, Rc::clone(&self.run_env.borrow())),
                        );
                    }
                }
                Some(functions)
            }
            None => None,
        };

        let class: Object = Object::new(OEnum::Caller(Caller::Class(Box::new(Class::new(
            stmt.name.clone(),
            super_class,
            fields,
            methods,
            functions,
        )))));

        // super -> run_env
        if let Some(previous) = previous {
            self.run_env.replace(previous);
        }

        self.run_env
            .borrow()
            .borrow_mut()
            .assign(&stmt.name, class)?;
        Ok(())
    }
}

impl ExprVisitor<Option<Object>> for Interpreter {
    fn visit_literal(&self, expr: &Literal) -> Result<Option<Object>, JokerError> {
        Ok(Some(Object::new(expr.value.clone())))
    }
    fn visit_unary(&self, expr: &Unary) -> Result<Option<Object>, JokerError> {
        let r_expr: Object = self.value_or_raise(
            &expr.l_opera,
            &expr.r_expr,
            String::from("unary object invalid value."),
        )?;
        match expr.l_opera.ttype {
            TokenType::Minus => match &*r_expr.get() {
                OEnum::Literal(literal) => match literal {
                    ObL::I32(i32_) => Ok(Some(Object::new(OEnum::Literal(ObL::I32(-i32_))))),
                    ObL::F64(f64_) => Ok(Some(Object::new(OEnum::Literal(ObL::F64(-f64_))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.l_opera,
                        format!(
                            "[[Minus]] The literal cannot take negative values. {} !=> -{}",
                            literal, literal
                        ),
                    ))),
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.l_opera,
                    String::from("not impl Minus!"),
                ))),
            },
            TokenType::Bang => match &*r_expr.get() {
                OEnum::Literal(ref literal) => match literal {
                    ObL::Bool(bool_) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(!bool_))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.l_opera,
                        format!(
                            "[[Bang]] The literal cannot take reversed values. {} !=> !{}",
                            literal, literal
                        ),
                    ))),
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.l_opera,
                    String::from("not impl Bang!"),
                ))),
            },
            _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                &expr.l_opera,
                String::from("Unreachable according to Literal Num!"),
            ))),
        }
    }
    fn visit_binary(&self, expr: &Binary) -> Result<Option<Object>, JokerError> {
        let l_expr: Object = self.value_or_raise(
            &expr.m_opera,
            &expr.l_expr,
            String::from("binary invalid left value."),
        )?;
        let r_expr: Object = self.value_or_raise(
            &expr.m_opera,
            &expr.r_expr,
            String::from("binary invalid right value."),
        )?;
        match expr.m_opera.ttype {
            TokenType::BangEqual => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_i32 != r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_f64 != r_f64))))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_bool != r_bool))))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_str != r_str))))),
                    (ObL::Null, ObL::Null) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(false))))),
                    (ObL::Null, _) | (_, ObL::Null)=> Ok(Some(Object::new(OEnum::Literal(ObL::Bool(true))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[BangEqual]] The literal cannot take bang equal values. !({l_literal} != {r_literal})")
                    )))
                }
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl BangEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::EqualEqual => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_i32 == r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_f64 == r_f64))))),
                    (ObL::Bool(l_bool), ObL::Bool(r_bool)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_bool == r_bool))))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_str == r_str))))),
                    (ObL::Null, ObL::Null) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(true))))),
                    (ObL::Null, _) | (_, ObL::Null)=> Ok(Some(Object::new(OEnum::Literal(ObL::Bool(false))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[EqualEqual]] The literal cannot take equal values. !({l_literal} == {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl EqualEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::Greater => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_i32 > r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_f64 > r_f64))))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_str > r_str))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Greater]] The literal cannot take greater values. !({l_literal} > {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Greater! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::GreaterEqual => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_i32 >= r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_f64 >= r_f64))))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_str >= r_str))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[GreaterEqual]] The literal cannot take greater equal values. !({l_literal} >= {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl GreaterEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::Less => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_i32 < r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_f64 < r_f64))))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_str < r_str))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Less]] The literal cannot take less values. !({l_literal} < {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Less! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::LessEqual => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_i32 <= r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_f64 <= r_f64))))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => Ok(Some(Object::new(OEnum::Literal(ObL::Bool(l_str <= r_str))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[LessEqual]] The literal cannot take less equal values. !({l_literal} <= {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl LessEqual! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            },
            TokenType::Plus => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::I32(l_i32 + r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::F64(l_f64 + r_f64))))),
                    (ObL::Str(l_str), ObL::Str(r_str)) => {
                        Ok(Some(Object::new(OEnum::Literal(ObL::Str(format!("{l_str}{r_str}"))))))
                    },
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Plus]] The literal cannot take plus values. !({l_literal} + {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Plus! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            TokenType::Minus => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::I32(l_i32 - r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::F64(l_f64 - r_f64))))),
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Minus]] The literal cannot take minus values. !({l_literal} - {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Minus! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            TokenType::Slash => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => {
                        if r_i32 != &0 {
                            Ok(Some(Object::new(OEnum::Literal(ObL::I32(l_i32 / r_i32)))))
                        } else {
                            Err(JokerError::Interpreter(InterpreterError::report_error(
                                &expr.m_opera,
                                format!("[[Slash::ZeroSlashError]]. !({l_literal} / {r_literal})")
                            )))
                        }
                    },
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => {
                        if r_f64 != &0f64 {
                            Ok(Some(Object::new(OEnum::Literal(ObL::F64(l_f64 / r_f64)))))
                        } else {
                            Err(JokerError::Interpreter(InterpreterError::report_error(
                                &expr.m_opera,
                                format!("[[Slash::ZeroSlashError]] . !({l_literal} / {r_literal})")
                            )))
                        }
                    },
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Slash]] The literal cannot take slash values. !({l_literal} / {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Slash! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            TokenType::Star => match (&*l_expr.get(), &*r_expr.get()) {
                (OEnum::Literal(l_literal), OEnum::Literal(r_literal)) => match (l_literal, r_literal) {
                    (ObL::I32(l_i32), ObL::I32(r_i32)) => Ok(Some(Object::new(OEnum::Literal(ObL::I32(l_i32 / r_i32))))),
                    (ObL::F64(l_f64), ObL::F64(r_f64)) => Ok(Some(Object::new(OEnum::Literal(ObL::F64(l_f64 * r_f64))))),
                    (ObL::Str(str_), ObL::I32(i32_))
                    | (ObL::I32(i32_), ObL::Str(str_)) => {
                        let mut count = *i32_;
                        let mut r_str: String = String::with_capacity(size_of_val(str_)* count as usize);
                        while count > 0 {
                            r_str.push_str(str_);
                            count -= 1;
                        }
                        Ok(Some(Object::new(OEnum::Literal(ObL::Str(r_str)))))
                    },
                    _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("[[Star]] The literal cannot take star values. !({l_literal} * {r_literal})")
                    )))
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.m_opera,
                        format!("not impl Star! (l_expr: {}, r_expr: {})", l_expr, r_expr),
                    ))),
            }
            _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                &expr.m_opera,
                String::from("Unreachable according other type!")
            )))
        }
    }
    fn visit_grouping(&self, expr: &Grouping) -> Result<Option<Object>, JokerError> {
        self.evaluate(&expr.expr)
    }
    fn visit_variable(&self, expr: &Variable) -> Result<Option<Object>, JokerError> {
        self.look_up_variable(&expr.name, &Expr::Variable(expr.clone()))
    }
    fn visit_assign(&self, expr: &Assign) -> Result<Option<Object>, JokerError> {
        let value: Object = self.value_or_raise(
            &expr.name,
            &expr.value,
            String::from("assign invalid value."),
        )?;
        match self.local_resolve.borrow().get(&Expr::Assign(expr.clone())) {
            Some(depth) => self.run_env.borrow().borrow_mut().assign_with_depth(
                *depth,
                &expr.name,
                value.clone(),
            )?,
            None => self.global.borrow_mut().assign(&expr.name, value.clone())?,
        }
        Ok(Some(value))
    }
    fn visit_logical(&self, expr: &Logical) -> Result<Option<Object>, JokerError> {
        let l_object: Object = self.value_or_raise(
            &expr.m_opera,
            &expr.l_expr,
            String::from("logical invalid left value"),
        )?;
        match expr.m_opera.ttype {
            TokenType::Or => {
                if self.is_true(&l_object) {
                    Ok(Some(Object::new(OEnum::Literal(ObL::Bool(true)))))
                } else {
                    let r_object: Object = self.value_or_raise(
                        &expr.m_opera,
                        &expr.r_expr,
                        String::from("logical invalid right value"),
                    )?;
                    if self.is_true(&r_object) {
                        Ok(Some(Object::new(OEnum::Literal(ObL::Bool(true)))))
                    } else {
                        Ok(Some(Object::new(OEnum::Literal(ObL::Bool(false)))))
                    }
                }
            }
            TokenType::And => {
                if !self.is_true(&l_object) {
                    Ok(Some(Object::new(OEnum::Literal(ObL::Bool(false)))))
                } else {
                    let r_object: Object = self.value_or_raise(
                        &expr.m_opera,
                        &expr.r_expr,
                        String::from("logical invalid right value"),
                    )?;
                    if self.is_true(&r_object) {
                        Ok(Some(Object::new(OEnum::Literal(ObL::Bool(true)))))
                    } else {
                        Ok(Some(Object::new(OEnum::Literal(ObL::Bool(false)))))
                    }
                }
            }
            _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                &expr.m_opera,
                format!("Unsupported logic operator: {:?}", expr.m_opera.ttype),
            ))),
        }
    }
    fn visit_trinomial(&self, expr: &Trinomial) -> Result<Option<Object>, JokerError> {
        let condition = expr.condition.accept(self)?.unwrap();
        if self.is_true(&condition) {
            let l_object = expr.l_expr.accept(self)?;
            Ok(l_object)
        } else {
            let r_object = expr.r_expr.accept(self)?;
            Ok(r_object)
        }
    }
    fn visit_call(&self, expr: &Call) -> Result<Option<Object>, JokerError> {
        let callee: Object = self.value_or_raise(
            &expr.paren,
            &expr.callee,
            String::from("call object invalid value."),
        )?;

        let mut arguments: Vec<Object> = Vec::new();
        for arg in &expr.arguments {
            arguments.push(self.evaluate(arg)?.unwrap());
        }

        let result: Result<Option<Object>, JokerError> =
            if let OEnum::Caller(caller) = &*callee.get() {
                if arguments.len() != caller.arity() {
                    return Err(JokerError::Call(Argument(ArgumentError::report_error(
                        &expr.paren,
                        format!(
                            "call expected {} arguments but got {}.",
                            caller.arity(),
                            arguments.len()
                        ),
                    ))));
                }
                caller.call(self, &arguments)
            } else {
                Err(JokerError::Call(NonCallable(NonError::report_error(
                    &expr.paren,
                    format!("caller this object is not callable object: '{}'", callee),
                ))))
            };

        result
    }
    fn visit_lambda(&self, expr: &LambdaExpr) -> Result<Option<Object>, JokerError> {
        let lambda: Object = Object::new(OEnum::Caller(Caller::Func(Function::Lambda(
            Lambda::new(expr, Rc::clone(&self.run_env.borrow())),
        ))));
        Ok(Some(lambda))
    }
    fn visit_getter(&self, expr: &Getter) -> Result<Option<Object>, JokerError> {
        let object: Object = self.value_or_raise(
            &expr.name,
            &expr.expr,
            String::from("getter object invalid value."),
        )?;
        let result: Result<Option<Object>, JokerError> = match &*object.get() {
            OEnum::Instance(instance) => match instance.getter(&expr.name)? {
                Some(object) => Ok(Some(object)),
                None => Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.name,
                    format!(
                        "instance getter undefined attribute '{}'.",
                        expr.name.lexeme
                    ),
                ))),
            },
            OEnum::Caller(caller) => match caller {
                Caller::Class(class) => match class.getter(&expr.name)? {
                    Some(object) => Ok(Some(object)),
                    None => Err(JokerError::Interpreter(InterpreterError::report_error(
                        &expr.name,
                        format!("class getter undefined attribute '{}'.", expr.name.lexeme),
                    ))),
                },
                _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.name,
                    String::from("this caller not getter attribute."),
                ))),
            },
            OEnum::Literal(literal) => {
                Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.name,
                    format!("literal '{}' not getter attribute.", literal),
                )))
            }
        };

        result
    }
    fn visit_setter(&self, expr: &Setter) -> Result<Option<Object>, JokerError> {
        let object: Object = self.value_or_raise(
            &expr.name,
            &expr.l_expr,
            String::from("setter object invalid left value."),
        )?;
        // if this handle in object.get_mut, can raise error:
        //      - error: already mutably borrowed: BorrowError
        let value: Object = self.value_or_raise(
            &expr.name,
            &expr.r_expr,
            String::from("setter object invalid right value."),
        )?;
        let result: Result<Option<Object>, JokerError> = match &mut *object.get_mut() {
            OEnum::Instance(instance) => {
                instance.setter(&expr.name, value.clone())?;
                Ok(Some(value))
            }
            _ => Err(JokerError::Interpreter(InterpreterError::report_error(
                &expr.name,
                String::from("setter only instance have attribute."),
            ))),
        };

        result
    }
    fn visit_this(&self, expr: &This) -> Result<Option<Object>, JokerError> {
        self.look_up_variable(&expr.keyword, &Expr::This(expr.clone()))
    }
    fn visit_super(&self, expr: &Super) -> Result<Option<Object>, JokerError> {
        let binding: std::cell::Ref<HashMap<Expr, usize>> = self.local_resolve.borrow();
        if let Some(depth) = binding.get(&Expr::Super(expr.clone())) {
            if let Some(super_obj) = self
                .run_env
                .borrow()
                .borrow()
                .get_with_depth(*depth, &expr.keyword)?
            {
                if let Some(this_instance) = self
                    .run_env
                    .borrow()
                    .borrow()
                    .get_with_depth(*depth - 1, &Token::this(0))?
                {
                    let super_class: Class =
                        super_obj.parse::<Class>().map_err(|_err| -> JokerError {
                            JokerError::Parser(ParserError::report_error(
                                &expr.keyword,
                                String::from("This object is not class object, parse error."),
                            ))
                        })?;
                    match super_class.get_method(&expr.method.lexeme) {
                        Some(method) => {
                            if let OEnum::Instance(instance) = &*this_instance.get() {
                                return Ok(Some(Object::new(OEnum::Caller(Caller::Func(
                                    method.bind(*instance.clone()),
                                )))));
                            }
                        }
                        None => {
                            return Err(JokerError::Interpreter(InterpreterError::report_error(
                                &expr.method,
                                format!("super class undefined method '{}'.", expr.method.lexeme),
                            )))
                        }
                    }
                };
                return Err(JokerError::Interpreter(InterpreterError::report_error(
                    &expr.keyword,
                    String::from("super object need class instance, but not find this instance."),
                )));
            };
            Err(JokerError::Interpreter(InterpreterError::report_error(
                &expr.keyword,
                String::from("super object is declared, but not define."),
            )))
        } else {
            Err(JokerError::Interpreter(InterpreterError::report_error(
                &expr.keyword,
                String::from("env not have super pos info."),
            )))
        }
    }
}

#[derive(Debug)]
pub struct InterpreterError {
    line: usize,
    where_: String,
    msg: String,
}

impl InterpreterError {
    pub fn new(token: &Token, msg: String) -> InterpreterError {
        let where_: String = if token.ttype == TokenType::Eof {
            String::from(" at end")
        } else {
            format!(" at '{}'", token.lexeme)
        };
        InterpreterError {
            line: token.line,
            where_,
            msg,
        }
    }
    pub fn report_error(token: &Token, msg: String) -> InterpreterError {
        let inter_err = InterpreterError::new(token, msg);
        inter_err.report();
        inter_err
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "InterpreterError(line: {}, where: {}, msg: {})",
            self.line, self.where_, self.msg
        )
    }
}

impl Error for InterpreterError {}

impl ReportError for InterpreterError {
    fn report(&self) {
        eprintln!(
            "[line {}] where: '{}', \n\tmsg: {}\n",
            self.line, self.where_, self.msg
        );
    }
}

#[cfg(test)]
mod tests {

    use super::super::{error::ReportError, object::literal_null, token::Token};
    use super::*;

    fn maker_literal_i32_expr(v: i32) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: OEnum::Literal(ObL::I32(v)),
        }))
    }
    fn maker_literal_f64_expr(v: f64) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: OEnum::Literal(ObL::F64(v)),
        }))
    }
    fn maker_literal_str_expr(v: String) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: OEnum::Literal(ObL::Str(v)),
        }))
    }
    fn maker_literal_bool_expr(v: bool) -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: OEnum::Literal(ObL::Bool(v)),
        }))
    }
    fn maker_literal_null_expr() -> Box<Expr> {
        Box::new(Expr::Literal(Literal {
            value: OEnum::Literal(ObL::Null),
        }))
    }
    fn maker_token(ttype: TokenType) -> Token {
        let lexeme = ttype.to_string();
        Token {
            ttype,
            lexeme,
            literal: literal_null(),
            line: 0,
        }
    }
    fn maker_unary_expr(ttype: TokenType, v: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Unary(Unary {
            l_opera: maker_token(ttype),
            r_expr: v,
        }))
    }
    fn maker_grouping_expr(expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Grouping(Grouping { expr }))
    }
    fn maker_binary_expr(l_expr: Box<Expr>, m_opera: Token, r_expr: Box<Expr>) -> Box<Expr> {
        Box::new(Expr::Binary(Binary {
            l_expr,
            m_opera,
            r_expr,
        }))
    }

    #[test]
    fn test_simple_expr() {
        // (-123)*(200/2)
        let expr: Expr = Expr::Binary(Binary {
            l_expr: maker_unary_expr(TokenType::Minus, maker_literal_i32_expr(123)),
            m_opera: maker_token(TokenType::Star),
            r_expr: maker_grouping_expr(maker_binary_expr(
                maker_literal_i32_expr(200),
                maker_token(TokenType::Slash),
                maker_literal_i32_expr(2),
            )),
        });

        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Some(Object::new(OEnum::Literal(ObL::I32(-12300))))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_i32_expr() {
        let expr = maker_literal_i32_expr(32);
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Some(Object::new(OEnum::Literal(ObL::I32(32))))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_f64_expr() {
        let expr = maker_literal_f64_expr(320.0);
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Some(Object::new(OEnum::Literal(ObL::F64(320.0))))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_str_expr() {
        let expr = maker_literal_str_expr(String::from("string"));
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(
                value,
                Some(Object::new(OEnum::Literal(ObL::Str(String::from(
                    "string"
                )))))
            ),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_bool_expr() {
        let expr = maker_literal_bool_expr(true);
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Some(Object::new(OEnum::Literal(ObL::Bool(true))))),
            Err(err) => err.report(),
        };
    }

    #[test]
    fn test_literal_null_expr() {
        let expr = maker_literal_null_expr();
        let interpreter: Interpreter = Interpreter::new();
        match interpreter.evaluate(&expr) {
            Ok(value) => assert_eq!(value, Some(Object::new(OEnum::Literal(ObL::Null)))),
            Err(err) => err.report(),
        };
    }
}
