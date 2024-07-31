use std::fmt::Display;
use super::{ast::{Binary, Expr, Literal, Statement, Unary, Variable}, Var};



// Explain yourself: build ast expr (Recursively distribute calls to your own display)
impl<'a> Display for Statement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Expr(expr) => Display::fmt(expr, f),
            Statement::Print(print) => Display::fmt(print, f),
            Statement::Var(left_var) => Display::fmt(left_var, f),
        }
    }
}

// Explain yourself: build ast expr (Recursively distribute calls to your own display)
impl<'a> Display for Var<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var {} = {};", self.name, self.value)
    }
}

// Explain yourself: build ast expr (Recursively distribute calls to your own display)
impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(literal) => Display::fmt(literal, f),
            Expr::Unary(unary) => Display::fmt(unary, f),
            Expr::Binary(binary) => Display::fmt(binary, f),
            Expr::Variable(right_var) => Display::fmt(right_var, f),
            Expr::Grouping(expr) => {
                let _ = write!(f, "( group ");
                let _ = Display::fmt(expr, f);
                write!(f, ")")
            }
        }
    }
}

// Explain yourself: build ast expr
impl<'a> Display for Literal<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Literal::I32(value) => write!(f, " {}", value),
            Literal::F64(value) => write!(f, " {}", value),
            Literal::Str(string) => write!(f, " \"{}\"", string),
            Literal::Bool(bool_) => write!(f, " {}", bool_),
            Literal::Null => write!(f, " null"),
        }
    }
}

// Explain yourself: transfer base type
impl<'a> TryInto<i32> for Literal<'a> {
    type Error = &'a str;
    fn try_into(self) -> Result<i32, Self::Error> {
        match self {
            Literal::I32(value) => Ok(value),
            _ => Err("[{:#?}] Literal is not an i32!"),
        }
    }
}
impl<'a> TryInto<f64> for Literal<'a> {
    type Error = &'a str;
    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            Literal::F64(value) => Ok(value),
            _ => Err("[{:#?}] Literal is not an f64!"),
        }
    }
}
impl<'a> TryInto<&'a str> for Literal<'a> {
    type Error = &'a str;
    fn try_into(self) -> Result<&'a str, Self::Error> {
        match self {
            Literal::Str(value) => Ok(value),
            _ => Err("[{:#?}] Literal is not an str!"),
        }
    }
}

// Explain yourself: build ast expr
impl<'a> Display for Unary<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "({} ", self.l_opera);
        let _ = Display::fmt(&self.r_expr, f);
        write!(f, ")")
    }
}

// Explain yourself: build ast expr
impl<'a> Display for Binary<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = write!(f, "(");
        let _ = Display::fmt(&self.m_opera, f);
        let _ = Display::fmt(&self.l_expr, f);
        let _ = Display::fmt(&self.r_expr, f);
        write!(f, ")")
    }
}

// Explain yourself: build ast expr
impl<'a> Display for Variable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Variable({})", self.name)
    }
}
