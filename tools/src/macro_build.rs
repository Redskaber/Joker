//! This file is macro build ast;
//!
//!

// define_ast!(
//  enum_name {
//      arm_struct(f_name: f_type),
//      arm_struct(f_name: f_type),
//  },
//  trait_name,
//  trait_name,
// )
//

#[derive(Debug)]
struct Object;
#[derive(Debug)]
struct Token;
struct JokerError;
impl std::fmt::Display for Object {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
impl std::fmt::Display for Token {
    fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}


macro_rules! define_ast {
    (
        $ast_name:ident {
            $($struct_name:ident {
                $($field:ident : $field_type:ty),* $(,)?
            }),* $(,)?
        },
        $visitor_name:ident {
            $($visit_name:ident),* $(,)?
        },
        $acceptor_name:ident,
    ) => {
        use std::fmt::Display;

        // abstract tree enum
        #[derive(Debug)]
        pub enum $ast_name {
            $($struct_name($struct_name),)*
        }

        // subtree struct
        $(
        #[derive(Debug)]
        pub struct $struct_name {
            $(pub $field: $field_type),*
        }

        impl $struct_name {
            pub fn new($($field: $field_type),*) -> $struct_name {
                $struct_name { $($field),* }
            }
            pub fn into_expr($($field: $field_type),*) -> $ast_name {
                $ast_name::$struct_name($struct_name::new($($field),*))
            }
        }
        )*

        // visitor trait
        pub trait $visitor_name<T> {
            $(fn $visit_name(&self, expr: &$struct_name) -> Result<T, JokerError>;)*
        }

        // accept trait
        pub trait $acceptor_name<T> {
            fn accept(&self, visitor: &dyn $visitor_name<T>) -> Result<T, JokerError>;
        }

        impl<T> $acceptor_name<T> for $ast_name {
            fn accept(&self, visitor: &dyn $visitor_name<T>) -> Result<T, JokerError> {
                match self {
                    $($ast_name::$struct_name(expr) => visitor.$visit_name(expr),)*
                }
            }
        }

        $(
        impl<T> $acceptor_name<T> for $struct_name {
            fn accept(&self, visitor: &dyn $visitor_name<T>) -> Result<T, JokerError> {
                visitor.$visit_name(self)
            }
        }
        )*

        impl Display for $ast_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $($ast_name::$struct_name(expr) => Display::fmt(expr, f)),*
                }
            }
        }
        // sub display
        $(define_ast!{@impl_display $struct_name, $($field: $field_type),*})*
    };

    (@impl_display $struct_name:ident, $($field:ident : $field_type:ty),* $(,)?) => {
        define_ast!{@impl_display_inner $struct_name, $($field : $field_type),*}
    };

    (@impl_display_inner Literal, $field:ident : $field_type:ty) => {
        impl Display for Literal {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let format_args = match &self.$field {
                    Some(value) => value.to_string(),
                    None => "None".to_string(),
                };
                write!(f, "Literal(value: {})", format_args)
            }
        }
    };

    (@impl_display_inner $struct_name:ident, $($field:ident : $field_type:ty),* $(,)?) => {
        impl Display for $struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut fields = Vec::new();
                $(fields.push(format!("{}: {}", stringify!($field), self.$field).to_string());)*
                let format_args = fields.join(", ").trim_end_matches(", ").to_string();
                write!(f, "{}({})", stringify!($struct_name), format_args)
            }
        }
    };
}

define_ast! {
    Expr {
        Literal     { value: Option<Object> },
        Unary       { l_opera: Token, r_expr: Box<Expr> },
        Binary      { l_expr: Box<Expr>, m_opera: Token, r_expr: Box<Expr> },
        Trinomial   { l_expr: Box<Expr>, m_expr: Box<Expr>, r_expr: Box<Expr> },
        Grouping    { expr: Box<Expr> },
    },
    ExprVisitor     { visit_literal, visit_unary, visit_binary, visit_trinomial, visit_grouping },
    ExprAccept,
}
