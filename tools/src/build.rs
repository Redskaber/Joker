//! generate ast.
//!
//!
use std::{
    fs::File,
    io::{self, Write},
};

#[derive(Debug)]
struct TreeType {
    pub struct_name: String,
    pub fields: Vec<String>,
}

pub fn generate_ast(output_dir: &String) -> io::Result<()> {
    let _ = define_ast(
        output_dir,
        &String::from("Expr"),
        &[
            String::from("Literal    : value Option<Object>"),
            String::from("Unary      : l_opera Token, r_expr Box<Expr>"),
            String::from("Binary     : l_expr Box<Expr>, m_opera Token, r_expr Box<Expr>"),
            String::from("Grouping   : expr Box<Expr>"),
        ],
    );
    Ok(())
}

fn define_ast(output_dir: &String, base_name: &String, types: &[String]) -> io::Result<()> {
    let path: String = format!("{output_dir}/{}.rs", base_name.to_lowercase());
    let mut file: File = File::create(path)?;
    let mut tree_types: Vec<TreeType> = Vec::new();

    // header
    writeln!(file, "use super::super::{{")?;
    writeln!(file, "    r#type::Object,")?;
    writeln!(file, "    error::JokerError,")?;
    writeln!(file, "    token::Token,")?;
    writeln!(file, "}};\n\n")?;

    // enum
    writeln!(file, "pub enum {base_name} {{")?;
    for ttype in types {
        let (base_struct_name, fields_text) = ttype.split_once(':').unwrap();
        let struct_name = base_struct_name.trim().to_string();
        // item
        writeln!(file, "    {struct_name}({struct_name}),")?;

        let field_split = fields_text.split(',');
        let mut fields: Vec<String> = Vec::new();
        for arg in field_split {
            let (f_name, f_type) = arg.trim().split_once(' ').unwrap();
            fields.push(format!("{f_name}: {f_type}"));
        }
        tree_types.push(TreeType {
            struct_name,
            fields,
        });
    }
    writeln!(file, "}}\n")?;

    // sub_type
    for tree in &tree_types {
        define_type(&mut file, tree)?;
    }

    // visitor
    writeln!(file, "impl<T> {base_name}Visitor<T> for {base_name} {{")?;
    writeln!(
        file,
        "    fn accept(&self, visitor: &dyn {base_name}Visitor<T>) -> Result<T, JokerError> {{"
    )?;
    writeln!(file, "        match self {{")?;
    for tree in &tree_types {
        let sub_name = &tree.struct_name;
        let sub_arg_name = sub_name.to_lowercase();
        writeln!(
            file,
            "            {}::{}({}) => {}.accept(visitor),",
            base_name, sub_name, sub_arg_name, sub_arg_name
        )?;
    }
    writeln!(file, "        }}")?;
    writeln!(file, "    }}")?;
    writeln!(file, "}}\n")?;
    define_visitor(&mut file, base_name, &tree_types)?;

    // accept
    define_accept(&mut file, base_name, &tree_types)?;
    Ok(())
}

fn define_type(file: &mut File, tree: &TreeType) -> io::Result<()> {
    writeln!(file, "pub struct {} {{", tree.struct_name)?;
    for field in &tree.fields {
        writeln!(file, "    pub {field},")?;
    }
    writeln!(file, "}}\n")?;

    Ok(())
}

fn define_visitor(
    file: &mut File,
    base_name: &String,
    tree_types: &Vec<TreeType>,
) -> io::Result<()> {
    writeln!(file, "pub trait {base_name}Visitor<T> {{")?;
    for ttype in tree_types {
        writeln!(
            file,
            "    fn visit_{}(&self, expr: &{}) -> Result<T, JokerError>;",
            ttype.struct_name.to_lowercase(),
            ttype.struct_name
        )?;
    }
    writeln!(file, "}}\n")?;
    Ok(())
}

fn define_accept(
    file: &mut File,
    base_name: &String,
    tree_types: &Vec<TreeType>,
) -> io::Result<()> {
    writeln!(file, "pub trait {base_name}Accept<T> {{")?;
    writeln!(
        file,
        "    fn accept(&self, visitor: &dyn {base_name}Visitor<T>) -> Result<T, JokerError>;"
    )?;
    writeln!(file, "}}\n")?;

    for tree in tree_types {
        writeln!(
            file,
            "impl<T> {base_name}Accept<T> for {} {{",
            tree.struct_name
        )?;
        writeln!(
            file,
            "    fn accept(&self, visitor: &dyn {base_name}Visitor<T>) -> Result<T, JokerError> {{"
        )?;
        writeln!(
            file,
            "        visitor.visit_{}(self)",
            tree.struct_name.to_lowercase()
        )?;
        writeln!(file, "    }}")?;
        writeln!(file, "}}\n")?;
    }
    Ok(())
}