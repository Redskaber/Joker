//! This file is test resolve rs
//! 
//! 
//! - [] 我们的解析器会计算出变量是在哪个环境中找到的, 但是它仍然需要根据名称在对应的map中查找。
//!     一个更有效的环境表示形式是将局部变量保存在一个数组中，并通过索引来查找它们。
//! 
//! - []  扩展解析器，为作用域中声明的每个局部变量关联一个唯一的索引。
//!     当解析一个变量的访问时，查找变量所在的作用域及对应的索引，并保存起来。在解释器中，使用这个索引快速的访问一个变量。
//! 
//! 

use std::{collections::HashMap, ops::{Index, IndexMut}};


#[derive(Debug, Hash, PartialEq, Eq)]
enum TokenType {
    Eof,
    String,
}


#[derive(Debug, Hash, PartialEq, Eq)]
struct Token {
    ttype: TokenType,
    lexeme: String,
    line: usize,
}


#[derive(Debug)]
enum VarStore<T> {
    Var(T),
    Block(Vec<VarStore<T>>),
}

impl<T> VarStore<T> {
    pub fn len(&self) -> usize {
        match self {
            VarStore::Var(_) => 1,
            VarStore::Block(store) => store.len(),
        }
    }
    pub fn push(&mut self, var_store: VarStore<T>) -> Result<(), String> {
        match self {
            VarStore::Var(_) => Err(String::from("Can't push to VarStore::Var!")),
            VarStore::Block(store) => {
                store.push(var_store);
                Ok(())
            },
        }
    }
    pub fn pop(&mut self) -> Result<Option<VarStore<T>>, String> {
        match self {
            VarStore::Var(_) => Err(String::from("Can't pop to VarStore::Var!")),
            VarStore::Block(store) => Ok(store.pop())
        }
    }
    pub fn get(&self, index: usize) ->Result<Option<&VarStore<T>>, String> {
        match self {
            VarStore::Var(_) => Err(String::from("Can't find to VarStore::Var!")),
            VarStore::Block(store) => Ok(store.get(index))
        }
    }  
}

impl<T> Index<usize> for VarStore<T> {
    type Output = VarStore<T>;
    fn index(&self, index: usize) -> &Self::Output {
        match self {
            VarStore::Var(_) => &self,
            VarStore::Block(store) => &store[index]
        }
    }
}

impl<T> IndexMut<usize> for VarStore<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match self {
            VarStore::Var(_) => self,
            VarStore::Block(store) => &mut store[index]
        }
    }
}

#[derive(Debug)]
enum VarStatus {
    Declare,
    Define,
    Used,
}

impl Default for VarStatus {
    fn default() -> Self {
        VarStatus::Declare
    }
}

#[derive(Debug)]
struct VarLayer {
    layer: usize,
    index: usize,
}

impl Default for VarLayer {
    // layer: layers are based on the current environment 
    fn default() -> Self {
        VarLayer { layer: 0, index: 0 }
    }
}

#[derive(Debug)]
struct VarItem<T, L, S> {
    var: T,
    layer: L,
    status: S,
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Key<T>(T);


#[derive(Debug)]
struct Resolver {
    var_stores: VarStore<VarItem<Token, VarLayer, VarStatus>>,
    var_indexes: HashMap<Key<Token>, usize>
}


pub fn resolve_main(){
    let mut resolve = Resolver {
        var_stores: VarStore::Block(vec![
            VarStore::Var(
                VarItem { 
                    var: Token { ttype: TokenType::String, lexeme: String::from("var_0"), line: 0 }, 
                    layer: VarLayer::default(),
                    status: VarStatus::default(),
                }
            )
        ]),
        var_indexes: HashMap::from([
            (Key(Token { ttype: TokenType::String, lexeme: String::from("var_0"), line: 0 }), 0)
        ]),
    };

    println!("resolve: {:#?}", resolve);
    println!("resolve: {:#?}", resolve.var_stores.get(
                                    *resolve.var_indexes.get(
                                        &Key(Token { ttype: TokenType::String, lexeme: String::from("var_0"), line: 0 })
                                    ).unwrap()
                                )
                            );

    match resolve.var_stores.push(
        VarStore::Var(
            VarItem { 
                var: Token { ttype: TokenType::Eof, lexeme: String::from("var_1"), line: 1 }, 
                layer: VarLayer { layer: 0, index: resolve.var_stores.len() },
                status: VarStatus::Declare,
            }
        )
    ) { 
        Ok(_) => {
            resolve.var_indexes.insert(
                Key(Token { ttype: TokenType::Eof, lexeme: String::from("var_1"), line: 1 }),
                resolve.var_stores.len() -1
            );
            println!("resolve: {:#?}", resolve);
        }, 
        Err(err) => println!("push error: {}", err) 
    };
}




#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_var_store() {
        let mut var_stores: VarStore<Token> = VarStore::Block(vec![
            VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_0"), line: 0 }), 
            VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_1"), line: 1 }), 
            VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_2"), line: 2 }),
            VarStore::Block(vec![
                VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_3_0"), line: 3 }), 
                VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_3_1"), line: 4 }), 
                VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_3_2"), line: 5 }),
                VarStore::Block(vec![
                    VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_3_3_0"), line: 6 }), 
                    VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_3_3_1"), line: 7 }), 
                    VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_3_3_2"), line: 8 }),
                ]),
            ]),
        ]);
        
        // get
        println!("get var_stores:");
        println!("var_stores: {:?}", var_stores);
        println!("var_stores[3][1]: {:?}", var_stores[3][1]);
        // set
        println!("set var_stores:");
        var_stores[3][1] = VarStore::Var(Token { ttype: TokenType::Eof, lexeme: String::from("update_3_1"), line: 4 });
        println!("var_stores[3][1] = VarStore::Var(Token {{ ttype: TokenType::Eof, lexeme: String::from(\"update_3_1\"), line: 4 }});");
        println!("get var_stores:");
        println!("var_stores[3][1]: {:?}", var_stores[3][1]);
        // push
        println!("get var_stores len: {}", var_stores.len());
        if let Err(err) = var_stores.push(VarStore::Var(Token { ttype: TokenType::String, lexeme: String::from("var_new"), line: 9 })){
            println!("push error: {}", err);
        };
        // get
        println!("get var_stores:");
        println!("get var_stores len: {}", var_stores.len());
        println!("var_stores: {:?}", var_stores);
        // pop
        println!("get var_stores len: {}", var_stores.len());
        match var_stores.pop() {
            Ok(op_store) => println!("pop ok: {:?}", op_store),
            Err(err) => println!("pop error: {}", err),
        }
        // get
        println!("get var_stores:");
        println!("get var_stores len: {}", var_stores.len());
        println!("var_stores: {:?}", var_stores);
        // var store get fn
        println!("fn get var_stores:");
        println!("fn get var_stores len: {}", var_stores.len());
        println!("fn get last var: {:?}", var_stores.get(var_stores.len() - 1));
        println!("var_stores: {:?}", var_stores);
    }
}
