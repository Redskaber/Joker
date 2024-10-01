//! This file is joker-language-bytecode-virtual-machine main rs
//! 
//! 
mod common;
mod chunk;
// mod token;
mod value;
mod debug;


use chunk::{Chunk, OpCode};
use debug::Debug;
use value::Value;

fn main() {

    let mut chunk: Chunk = Chunk::new();
    chunk.push_value(Value::Constant(1000));
    chunk.push_code(OpCode::OpConstant(0));
    chunk.push_code(OpCode::OpReturn);
    println!("chunk: {:#?}", chunk);
    Debug::disassemble_chunk(&chunk, "Bytecode").unwrap();

}
