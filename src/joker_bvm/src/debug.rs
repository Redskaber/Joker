//! This file is Joker-language debug rs
//! 
//! 
//! 

use crate::chunk::{Chunk, OpCode};


#[derive(Debug)]
pub struct JokerError;

pub struct Debug;

impl Debug {
    pub fn disassemble_chunk(chunk: &Chunk, name: &str) -> Result<(), JokerError>{
        println!("== {name} ==");
        for (index, op_code) in chunk.get_codes().iter().enumerate() {
            Debug::disassemble_instruction(chunk, op_code, index)?
        }
        Ok(())
    }
    pub fn disassemble_instruction(chunk: &Chunk, op_code: &OpCode, index: usize) -> Result<(), JokerError> {
        print!("{:0>4}\t", index);
        match op_code {
            OpCode::OpReturn => Ok(Debug::simple_opcode_instruction(op_code)),
            OpCode::OpConstant(constant_index) => Debug::constant_instruction(chunk, op_code, constant_index),
            // OpCode::OpConstantLong(constant_long_index) => Debug::constant_instruction(chunk, op_code, constant_long_index),
        }
    }
    pub fn simple_opcode_instruction(op_code: &OpCode) {
        println!("{op_code}");
    }
    /*
        OpCode Constant:

     */
    pub fn constant_instruction(chunk: &Chunk, op_code: &OpCode, constant_index: &usize) -> Result<(), JokerError> {
        match chunk.get_value(*constant_index) {
            Some(constant_value) => {   
                println!("{:>16} {:0>4} {}", op_code, constant_index, constant_value);
                Ok(())
            }
            None => Err(JokerError {})
        }
    } 
}

