//! This is Joker-language chunk rs
//! 
//!     rust: 对齐方式中，使用repr(align(number_byte)) 指定
//!         所以， 在rust 中最小的存储单元是 1byte.
//! 
//! 
// use crate::common;

use std::fmt::{Display, Debug};

use crate::value::Value;


#[derive(Debug)]
#[repr(align(1))]   // 8bit
pub enum OpCode {
    OpReturn,
    OpConstant(usize),
    // OpConstantLong(usize),
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpReturn => write!(f, "OpReturn"),
            OpCode::OpConstant(value) => write!(f, "OpConstant({value})"),
            // OpCode::OpConstantLong(value) => write!(f, "OpConstantLong({value})"),
        }
    }
}


#[derive(Debug)]
pub struct Chunk {
    codes: Vec<OpCode>,        // opcode | operand  { Vec::length, capacity }
    values: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { 
            codes: Vec::new(),
            values: Vec::new(), 
        }
    }
    pub fn get_codes(&self) -> &Vec<OpCode> {
        &self.codes
    }
    pub fn push_code(&mut self, code: OpCode)  {
        self.codes.push(code);
    }
    pub fn len_code(&self) -> usize {
        self.codes.len()
    }
    pub fn get_code(&self, index: usize) -> Option<&OpCode> {
        self.codes.get(index)
    }
    pub fn push_value(&mut self, value: Value) {
        self.values.push(value);
    }
    pub fn get_value(&self, index: usize) -> Option<&Value> {
        self.values.get(index)
    }
    pub fn len_value(&self) -> usize {
        self.values.len()
    }
}

#[cfg(test)]
mod tests {
    use std::mem::size_of;      // size  (实际存储大小)
    use std::mem::align_of;     // align (内存对齐大小)
    use super::*;

    #[test]
    pub fn test_opcode_align() {
        let size: usize = size_of::<OpCode>();
        let align: usize = align_of::<OpCode>();
        println!("{size} {align}");
        assert_eq!(size, 24);
        assert_eq!(align, 8);
    }
    #[test]
    pub fn test_value_align() {
        let size: usize = size_of::<Value>();
        let align: usize = align_of::<Value>();
        assert_eq!(size, 16);
        assert_eq!(align, 8);
    }
}
