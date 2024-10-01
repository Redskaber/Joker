/*
	The bytecode virtual machine.
	This file defines the Value type used in the virtual machine.

	1. 整数（固定大小的值） -(指令集) 
		-> immediate instructions( 即时指令[ 对齐、填充、字节顺序 ] ): [opcode + immediate value ]
		
		值的比特位紧跟在指令码之后。
	2. 字符串 (可变大小的值) -（二进制可执行文件）-(常量数据区)
		-> instuctions( 常量指令 )： [opcode + constant index]

		地址和索引紧跟在指令码之后。

	---------------------------------------
	常量池( Constant Pool )：
		存放字符串常量，字符串常量的地址和索引在二进制可执行文件中。
		常量池是一个值的数组。[arrary[0], arrary[1],... arrary[n]
		加载常量的指令根据数组索引获取值。[index]
	---------------------------------------

	instruction format( 指令格式 ): 
		[opcode + operand1 + operand2 + ... ]

		每个操作码会定义它有多少操作数以及各自的含义
*/

#pragma once

#ifndef joker_value_h
#define joker_value_h

#include "common.h"

typedef double Value;

// 值数组类型
typedef struct {
	uint32_t count;
	uint32_t capacity;
	Value* values;
} ValueArray;

void init_value_array(ValueArray* array);
void free_value_array(ValueArray* array);
void write_value_array(ValueArray* array, Value value);


void print_value(Value value);



#endif /* joker_value_h */

