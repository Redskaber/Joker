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

/* forward-declares*/
typedef struct Object Object;
typedef struct String String;


/* 值类型 */
typedef enum {
	VAL_NULL = 0,
	VAL_I32  = 1,
	VAL_F64  = 2,
	VAL_BOOL = 3,
	VAL_OBJECT = 4,
} ValueType;



/* 值: 存储在栈中的值 
* 
* TODO: Value Hasher
*/
typedef struct Value {
	ValueType type;
	union {
		int32_t		i32;
		double		f64;
		bool	boolean;
		Object*  object; // 指针, object 对象本身存储在堆上, 具体对象通过强转为Object* 类型来获取type 属性标签，通过二次强转获取具体对象类型
	} as;
} Value;

#define __panic(msg) \
	do { \
		fprintf(stderr, "Panic: %s\n", msg); \
		exit(1); \
	} while (0)	

#define macro_i32_from_val(i32_)	((Value){ VAL_I32,	{ .i32 = (i32_) } })
#define macro_f64_from_val(f64_)	((Value){ VAL_F64,	{ .f64 = (f64_) } })
#define macro_bool_from_val(bool_)	((Value){ VAL_BOOL, { .boolean = (bool_) } })
#define macro_obj_from_val(obj_)	((Value){ VAL_OBJECT, { .object = (Object*)(obj_) } })  // value need ownership of object
#define macro_null_var				((Value){ VAL_NULL, { .i32 = 0 } })

#define macro_is_i32(value)		((value).type == VAL_I32)
#define macro_is_f64(value)		((value).type == VAL_F64)
#define macro_is_number(value)	((value).type == VAL_I32 || (value).type == VAL_F64)
#define macro_is_bool(value)	((value).type == VAL_BOOL)
#define macro_is_obj(value)		((value).type == VAL_OBJECT)
#define macro_is_null(value)	((value).type == VAL_NULL)

#define macro_as_i32(value)		((value).as.i32)
#define macro_as_f64(value)		((value).as.f64)
#define macro_as_bool(value)	((value).as.boolean)
#define macro_as_obj(value)		((value).as.object)
#define macro_as_i32_ptr(value_ptr)		((value_ptr)->as.i32)
#define macro_as_f64_ptr(value_ptr)		((value_ptr)->as.f64)
#define macro_as_bool_ptr(value_ptr)	((value_ptr)->as.boolean)
#define macro_as_obj_ptr(value_ptr)		((value_ptr)->as.object)

#define macro_matches(value, t)			((value).type == (t))
#define macro_matches_ptr(value_ptr, t) ((value_ptr) && (value_ptr)->type == (t))

#define macro_type_name(value)				\
	(										\
		(value).type == VAL_I32 ? "i32" :	\
		(value).type == VAL_F64 ? "f64" :	\
		(value).type == VAL_BOOL? "bool" :	\
		(value).type == VAL_NULL? "null" :	\
		"unknown"							\
	)
#define macro_type_name_ptr(value_ptr) macro_type_name(*value_ptr)
#define macro_check_emptyptr(value_ptr)		\
	do {									\
		if (!(value_ptr)) {				    \
			__panic("Null pointer access detected in macro_check_emptyptr."); \
		}									\
	} while (0)


// 值数组类型
typedef struct {
	uint32_t count;
	uint32_t capacity;
	Value* values;
} ValueArray;



void init_value_array(ValueArray* array);
void free_value_array(ValueArray* array);
void write_value_array(ValueArray* array, Value value);
bool values_equal(Value left, Value right);

void print_value(Value value);
void printf_value(Value value);


#endif /* joker_value_h */

