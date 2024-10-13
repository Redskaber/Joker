/*
	The bytecode virtual machine.
	This file defines the Value type used in the virtual machine.

	1. �������̶���С��ֵ�� -(ָ�) 
		-> immediate instructions( ��ʱָ��[ ���롢��䡢�ֽ�˳�� ] ): [opcode + immediate value ]
		
		ֵ�ı���λ������ָ����֮��
	2. �ַ��� (�ɱ��С��ֵ) -�������ƿ�ִ���ļ���-(����������)
		-> instuctions( ����ָ�� )�� [opcode + constant index]

		��ַ������������ָ����֮��

	---------------------------------------
	������( Constant Pool )��
		����ַ����������ַ��������ĵ�ַ�������ڶ����ƿ�ִ���ļ��С�
		��������һ��ֵ�����顣[arrary[0], arrary[1],... arrary[n]
		���س�����ָ���������������ȡֵ��[index]
	---------------------------------------

	instruction format( ָ���ʽ ): 
		[opcode + operand1 + operand2 + ... ]

		ÿ��������ᶨ�����ж��ٲ������Լ����Եĺ���
*/

#pragma once

#ifndef joker_value_h
#define joker_value_h

#include "common.h"

/* forward-declares*/
typedef struct Object Object;
typedef struct String String;


/* ֵ���� */
typedef enum {
	VAL_NULL = 0,
	VAL_I32  = 1,
	VAL_F64  = 2,
	VAL_BOOL = 3,
	VAL_OBJECT = 4,
} ValueType;



/* ֵ: �洢��ջ�е�ֵ 
* 
* TODO: Value Hasher
*/
typedef struct Value {
	ValueType type;
	union {
		int32_t		i32;
		double		f64;
		bool	boolean;
		Object*  object; // ָ��, object �������洢�ڶ���, �������ͨ��ǿתΪObject* ��������ȡtype ���Ա�ǩ��ͨ������ǿת��ȡ�����������
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


// ֵ��������
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
