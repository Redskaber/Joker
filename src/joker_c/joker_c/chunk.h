#pragma once

#ifndef JOKER_CHUNK_H
#define JOKER_CHUNK_H

#include "common.h"
#include "value.h"

/* Error */

/*
 * OpCode: ������
 */
typedef enum Opcode {
	op_return,		  //  8 bit
	op_break,		  // 16 bit(break + offset[8bit])							// short_jump
	op_constant,	  // 16 bit(constant + offset[8bit])
	op_constant_long, // 24 bit(constant_long + offset[high 8 bit, low 8 bit])
	op_null,		  //  8 bit
	op_true,		  //  8 bit
	op_false,		  //  8 bit
	op_not,			  //  8 bit(unary!)
	op_negate,		  //  8 bit(unary(-))
	op_print,		  //  8 bit(print)
	op_pop,			  //  8 bit(pop)
	op_define_global, // 16 bit(define_global + index)
	op_set_global,	  // 16 bit(set_global + index)
	op_get_global,	  // 16 bit(get_global + index)
	op_set_local,	  // 16 bit(set_local + index)
	op_get_local,	  // 16 bit(get_local + index)
	op_get_upvalue,	  // 16 bit(get_upvalue + index)
	op_set_upvalue,	  // 16 bit(set_upvalue + index)
	op_jump_if_false, // 24 bit(jump_if_false + offset[high 8 bit, low 8 bit])
	op_jump,		  // 24 bit(jump + offset[high 8 bit, low 8 bit])
	op_loop,		  // 24 bit(loop + offset[high 8 bit, low 8 bit])
	op_call,		  // 16 bit(call + offset[8 bit])
	op_closure,		  // 16 bit(closure + offset[is_local{1bit}, index{7bit}])
	op_close_upvalue, //  8 bit(close_upvalue)
	op_equal,		  //  8 bit(==)
	op_not_equal,	  //  8 bit(!=)
	op_less,		  //  8 bit(<)
	op_less_equal,	  //  8 bit(<=)
	op_greater,		  //  8 bit(>)
	op_greater_equal, //  8 bit(>=)
	op_add,			  //  8 bit(+)
	op_subtract,	  //  8 bit(-)
	op_multiply,	  //  8 bit(*)
	op_divide,		  //  8 bit(/)
} OpCode;

typedef struct RleLine {
	line_t line;
	int count;
} RleLine;

typedef struct RleLines {
	int count;
	int capacity;
	RleLine* lines;
} RleLines;
void init_rle_lines(RleLines* lines);
void free_rle_lines(RleLines* lines);
line_t get_rle_line(RleLines* lines, index_t code_count);

/*
 * Chunk: �ֽ����
 *  - count: ָ������
 *  - capacity: ָ������
 *  - code: ָ������
 *
 *	ָ��������ÿ��Ԫ�ص�����Ϊ uint8_t, ��ʾһ���ֽڵ�ָ��.
 */
typedef struct Chunk {
	int count;
	int capacity;
	uint8_t* code;		  // ָ������ (������ | ������)
	RleLines lines;		  // �к�����
	ValueArray constants; // ��������
} Chunk;
void init_chunk(Chunk* chunk);
void free_chunk(Chunk* chunk);
void write_chunk(Chunk* chunk, uint8_t code, line_t line);

index_t add_constant(Chunk* chunk, Value value);
void write_constant(Chunk* chunk, Value value, line_t line);

#endif /* joker_chunk_h */
