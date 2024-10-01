#pragma once

#ifndef JOKER_CHUNK_H
#define JOKER_CHUNK_H

#include "common.h"
#include "value.h"

typedef uint32_t index_t;
typedef uint32_t line_t;

/*
 * OpCode: 操作码
 */
typedef enum
{
	op_return,		  //  8 bit
	op_constant,	  //  8 bit
	op_constant_long, // 24 bit
	op_add,			  //  8 bit(+)
	op_subtract,	  //  8 bit(-)
	op_multiply,	  //  8 bit(*)
	op_divide,		  //  8 bit(/)
	op_negate,		  //  8 bit(unary(-))
} OpCode;

typedef struct
{
	line_t line;
	uint32_t count;
} RleLine;

typedef struct
{
	uint32_t count;
	uint32_t capacity;
	RleLine *lines;
} RleLines;
void init_rle_lines(RleLines *lines);
void free_rle_lines(RleLines *lines);
line_t get_rle_line(RleLines *lines, index_t code_count);

/*
 * Chunk: 字节码块
 *  - count: 指令数量
 *  - capacity: 指令容量
 *  - code: 指令数组
 *
 *	指令数组中每个元素的类型为 uint8_t, 表示一个字节的指令.
 */
typedef struct
{
	uint32_t count;
	uint32_t capacity;
	uint8_t *code;		  // 指令数组 (操作码 | 操作数)
	RleLines lines;		  // 行号数组
	ValueArray constants; // 常量数组
} Chunk;
void init_chunk(Chunk *chunk);
void free_chunk(Chunk *chunk);
void write_chunk(Chunk *chunk, uint8_t code, line_t line);

void write_constant(Chunk *chunk, Value value, line_t line);

#endif /* joker_chunk_h */
