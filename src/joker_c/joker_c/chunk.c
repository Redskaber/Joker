#include <stdlib.h>
#include <stdio.h>

#include "chunk.h"
#include "memory.h"

/*
* panic_error function is used to print error message and exit the program.
* __declspec(noreturn) is used to indicate that the function does not return.
*/
static void __declspec(noreturn) panic_error(const char* message) {
    fprintf(stderr, "PANIC: %s\n", message);
    exit(EXIT_FAILURE);
}


void init_rle_lines(RleLines* rle_line) {
    rle_line->count = 0;
    rle_line->capacity = 0;
    rle_line->lines = NULL;
}

void free_rle_lines(RleLines* rle_line) {
    macro_free_array(RleLine, rle_line->lines, rle_line->capacity);
    init_rle_lines(rle_line);
}

// get the line of the given code count in the RLE lines
// return 0 if code count out of range
// Hint: It¡¯s not necessary for getLine() to be particularly efficient. 
//  Since it is called only when a runtime error occurs, 
//  it is well off the critical path where performance matters. 
line_t get_rle_line(RleLines* lines, index_t code_count) {
    uint32_t count = 0;

    for (uint32_t i = 0; i < lines->count; i++) {
        count += lines->lines[i].count;
        if (code_count < count) {
            return lines->lines[i].line;
        }
    }
    panic_error("[Chunk::get_rle_line] code count out of range");
}


// Chunk structure:
void init_chunk(Chunk* chunk) {
	chunk->count = 0;
	chunk->capacity = 0;
	chunk->code = NULL;
    init_rle_lines(&chunk->lines);
    init_value_array(&chunk->constants);
}


void free_chunk(Chunk* chunk) {
    macro_free_array(uint8_t, chunk->code, chunk->capacity);
    free_rle_lines(&chunk->lines);
    free_value_array(&chunk->constants);
    init_chunk(chunk);
}

/*
* It writes the given code and line to the chunk¡¯s code array 
* and also updates the RLE lines accordingly.
* op_constant and op_constant_long are encoded as uint8_t and uint16_t respectively.
*/
void write_chunk(Chunk* chunk, uint8_t code, line_t line) {
    if (chunk->capacity < chunk->count + 1) {
        int old_capacity = chunk->capacity;
        chunk->capacity = macro_grow_capacity(old_capacity);
        chunk->code = macro_grow_array(
            uint8_t, 
            chunk->code, 
            old_capacity, 
            chunk->capacity
        );
        chunk->lines.lines = macro_grow_array(
            RleLine,
            chunk->lines.lines,
            old_capacity,
            chunk->capacity
        );
    }

    // add line to RLE lines
    if (chunk->lines.count == 0 || chunk->lines.lines[chunk->lines.count-1].line != line) {
        if (chunk->lines.capacity < chunk->lines.count + 1) {
            int old_capacity = chunk->lines.capacity;
            chunk->lines.capacity = macro_grow_capacity(old_capacity);
            chunk->lines.lines = macro_grow_array(
                RleLine,
                chunk->lines.lines,
                old_capacity,
                chunk->lines.capacity
            );
        }
        chunk->lines.lines[chunk->lines.count].line = line;
        chunk->lines.lines[chunk->lines.count].count = 1;
        chunk->lines.count++;
    } else {
        chunk->lines.lines[chunk->lines.count-1].count++;
    }

    chunk->code[chunk->count] = code;
    chunk->count++;
}


static index_t add_value_to_chunk(Chunk* chunk, Value value) {
    write_value_array(&chunk->constants, value);
    return chunk->constants.count - 1;  // index of the added constant
}

/*
* It writes the given constant value to the chunk¡¯s code array
* and also updates the RLE lines accordingly.
* 
* TODO: what handle op_constant_long? uint16_t?
*  answer: used double call of write_chunk() to write op_constant_long and uint16_t
*/
void write_constant(Chunk* chunk, Value value, line_t line) {
    index_t index = add_value_to_chunk(chunk, value);
    if (index < UINT8_MAX) {
        write_chunk(chunk, op_constant, line);
        write_chunk(chunk, (uint8_t)index, line);
    } else {
        write_chunk(chunk, op_constant_long, line);
        write_chunk(chunk, (uint8_t)(index >> 8), line);
        write_chunk(chunk, (uint8_t)index, line);
    }
}

