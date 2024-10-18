/*
* debug.c
* Bytecode Virtual Machine - Debugging functions
* 
* This file contains the implementation of the debugging functions used by the
* bytecode virtual machine.
*/

#include <stdio.h>

#include "debug.h"
#include "value.h"
#include "chunk.h"

// Forward declarations of helper functions
static int simple_instruction(const char* name, int offset);
static int constant_instruction(const char* name, Chunk* chunk, int offset);
static int constant_long_instruction(const char* name, Chunk* chunk, int offset);
static int byte_instruction(const char* name, Chunk* chunk, int offset);
static int jump_instruction(const char* name, int sign, Chunk* chunk, int offset);



void disassemble_chunk(Chunk* chunk, const char* name) {
    printf("== %s ==\n", name);
    for (int offset = 0; offset < chunk->count;) {
        // return next instruction offset
        offset = disassemble_instruction(chunk, offset);
    }
}

int disassemble_instruction(Chunk* chunk, int offset) {
    printf("%04d ", offset);

    // print line number (if changed)
    line_t current_line = get_rle_line(&chunk->lines, offset);
    if (offset > 0 && current_line == get_rle_line(&chunk->lines, offset - 1)) {
        printf("   | ");
    } else {
        printf("%4d ", current_line);
    }
    // print instruction
    uint8_t instruction = chunk->code[offset];
    switch (instruction) {
    case op_null:
        return simple_instruction("op_null", offset);
    case op_true:
        return simple_instruction("op_true", offset);
    case op_false:
        return simple_instruction("op_false", offset);
    case op_pop:
        return simple_instruction("op_pop", offset);
    case op_define_global:
        return constant_instruction("op_define_global", chunk, offset);
    case op_set_global:
        return constant_instruction("op_set_global", chunk, offset);
    case op_get_global:
        return constant_instruction("op_get_global", chunk, offset);
    case op_set_local:
        return byte_instruction("op_set_local", chunk, offset);
    case op_get_local:
        return byte_instruction("op_get_local", chunk, offset);
    case op_equal:
        return simple_instruction("op_equal", offset);
    case op_not_equal:
        return simple_instruction("op_not_equal", offset);
    case op_greater:
        return simple_instruction("op_greater", offset);
    case op_less:
        return simple_instruction("op_less", offset);
    case op_greater_equal:
        return simple_instruction("op_greater_equal", offset);
    case op_less_equal:
        return simple_instruction("op_less_equal", offset);
    case op_add:
        return simple_instruction("op_add", offset);
    case op_subtract:
        return simple_instruction("op_subtract", offset);
    case op_multiply:
        return simple_instruction("op_multiply", offset);
    case op_divide:
        return simple_instruction("op_divide", offset);
    case op_constant:
        return constant_instruction("op_constant", chunk, offset);
    case op_constant_long:
        return constant_long_instruction("op_constant_long", chunk, offset);
    case op_not:
        return simple_instruction("op_not", offset);
    case op_negate:                                                         // unary operator(-)
        return simple_instruction("op_negate", offset);
    case op_print:
        return simple_instruction("op_print", offset);
    case op_jump:
        return jump_instruction("op_jump", 1, chunk, offset);
    case op_jump_if_false:
        return jump_instruction("op_jump_if_false", 1, chunk, offset);
    case op_loop:
        return jump_instruction("op_loop", -1, chunk, offset);
    case op_call:
        return byte_instruction("op_call", chunk, offset);
    case op_return:
        return simple_instruction("op_return", offset);
    default:
        printf("unknown opcode %d\n", instruction);
        return offset + 1;
     }
}


static int simple_instruction(const char* name, int offset) {
    printf("%-16s\n", name);
    return offset + 1;
}

static int constant_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t constant_offset = chunk->code[offset + 1];
    printf("%-16s %4d '", name, constant_offset);
    print_value(chunk->constants.values[constant_offset]);
    printf("'\n");
    return offset + 2;
}

static int constant_long_instruction(const char* name, Chunk* chunk, int offset) {
    uint16_t constant_offset = (uint16_t)(chunk->code[offset + 1] << 8) | chunk->code[offset + 2];
    printf("%-16s %4d '", name, constant_offset);
    print_value(chunk->constants.values[constant_offset]);
    printf("'\n");
    return offset + 3;
}

static int byte_instruction(const char* name, Chunk* chunk, int offset) {
    uint8_t slot = chunk->code[offset + 1];
    printf("%-16s %4d\n", name, slot);
    return offset + 2;
}

static int jump_instruction(const char* name, int sign, Chunk* chunk, int offset) {
    uint16_t jump_offset = (uint16_t)(chunk->code[offset + 1] << 8) | chunk->code[offset + 2];
    printf("%-16s %4d -> %d\n", name, sign * jump_offset, offset + 3 + sign * jump_offset);
    return offset + 3;
}

