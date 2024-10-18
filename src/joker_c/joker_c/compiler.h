#pragma once

#ifndef joker_compiler_h
#define joker_compiler_h

#include "common.h"
#include "function.h"
#include "vm.h"
#include "token.h"


/*
typedef enum {
    compile_success,
    compile_error,
} CompileResult;
*/


typedef struct LocalVariable {
    Token* name;
    int depth;
} LocalVariable;


typedef struct Compiler {
    struct Compiler* enclosing;                     // enclosing compiler
    LocalVariable locals[uint8_count];               // TODO: optimize
    Function* function;                              // top-level function: e.g. main()
    FunctionType function_type;                      // function type
    int local_count;
    int scope_depth;
} Compiler;


Function* compile(VirtualMachine* vm, const char* source);


void init_compiler(Compiler* self, FunctionType function_type);
void free_compiler(Compiler* self);

Chunk* current_chunk(Compiler* self);
void compile_begin_scope(Compiler* self);
void compile_end_scope(Compiler* self);


#endif /* joker_compiler_h */

