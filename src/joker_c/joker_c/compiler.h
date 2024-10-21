#pragma once

#ifndef joker_compiler_h
#define joker_compiler_h

#include "common.h"
#include "fn.h"
#include "vm.h"
#include "token.h"



typedef struct LocalVariable {
    Token* name;
    int depth;
} LocalVariable;


typedef struct Compiler {
    struct Compiler* enclosing;                     // enclosing compiler
    LocalVariable locals[uint8_count];              // TODO: optimize
    Fn* function;                                   // top-level function: e.g. main()
    FnType function_type;                           // function type
    int local_count;
    int scope_depth;
} Compiler;


Fn* compile(VirtualMachine* vm, const char* source);


void init_compiler(Compiler* self, FnType function_type);
void free_compiler(Compiler* self);

Chunk* current_chunk(Compiler* self);
void compile_begin_scope(Compiler* self);
void compile_end_scope(Compiler* self);


#endif /* joker_compiler_h */

