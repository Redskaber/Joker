#pragma once

#ifndef joker_compiler_h
#define joker_compiler_h
#include "vm.h"
#include "token.h"

typedef enum {
    compile_success,
    compile_error,
} CompileResult;


typedef struct LocalVariable {
    Token* name;
    int depth;
} LocalVariable;


typedef struct Compiler {
    LocalVariable locals[max_local_variable_count];  // TODO: optimize
    int local_count;
    int scope_depth;
} Compiler;


CompileResult compile(VirtualMachine* vm, const char* source);


void init_compiler(Compiler* self);
void free_compiler(Compiler* self);

void compile_begin_scope(Compiler* self);
void compile_end_scope(Compiler* self);


#endif /* joker_compiler_h */

