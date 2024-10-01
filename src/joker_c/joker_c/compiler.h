#pragma once

#ifndef joker_compiler_h
#define joker_compiler_h
#include "vm.h"

typedef enum {
    compile_success,
    compile_error,
} CompileResult;


CompileResult compile(VirtualMachine* vm, Chunk* chunk, const char* source);


#endif /* joker_compiler_h */

