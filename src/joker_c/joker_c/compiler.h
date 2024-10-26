#pragma once

#ifndef joker_compiler_h
#define joker_compiler_h

#include "common.h"
#include "fn.h"
#include "vm.h"
#include "token.h"
#include "upvalue.h"

/* local variable */
typedef struct LocalVariable {
	Token* name;
	int depth;
	bool is_captured;
} LocalVariable;

typedef struct Compiler {
	struct Compiler* enclosing;                     // enclosing compiler
	LocalVariable locals[uint8_count];              // TODO: optimize
	int local_count;                                // function local variable count range [0, 127]

	int scope_depth;                                // compiler scope depth
	Fn* fn;                                         // function top-level function: e.g. main()
	FnType fn_type;                                 // function type
	upvalue_info_t upvalues[upvalue_index_count];   // function used local variable upvalue info list
} Compiler;

Fn* compile(VirtualMachine* vm, const char* source);

void init_compiler(Compiler* self, FnType function_type);
void free_compiler(Compiler* self);

Chunk* current_chunk(Compiler* self);
void compile_begin_scope(Compiler* self);
void compile_end_scope(Compiler* self);

#endif /* joker_compiler_h */
