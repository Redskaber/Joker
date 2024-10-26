#pragma once

#ifndef joker_vm_h
#define joker_vm_h

#include "common.h"
#include "chunk.h"
#include "native.h"
#include "closure.h"
#include "value.h"
#include "hashmap.h"

typedef struct Compiler Compiler;           // forward declaration

#define frames_statck_max 64                // static const int frames_statck_max = 64;
#define constent_stack_max (frames_statck_max * uint8_count)

typedef struct CallFrame {
	Value* base_pointer;                    // vm stack get function base address. (ptr ->slots {Value, Value, ...})
	Closure* closure;                       // the closure of the function
	uint8_t* ip;                            // func self instruction pointer. (return address: func ip execute over call caller ip continue execute)
} CallFrame;
/*
fn func1() {
	func2();
}
fn func2() {
	func3();
}
fn func3() {
	return 1;
}
call func1()
	-> execute func1->ip     (over func1)   ¡ü continue execute up_func->ip
	-> execute func2->ip     (over func2)   | continue execute func1->ip
	-> execute func3->ip  ---(over func3)---> continue exucute func2->ip
*/

typedef enum InterpretResult {
	interpret_ok,
	interpret_compile_error,
	interpret_runtime_error
} InterpretResult;

typedef struct VirtualMachine {
	CallFrame frames[frames_statck_max];    // the func stack: {vm->ip} goto {vm->frames[index]->ip}
	int frame_count;                        // the call stack count
	Value stack[constent_stack_max];        // the stack
	Value* stack_top;                       // top of the stack

	HashMap strings;                        // string constants
	HashMap globals;                        // global variables
	UpvaluePtr open_upv_ptr;                // upvalue pointer header node
	Compiler* compiler;                     // the compiler
} VirtualMachine;

void init_virtual_machine(VirtualMachine* self);
void free_virtual_machine(VirtualMachine* self);

Compiler* replace_compiler(VirtualMachine* self, Compiler* compiler);
InterpretResult interpret(VirtualMachine* self, const char* source);

/* Value stack operations */
void push(VirtualMachine* self, Value value);
Value pop(VirtualMachine* self);
InterpretResult negate(VirtualMachine* self);

#endif /* joker_vm_h */
