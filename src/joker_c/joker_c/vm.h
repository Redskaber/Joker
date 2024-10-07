#pragma once

#ifndef joker_vm_h
#define joker_vm_h

#include "chunk.h"
#include "value.h"

#define constent_stack_max 256  // static const int constent_stack_max = 256;


typedef enum {
    interpret_ok,
    interpret_compile_error,
    interpret_runtime_error
} InterpretResult;



typedef struct {
    Chunk* chunk;               // the bytecode chunk
    uint8_t* ip;                // instruction pointer (current executing instruction pointer)
    Value stack[constent_stack_max];        // the stack
    Value* stack_top;                       // top of the stack
} VirtualMachine;


void init_virtual_machine(VirtualMachine* vm);
void free_virtual_machine(VirtualMachine* vm);

InterpretResult interpret(VirtualMachine* vm, const char* source);

/* Value stack operations */
void push(VirtualMachine* vm, Value value);
Value pop(VirtualMachine* vm);
InterpretResult negate(VirtualMachine* vm);



#endif /* joker_vm_h */

