#pragma once

#ifndef joker_vm_h
#define joker_vm_h

#include "chunk.h"
#include "value.h"
#include "hashmap.h"

typedef struct Compiler Compiler;   // forward declaration


#define constent_stack_max 256      // static const int constent_stack_max = 256;


typedef enum {
    interpret_ok,
    interpret_compile_error,
    interpret_runtime_error
} InterpretResult;



typedef struct VirtualMachine {
    Chunk* chunk;                           // the bytecode chunk
    uint8_t* ip;                            // instruction pointer (current executing instruction pointer)
    Value stack[constent_stack_max];        // the stack
    Value* stack_top;                       // top of the stack
    HashMap strings;                        // string constants
    HashMap globals;                        // global variables
   Compiler* compiler;                      // the compiler
} VirtualMachine;


void init_virtual_machine(VirtualMachine* self);
void free_virtual_machine(VirtualMachine* self);

InterpretResult interpret(VirtualMachine* self, const char* source);

/* Value stack operations */
void push(VirtualMachine* self, Value value);
Value pop(VirtualMachine* self);
InterpretResult negate(VirtualMachine* self);



#endif /* joker_vm_h */

