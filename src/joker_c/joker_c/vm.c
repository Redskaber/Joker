#pragma once

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "value.h"
#include "debug.h"
#include "vm.h"


/* Virtual machine operations */
static void reset_stack(VirtualMachine* vm);
static InterpretResult run(VirtualMachine* vm);



void init_virtual_machine(VirtualMachine* vm) {
	reset_stack(vm);
}

void free_virtual_machine(VirtualMachine* vm) {

}

// Value stack operations
static void reset_stack(VirtualMachine* vm) {
	vm->stack_top = vm->stack;
}
static void stack_overflow() {
	fprintf(stderr, "Stack overflow\n");
	exit(1);
}
static void stack_underflow() {
	fprintf(stderr, "Stack underflow\n");
	exit(1);
}


void push(VirtualMachine* vm, Value value) {
	if (vm->stack_top >= vm->stack + constent_stack_max) {
		// raise overflow error, overflow the stack
		stack_overflow();
	}
	*vm->stack_top++ = value;
}

Value pop(VirtualMachine* vm) {
	if (vm->stack_top <= vm->stack) {
		// raise underflow error, underflow the stack
		stack_underflow();
	}
	return *(--vm->stack_top);
}

/* optimization negate operation, reduce stack top pointer change, improve performance */
void negate(VirtualMachine* vm) {
	if (vm->stack_top <= vm->stack) {
		// raise underflow error, underflow the stack
		stack_underflow();
	}
	*(vm->stack_top - 1) = -*(vm->stack_top - 1);
}

/*
* Interprets the given chunk of code.
* Returns an InterpretResult indicating the result of the interpretation.
*/
InterpretResult interpret(VirtualMachine* vm, const char* source) {
	// printf("Interpreting source code:\n%s\n", source);

	Chunk chunk;
	init_chunk(&chunk);

	if (!compile(vm, &chunk, source)) {
		free_chunk(&chunk);
		return interpret_compile_error;
	}

	vm->chunk = &chunk;
	vm->ip = chunk.code;	// set instruction pointer to start of code

	InterpretResult result = run(vm);
	free_chunk(&chunk);

	return result;
}


/*
* Runs the virtual machine.
* Returns an InterpretResult indicating the result of the interpretation.
* "direct threaded code"、"跳表" 和 "computed goto" 等技术可以优化字节码执行效率。
* 最快解决方案： 对C的非标准拓展 | 手写汇编代码
*/
static InterpretResult run(VirtualMachine* vm) {
/* read a byte from the current instruction pointer */
#define macro_read_byte() (*vm->ip++)
/* read a constant from the constant pool */
#define macro_read_constant(vm) (vm->chunk->constants.values[macro_read_byte()])
/* read*/
#define macro_read_binary_op(vm, op)\
    do {							\
		Value right = pop(vm);		\
		Value left = pop(vm);		\
		push(vm, left op right);	\
	} while(false)


	Value constant;

	while (true) {

/* debug_trace_execution */
#ifdef debug_trace_execution
		/* stack trace: print the current stack all at once.
		* 
		VirtualMachine:
			Value* stack_top = vm->stack_top; 
			Value* stack = vm->stack;
			for (int i = 0; i < constent_stack_max; i++) {

		*/
		printf("		");
		for (Value* slot = vm->stack; slot < vm->stack_top; slot++) {
			printf("[ ");
			print_value(*slot);
			printf(" ]");
		}
		printf("\n");


		/* print current instruction pointer.
		* 
		VirtualMachine: 
			chunk -> chunk->code [first instruction]
			ip -> current instruction pointer
		offset:
			vm->ip - vm->chunk->code
		*/
		disassemble_instruction(vm->chunk, (uint32_t)(vm->ip - vm->chunk->code));
#endif
		/* execute the current instruction.
		* 
		VirtualMachine:
			push(vm, constant);			// push constant to stack
			pop(vm);					// pop constant from stack
		*/
		uint8_t instruction = macro_read_byte();
		switch (instruction) {
			case op_add:
				macro_read_binary_op(vm, +);
				break;
			case op_subtract:
				macro_read_binary_op(vm, -);
				break;
			case op_multiply:
				macro_read_binary_op(vm, *);
				break;
			case op_divide:
				macro_read_binary_op(vm, /);
				break;
			case op_constant:
				constant = macro_read_constant(vm);	
				push(vm, constant);
				break;
			case op_constant_long:
				constant = macro_read_constant(vm);
				push(vm, constant);
				break;
			case op_negate:
				negate(vm);
				break;
			case op_return:
				print_value(pop(vm));
				printf("\n");
				return interpret_ok;
		}
	}

#undef macro_binary_op
#undef macro_read_constant
#undef macro_read_byte
}

/* just-in-time compilation(JIT) */

