#pragma once

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "value.h"
#include "memory.h"
#include "object.h"
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

static void runtime_error(VirtualMachine* vm, const char* message, ...) {
	va_list args;
	va_start(args, message);
	vfprintf(stderr, message, args);
	va_end(args);
	fputs("\n", stderr);

	ptrdiff_t instrution = vm->ip - vm->chunk->code - 1;
	line_t line = get_rle_line(&vm->chunk->lines, instrution);
	fprintf(stderr, "[line %d] where: at runtime error in script.\n", line);
	reset_stack(vm);
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

static Value* peek(VirtualMachine* vm, int distance) {
	if (vm->stack_top - distance < vm->stack) {
		// raise underflow error, underflow the stack
		stack_underflow();
	}
	// return the value at the given distance from the top of the stack
	return (vm->stack_top - 1 - distance);
}

/* optimization negate operation, reduce stack top pointer change, improve performance */
InterpretResult negate(VirtualMachine* vm) {
	if (vm->stack_top <= vm->stack) {
		// raise underflow error, underflow the stack
		stack_underflow();
	}
	Value* slot = peek(vm, 0);
	switch (slot->type)  
	{
	case VAL_I32: slot->as.i32 = -slot->as.i32; break;
	case VAL_F64: slot->as.f64 = -slot->as.f64; break;
	default:
		//  raise runtime error, expected number for negate operation, found...
		runtime_error(vm, "Expected number for negate operation, Found...");
		return interpret_runtime_error;
	}
	return interpret_ok;
}

InterpretResult not_(VirtualMachine* vm) {
	if (vm->stack_top <= vm->stack) {
		// raise underflow error, underflow the stack
		stack_underflow();
	}
	Value* slot = peek(vm, 0);
	switch (slot->type)  
	{
	case VAL_BOOL: slot->as.boolean = !slot->as.boolean; break;
	default:
		//  raise runtime error, expected boolean for not operation, found...
		runtime_error(vm, "Expected boolean for not operation, Found...");
		return interpret_runtime_error;
	}
	return interpret_ok;
}

static void concatenate_string(VirtualMachine* vm) {
	StringObject* right = macro_as_string(pop(vm));
	StringObject* left = macro_as_string(pop(vm));
	push(vm, macro_obj_from_val(concat_string(left, right)));
}

/*
* Interprets the given chunk of code.
* Returns an InterpretResult indicating the result of the interpretation.
*/
InterpretResult interpret(VirtualMachine* vm, const char* source) {
	// printf("Interpreting source code:\n%s\n", source);

	Chunk chunk;
	init_chunk(&chunk);

	if (compile(vm, &chunk, source) == compile_error) {
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
/* read a binary operation from the current instruction pointer : TODO: 将binary等操作符的实现分离出来，单独类型判断*/
#define macro_read_binary_op(vm, op, is_cmp)														\
    do {																							\
        Value* r_slot = peek(vm, 0);																\
        Value* l_slot = peek(vm, 1);																\
        macro_check_emptyptr(r_slot);																\
        macro_check_emptyptr(l_slot);																\
        if (macro_matches_ptr(r_slot, VAL_I32) && macro_matches_ptr(l_slot, VAL_I32)) {				\
		    int r_i32 = macro_as_i32(pop(vm));														\
		    int l_i32 = macro_as_i32(pop(vm));														\
		    is_cmp ? push(vm, macro_bool_from_val(l_i32 op r_i32)) :								\
				push(vm, macro_i32_from_val(l_i32 op r_i32));										\
		} else if (macro_matches_ptr(r_slot, VAL_F64) && macro_matches_ptr(l_slot, VAL_F64)) {		\
			double r_f64 = macro_as_f64(pop(vm));													\
			double l_f64 = macro_as_f64(pop(vm));													\
			is_cmp ? push(vm, macro_bool_from_val(l_f64 op r_f64)) :								\
				push(vm, macro_f64_from_val(l_f64 op r_f64));										\
		} else {																					\
			runtime_error(vm, "[line %d] where: at runtime binary '"#op "' operation.\n"			\
				"\tExpected number[(i32,i32), (f64,f64)] for left and right operands, "				\
					"Found left: %s, right: %s",													\
				get_rle_line(&vm->chunk->lines, vm->ip - vm->chunk->code - 1),						\
				macro_type_name_ptr(l_slot), macro_type_name_ptr(r_slot)							\
			);																						\
			return interpret_runtime_error;															\
		}																							\
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
		case op_constant:
			constant = macro_read_constant(vm);
			push(vm, constant);
			break;
		case op_constant_long:
			constant = macro_read_constant(vm);
			push(vm, constant);
			break;
		case op_negate: negate(vm); break;
		case op_not:	not_(vm); break;
		case op_null:	push(vm, macro_null_var); break;
		case op_true:	push(vm, macro_bool_from_val(true)); break;
		case op_false:	push(vm, macro_bool_from_val(false)); break;
		case op_equal: {
			Value right = pop(vm);
			Value left = pop(vm);
			push(vm, macro_bool_from_val(values_equal(left, right)));
			break;
		}
		case op_not_equal: {
			Value right = pop(vm);
			Value left = pop(vm);
			push(vm, macro_bool_from_val(!values_equal(left, right)));
			break;
		}
		case op_greater:		macro_read_binary_op(vm, >, true); break;
		case op_greater_equal:	macro_read_binary_op(vm, >=, true); break;
		case op_less:			macro_read_binary_op(vm, <, true); break;
		case op_less_equal:		macro_read_binary_op(vm, <=, true); break;
		case op_add: {
			Value* r_slot = peek(vm, 0);
			Value* l_slot = peek(vm, 1);
			macro_check_emptyptr(r_slot);
			macro_check_emptyptr(l_slot);
			if (macro_is_string(*l_slot) && macro_is_string(*r_slot)) {
				concatenate_string(vm);
			}
			else {
				macro_read_binary_op(vm, +, false);
			}
			break;
		}
		case op_subtract:		macro_read_binary_op(vm, -, false); break;
		case op_multiply:		macro_read_binary_op(vm, *, false); break;
		case op_divide:			macro_read_binary_op(vm, /, false); break;
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

