#pragma once

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "value.h"
#include "string.h"
#include "memory.h"
#include "object.h"
#include "debug.h"
#include "vm.h"


/* Virtual machine operations */
static int current_code_index(VirtualMachine* self);
static void reset_stack(VirtualMachine* self);
static InterpretResult run(VirtualMachine* self);


static __declspec(noreturn) void panic_error(const char* msg, ...) {
	va_list args;
	va_start(args, msg);
	vfprintf(stderr, msg, args);
	va_end(args);
	fputs("\n", stderr);

	exit(EXIT_FAILURE);
}

// pointer arithmetic to convert stack index to uint32_t for stack access
static int convert_check(ptrdiff_t index) {
	if (index < 0 || index >= INT32_MAX) {
		panic_error("[VirtualMachine::convert_check] pointer arithmetic overflow.");
	}
	return (int)index;
}


static void runtime_error(VirtualMachine* self, const char* message, ...) {
	va_list args;
	va_start(args, message);
	vfprintf(stderr, message, args);
	va_end(args);
	fputs("\n", stderr);

	index_t code_index = current_code_index(self);
	line_t line = get_rle_line(&self->chunk->lines, code_index);
	fprintf(stderr, "[line %d] where: at runtime error in script.\n", line);
	reset_stack(self);
}


/*
* vm->ip: 指向下一条需要执行的指令的指针
* vm->chunk->code: 指向代码段的起始地址
* vm->ip - vm->chunk->code - 1: 指向当前指令的偏移量
*/
static int current_code_index(VirtualMachine* self) {
	return convert_check(self->ip - self->chunk->code - 1);
}




void init_virtual_machine(VirtualMachine* self) {
	self->chunk = NULL;
	self->ip = NULL;
	self->compiler = NULL;
	reset_stack(self);
	init_hashmap(&self->strings); // 字符串驻留
	init_hashmap(&self->globals); // 全局变量
}

void free_virtual_machine(VirtualMachine* self) {
	free_hashmap(&self->strings); // 字符串驻留
	free_hashmap(&self->globals); // 全局变量
}

// Value stack operations
static void reset_stack(VirtualMachine* self) {
	self->stack_top = self->stack;
}



static void push(VirtualMachine* self, Value value) {
	if (self->stack_top >= self->stack + constent_stack_max) {
		// raise overflow error, overflow the stack
		panic_error("[VirtualMachine::push] stack overflow.");
	}
	*self->stack_top++ = value;
}

static Value pop(VirtualMachine* self) {
	if (self->stack_top <= self->stack) {
		// raise underflow error, underflow the stack
		panic_error("[VirtualMachine::pop] stack underflow.");
	}
	return *(--self->stack_top);
}

static Value* peek(VirtualMachine* self, int distance) {
	if (self->stack_top - distance < self->stack) {
		// raise underflow error, underflow the stack
		panic_error("[VirtualMachine::peek] stack underflow.");
	}
	// return the value at the given distance from the top of the stack
	return (self->stack_top - 1 - distance);
}

/* optimization negate operation, reduce stack top pointer change, improve performance */
static InterpretResult negate(VirtualMachine* self) {
	if (self->stack_top <= self->stack) {
		// raise underflow error, underflow the stack
		panic_error("[VirtualMachine::negate] stack underflow.");
	}
	Value* slot = peek(self, 0);
	switch (slot->type)  
	{
	case VAL_I32: slot->as.i32 = -slot->as.i32; break;
	case VAL_F64: slot->as.f64 = -slot->as.f64; break;
	default:
		//  raise runtime error, expected number for negate operation, found...
		runtime_error(self, "Expected number for negate operation, Found...");
		return interpret_runtime_error;
	}
	return interpret_ok;
}

static InterpretResult not_(VirtualMachine* self) {
	if (self->stack_top <= self->stack) {
		// raise underflow error, underflow the stack
		panic_error("[VirtualMachine::not_] stack underflow.");
	}
	Value* slot = peek(self, 0);
	switch (slot->type)  
	{
	case VAL_BOOL: slot->as.boolean = !slot->as.boolean; break;
	default:
		//  raise runtime error, expected boolean for not operation, found...
		runtime_error(self, "Expected boolean for not operation, Found...");
		return interpret_runtime_error;
	}
	return interpret_ok;
}

static void concatenate_string(VirtualMachine* self) {
	String* right = macro_as_string(pop(self));
	String* left = macro_as_string(pop(self));
	push(self, macro_obj_from_val(concat_string(&self->strings, left, right)));
}




/*
* Interprets the given chunk of code.
* Returns an InterpretResult indicating the result of the interpretation.
*/
InterpretResult interpret(VirtualMachine* self, const char* source) {
	// printf("Interpreting source code:\n%s\n", source);

	Chunk chunk;
	init_chunk(&chunk);
	self->chunk = &chunk;	// set chunk to current chunk

	if (compile(self, source) == compile_error) {
		free_chunk(&chunk);
		self->chunk = NULL;
		return interpret_compile_error;
	}

	self->ip = chunk.code;	// set instruction pointer to start of code
	InterpretResult result = run(self);
	free_chunk(&chunk);

	return result;
}


/*
* Runs the virtual machine.
* Returns an InterpretResult indicating the result of the interpretation.
* "direct threaded code"、"跳表" 和 "computed goto" 等技术可以优化字节码执行效率。
* 最快解决方案： 对C的非标准拓展 | 手写汇编代码
*/
static InterpretResult run(VirtualMachine* self) {
/* read a byte from the current instruction pointer */
#define macro_read_byte() (*self->ip++)
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
				get_rle_line(&vm->chunk->lines, current_code_index(vm)),							\
				macro_type_name_ptr(l_slot), macro_type_name_ptr(r_slot)							\
			);																						\
			return interpret_runtime_error;															\
		}																							\
    } while(false)

/* read a string from the constant pool */
#define macro_read_string(vm) (macro_as_string(macro_read_constant(vm)))


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
		for (Value* slot = self->stack; slot < self->stack_top; slot++) {
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
		disassemble_instruction(self->chunk, (int)(self->ip - self->chunk->code));
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
			constant = macro_read_constant(self);
			push(self, constant);
			break;
		case op_constant_long:
			constant = macro_read_constant(self);
			push(self, constant);
			break;
		case op_negate: negate(self); break;
		case op_not:	not_(self); break;
		case op_null:	push(self, macro_null_var); break;
		case op_true:	push(self, macro_bool_from_val(true)); break;
		case op_false:	push(self, macro_bool_from_val(false)); break;
		case op_equal: {
			Value right = pop(self);
			Value left = pop(self);
			push(self, macro_bool_from_val(values_equal(left, right)));
			break;
		}
		case op_not_equal: {
			Value right = pop(self);
			Value left = pop(self);
			push(self, macro_bool_from_val(!values_equal(left, right)));
			break;
		}
		case op_greater:		macro_read_binary_op(self, >, true); break;
		case op_greater_equal:	macro_read_binary_op(self, >=, true); break;
		case op_less:			macro_read_binary_op(self, <, true); break;
		case op_less_equal:		macro_read_binary_op(self, <=, true); break;
		case op_add: {
			Value* r_slot = peek(self, 0);
			Value* l_slot = peek(self, 1);
			macro_check_emptyptr(r_slot);
			macro_check_emptyptr(l_slot);
			if (macro_is_string(*l_slot) && macro_is_string(*r_slot)) {
				concatenate_string(self);
			}
			else {
				macro_read_binary_op(self, +, false);
			}
			break;
		}
		case op_subtract:		macro_read_binary_op(self, -, false); break;
		case op_multiply:		macro_read_binary_op(self, *, false); break;
		case op_divide:			macro_read_binary_op(self, /, false); break;
		case op_print: {
			print_value(pop(self));
			printf("\n");
			break;
		}
		case op_pop: pop(self); break;
		case op_define_global: {
			String* identifier = macro_read_string(self);
			hashmap_set(&self->globals, identifier, *peek(self, 0));
			pop(self);
			break;
		}
		case op_set_global: {
			String* identifier = macro_read_string(self);
			Entry* entry = hashmap_get_entry(&self->globals, identifier);
			if (is_empty_entry(entry)) {
				runtime_error(self, "[line %d] where: at runtime undefined global variable '%s'.",
					get_rle_line(&self->chunk->lines, current_code_index(self)),
					identifier->chars
				);
				return interpret_runtime_error;
			}
			else {
				entry->value = *peek(self, 0);
			}
			break;
		}
		case op_get_global: {
			String* identifier = macro_read_string(self);
			Option(Value) value = hashmap_get(&self->globals, identifier);
			if (is_none(value)) {
				runtime_error(self, "[line %d] where: at runtime undefined global variable '%s'.",
					get_rle_line(&self->chunk->lines, current_code_index(self)),
					identifier->chars
				);
				return interpret_runtime_error;
			}
			push(self, unwrap(value));
			break;
		}
		case op_set_local: {
			uint8_t slot = macro_read_byte();
			self->stack[slot] = *peek(self, 0);
			break;
		}
		case op_get_local: {
			uint8_t slot = macro_read_byte();
			push(self, self->stack[slot]);
			break;
		}
		case op_return:
			return interpret_ok;
		}
	}

#undef macro_read_string 
#undef macro_binary_op
#undef macro_read_constant
#undef macro_read_byte
}

/* just-in-time compilation(JIT) */

