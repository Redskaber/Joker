#pragma once

#include <time.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"
#include "compiler.h"
#include "error.h"
#include "value.h"
#include "string.h"
#include "function.h"
#include "memory.h"
#include "object.h"
#include "debug.h"
#include "vm.h"


/* Virtual machine operations */
static int current_code_index(CallFrame* frame);
static void define_native(VirtualMachine * self, const char* name, NativeFnPtr function);
static bool call(VirtualMachine* self, Function* func, int arg_count);
static bool call_value(VirtualMachine* self, Value* callee, int arg_count);
static void reset_stack(VirtualMachine* self);
static InterpretResult run(VirtualMachine* self);


/************************ Native Function ************************/
static Value clock_nativer(Value* args, int arg_count) {
	return macro_f64_from_val((double)clock() / CLOCKS_PER_SEC);
}
/******************************************************************/


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

	// heap stack trace
	for (int i = self->frame_count - 1; i >= 0; i--) {
		CallFrame* frame = &self->frames[i];
		Function* func = frame->func;
		int index = current_code_index(frame);
		fprintf(stderr, "[line %d] in ", get_rle_line(&func->chunk.lines, index));
		is_anonymous_fn(func) ? 
			fprintf(stderr, "script\n") : 
			fprintf(stderr, "%s()\n", func->name->chars);
	}

	// reset stack
	reset_stack(self);
}


/*
* vm->ip: 指向下一条需要执行的指令的指针
* vm->chunk->code: 指向代码段的起始地址
* vm->ip - vm->chunk->code - 1: 指向当前指令的偏移量
*/
static int current_code_index(CallFrame* frame) {
	return convert_check(frame->ip - frame->func->chunk.code - 1);
}


static bool is_falsey(VirtualMachine* self, Value* value) {
	switch (value->type) {
	case VAL_BOOL: return !value->as.boolean;
	default: 
		//  raise runtime error, expected boolean for not operation, found...
		runtime_error(self, "Expected boolean for not operation, Found...");
		return false;
	}
}


void init_virtual_machine(VirtualMachine* self) {
	self->compiler = NULL;
	reset_stack(self);
	init_hashmap(&self->strings); // 字符串驻留
	init_hashmap(&self->globals); // 全局变量

	/* native */
	define_native(self, "clock", clock_nativer);
}

void free_virtual_machine(VirtualMachine* self) {
	free_hashmap(&self->strings); // 字符串驻留
	free_hashmap(&self->globals); // 全局变量
}

// Value stack operations
static void reset_stack(VirtualMachine* self) {
	self->stack_top = self->stack;
	self->frame_count = 0;
}

Compiler* replace_compiler(VirtualMachine* self, Compiler* compiler) {
	Compiler* old_compiler = self->compiler;
	self->compiler = compiler;
	return old_compiler;
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
* TODO: push && pop ?  => gc?
* used stack push and pop, bucause gc need to know the stack pointer change
*							  |  ---(-2)----> | 
* value					      V				  V
* stack [..., name, function,...]  => [..., name, function,...]  
*/
static void define_native(VirtualMachine* self, const char* name, NativeFnPtr function) {
	push(self, macro_obj_from_val(new_string(&self->strings, name, (int)strlen(name))));
	push(self, macro_obj_from_val(new_native(function)));
	hashmap_set(&self->globals, macro_as_string(self->stack[0]), self->stack[1]);
	// self->stack_top -= 2;		// remove the name and function from the stack
	pop(self);
	pop(self);
}

static bool call_value(VirtualMachine* self, Value* callee, int arg_count) {
	if (macro_is_obj_ptr(callee)) {
		switch (macro_obj_ptr_type(callee))
		{
		case obj_function: 
			return call(self, macro_as_function_from_obj_ptr(callee), arg_count);
		case obj_native: {
			NativeFnPtr native_fn = macro_as_native(callee);
			/*
						|<result> |	  <- arg_count -> |
						V		  V 			      V
				[..., native_fn, arg1, arg2,..., argN, ...]
			*/
			Value result = native_fn(self->stack_top - arg_count, arg_count);	// get args
			self->stack_top -= arg_count + 1;	// pop args + function
			push(self, result);
			return true;
		}
		default:
			break;
		}
	}
	runtime_error(self, "[VirtualMachine::call_value] Can only call functions and classes.");
	return false;
}

static bool call(VirtualMachine* self, Function* func, int arg_count) {
	if (arg_count != func->arity) {
		runtime_error(self, "[VirtualMachine::call] Expected %d arguments, Found %d arguments.", func->arity, arg_count);
		return false;
	}
	if (self->frame_count == frames_statck_max) {
		runtime_error(self, "[VirtualMachine::call] CallFrame Stack overflow.");
		return false;
	}
	CallFrame* frame = &self->frames[self->frame_count++];
	// set up the new call frame: execute func frame.
	frame->func = func;
	frame->ip = func->chunk.code;
	frame->base_pointer = self->stack_top - arg_count - 1;

	return true;
}


/*
* Interprets the given chunk of code.
* Returns an InterpretResult indicating the result of the interpretation.
*/
InterpretResult interpret(VirtualMachine* self, const char* source) {
	Function* func = compile(self, source);
	if (func == NULL) return interpret_compile_error;

	push(self, macro_obj_from_val(func));
	call(self, func, 0);	// call the top-level function: e.g main()

	return run(self);
}


/*
* Runs the virtual machine.
* Returns an InterpretResult indicating the result of the interpretation.
* "direct threaded code"、"跳表" 和 "computed goto" 等技术可以优化字节码执行效率。
* 最快解决方案： 对C的非标准拓展 | 手写汇编代码
*/
static InterpretResult run(VirtualMachine* self) {
	// frame: call stack frame
	CallFrame* frame = &self->frames[self->frame_count - 1];

/* read a byte from the current instruction pointer: vm->ip++ goto frame->ip++ */
#define macro_read_byte() (*frame->ip++)
/* read a constant from the constant pool */
#define macro_read_constant() (frame->func->chunk.constants.values[macro_read_byte()])
/* read a binary operation from the current instruction pointer : TODO: 将binary等操作符的实现分离出来，单独类型判断*/
#define macro_read_binary_op(op, is_cmp)															\
    do {																							\
        Value* r_slot = peek(self, 0);																\
        Value* l_slot = peek(self, 1);																\
        macro_check_emptyptr(r_slot);																\
        macro_check_emptyptr(l_slot);																\
        if (macro_matches_ptr(r_slot, VAL_I32) && macro_matches_ptr(l_slot, VAL_I32)) {				\
		    int r_i32 = macro_as_i32(pop(self));													\
		    int l_i32 = macro_as_i32(pop(self));													\
		    is_cmp ? push(self, macro_bool_from_val(l_i32 op r_i32)) :								\
				push(self, macro_i32_from_val(l_i32 op r_i32));										\
		} else if (macro_matches_ptr(r_slot, VAL_F64) && macro_matches_ptr(l_slot, VAL_F64)) {		\
			double r_f64 = macro_as_f64(pop(self));													\
			double l_f64 = macro_as_f64(pop(self));													\
			is_cmp ? push(self, macro_bool_from_val(l_f64 op r_f64)) :								\
				push(self, macro_f64_from_val(l_f64 op r_f64));										\
		} else {																					\
			runtime_error(self, "[line %d] where: at runtime binary '"#op "' operation.\n"			\
				"\tExpected number[(i32,i32), (f64,f64)] for left and right operands, "				\
					"Found left: %s, right: %s",													\
				get_rle_line(&frame->func->chunk.lines, current_code_index(&self->frames[self->frame_count - 1])), \
				macro_type_name_ptr(l_slot), macro_type_name_ptr(r_slot)							\
			);																						\
			return interpret_runtime_error;															\
		}																							\
    } while(false)

/* read a string from the constant pool */
#define macro_read_string() (macro_as_string(macro_read_constant()))
/* read a short from the current instruction pointer */
#define macro_read_short()																			\
	(frame->ip +=2,																					\
	((uint16_t)(frame->ip[-2]) << 8) | (uint16_t)(frame->ip[-1]))


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
		disassemble_instruction(&frame->func->chunk, (int)(frame->ip - frame->func->chunk.code));
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
			constant = macro_read_constant();
			push(self, constant);
			break;
		case op_constant_long:
			constant = macro_read_constant();
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
		case op_greater:		macro_read_binary_op(>, true); break;
		case op_greater_equal:	macro_read_binary_op(>=, true); break;
		case op_less:			macro_read_binary_op(<, true); break;
		case op_less_equal:		macro_read_binary_op(<=, true); break;
		case op_add: {
			Value* r_slot = peek(self, 0);
			Value* l_slot = peek(self, 1);
			macro_check_emptyptr(r_slot);
			macro_check_emptyptr(l_slot);
			if (macro_is_string(*l_slot) && macro_is_string(*r_slot)) {
				concatenate_string(self);
			}
			else {
				macro_read_binary_op(+, false);
			}
			break;
		}
		case op_subtract:		macro_read_binary_op(-, false); break;
		case op_multiply:		macro_read_binary_op(*, false); break;
		case op_divide:			macro_read_binary_op(/, false); break;
		case op_print: {
			print_value(pop(self));
			printf("\n");
			break;
		}
		case op_pop: pop(self); break;
		case op_define_global: {
			String* identifier = macro_read_string();
			hashmap_set(&self->globals, identifier, *peek(self, 0));
			pop(self);
			break;
		}
		case op_set_global: {
			String* identifier = macro_read_string();
			Entry* entry = hashmap_get_entry(&self->globals, identifier);
			if (is_empty_entry(entry)) {
				runtime_error(self, "[line %d] where: at runtime undefined global variable '%s'.",
					get_rle_line(&frame->func->chunk.lines, current_code_index(frame)),
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
			String* identifier = macro_read_string();
			Option(Value) value = hashmap_get(&self->globals, identifier);
			if (is_none(value)) {
				runtime_error(self, "[line %d] where: at runtime undefined global variable '%s'.",
					get_rle_line(&frame->func->chunk.lines, current_code_index(frame)),
					identifier->chars
				);
				return interpret_runtime_error;
			}
			push(self, unwrap(value));
			break;
		}
		case op_set_local: {
			uint8_t slot = macro_read_byte();
			frame->base_pointer[slot] = *peek(self, 0);
			break;
		}
		case op_get_local: {
			uint8_t slot = macro_read_byte();
			push(self, frame->base_pointer[slot]);
			break;
		}
		case op_jump_if_false: {
			uint16_t offset = macro_read_short();			// short: 16-bit unsigned signed integer
			if (is_falsey(self, peek(self, 0))) {			// e.g.: self->ip += falsey(peek(self, 0)) * offset;
				frame->ip += offset;
			}
			break;
		}
		case op_jump: {
			uint16_t offset = macro_read_short();
			frame->ip += offset;
			break;
		}
		case op_loop: {
			uint16_t offset = macro_read_short();
			frame->ip -= offset;
			break;
		}
		case op_call: {
			int arg_count = macro_read_byte();
			// call success value can in frames insert new frame(function call).
			if (!call_value(self, peek(self, arg_count), arg_count)) {
				return interpret_runtime_error;
			}
			// update current frame pointer point to the new frame(function call);
			// ps: set base pointer
			frame = &self->frames[self->frame_count - 1];
			break;
		}
		case op_return: {
			Value result = pop(self);	// pop the return value
			self->frame_count--;		// jump to the caller frame
			if (self->frame_count == 0) {	// if the caller frame is the top-level frame, return the result.
				return interpret_ok;
			}

			// update vm stack top pointer point to the caller frame stack top pointer.
			// this don't need set frame ip pointer, 
			// because caller frame execute frame->ip over 
			// can return to caller frame continue execute caller->frame->ip ++, next.
			self->stack_top = frame->base_pointer;
			// push the return value to the caller frame stack.
			push(self, result);
			// update frame pointer point to the caller frame.
			frame = &self->frames[self->frame_count - 1];
			break;
		}
		}
	}

#undef macro_read_short
#undef macro_is_false
#undef macro_read_string 
#undef macro_binary_op
#undef macro_read_constant
#undef macro_read_byte
}

/* just-in-time compilation(JIT) */

