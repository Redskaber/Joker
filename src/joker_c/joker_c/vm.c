#pragma once

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "common.h"
#include "ops.h"
#include "memory.h"
#include "error.h"
#include "debug.h"
#include "native.h"
#include "fn.h"
#include "closure.h"
#include "value.h"
#include "object.h"
#include "string.h"
#include "compiler.h"
#include "vm.h"

/* Virtual machine operations */
static int current_code_index(CallFrame* frame);
static void define_native(VirtualMachine* self, const char* name, NativeFnPtr function);
static bool call(VirtualMachine* self, Closure* closure, int arg_count);
static bool call_value(VirtualMachine* self, Value* callee, int arg_count);
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

	// heap stack trace
	for (int i = self->frame_count - 1; i >= 0; i--) {
		CallFrame* frame = &self->frames[i];
		Fn* fn = frame->closure->fn;
		int index = current_code_index(frame);
		fprintf(stderr, "[line %d] in ", get_rle_line(&fn->chunk.lines, index));
		is_anonymous_fn(fn) ?
			fprintf(stderr, "script\n") :
			fprintf(stderr, "%s()\n", fn->name->chars);
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
	return convert_check(frame->ip - frame->closure->fn->chunk.code - 1);
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

	/* regiter */
	define_native(self, "clock", native_clock);
	define_native(self, "time", native_time);
	define_native(self, "sleep", native_sleep);
	define_native(self, "date", native_date);
}

void free_virtual_machine(VirtualMachine* self) {
	free_hashmap(&self->strings); // 字符串驻留
	free_hashmap(&self->globals); // 全局变量
}

// Value stack operations
static void reset_stack(VirtualMachine* self) {
	self->stack_top = self->stack;
	self->frame_count = 0;
	self->open_upv_ptr = NULL;
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
		case obj_closure:
			return call(self, macro_as_closure(callee), arg_count);
		case obj_native: {
			NativeFnPtr native_fn = macro_as_native(callee);
			/*
						|<result> |	  <- arg_count -> |
						V		  V 			      V
				[..., native_fn, arg1, arg2,..., argN, ...]
			*/
			Value result = native_fn(self, arg_count, self->stack_top - arg_count);	// get args
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

static bool call(VirtualMachine* self, Closure* closure, int arg_count) {
	if (arg_count != closure->fn->arity) {
		runtime_error(self, "[VirtualMachine::call] Expected %d arguments, Found %d arguments.", closure->fn->arity, arg_count);
		return false;
	}
	if (self->frame_count == frames_statck_max) {
		runtime_error(self, "[VirtualMachine::call] CallFrame Stack overflow.");
		return false;
	}
	CallFrame* frame = &self->frames[self->frame_count++];
	// set up the new call frame: execute func frame.
	frame->closure = closure;
	frame->ip = closure->fn->chunk.code;
	frame->base_pointer = self->stack_top - arg_count - 1;

	return true;
}

/*
* Interprets the given chunk of code.
* Returns an InterpretResult indicating the result of the interpretation.
*/
InterpretResult interpret(VirtualMachine* self, const char* source) {
	Fn* fn = compile(self, source);
	if (fn == NULL) return interpret_compile_error;

	push(self, macro_obj_from_val(fn));
	Closure* closure = new_closure(fn);
	pop(self);
	push(self, macro_obj_from_val(closure));
	call(self, closure, 0);					// call the top-level function closure

	return run(self);
}

static InterpretResult read_binary(VirtualMachine* self, CallFrame* frame, Ops op) {
	InterpretResult result = interpret_ok;

	Value* r_slot = peek(self, 0);
	Value* l_slot = peek(self, 1);
	macro_check_emptyptr(r_slot);
	macro_check_emptyptr(l_slot);

	if (macro_matches_ptr(r_slot, VAL_I32) && macro_matches_ptr(l_slot, VAL_I32)) {
		int r_i32 = macro_as_i32(pop(self));
		int l_i32 = macro_as_i32(pop(self));
		switch (op) {
		case ADD: push(self, macro_i32_from_val(macro_i32_check(l_i32 + r_i32))); break;
		case SUB: push(self, macro_i32_from_val(macro_i32_check(l_i32 - r_i32))); break;
		case MUL: push(self, macro_i32_from_val(macro_i32_check(l_i32 * r_i32))); break;
		case DIV: push(self, macro_i32_from_val(macro_i32_check(l_i32 / r_i32))); break;
		case MOD: push(self, macro_i32_from_val(macro_i32_check(l_i32 % r_i32))); break;
		case EQ:  push(self, macro_bool_from_val(l_i32 == r_i32)); break;
		case NEQ: push(self, macro_bool_from_val(l_i32 != r_i32)); break;
		case LT:  push(self, macro_bool_from_val(l_i32 < r_i32)); break;
		case GT:  push(self, macro_bool_from_val(l_i32 > r_i32)); break;
		case LTE: push(self, macro_bool_from_val(l_i32 <= r_i32)); break;
		case GTE: push(self, macro_bool_from_val(l_i32 >= r_i32)); break;
		case AND: push(self, macro_bool_from_val(l_i32 && r_i32)); break;
		case OR:  push(self, macro_bool_from_val(l_i32 || r_i32)); break;
		case XOR: push(self, macro_bool_from_val(l_i32 ^ r_i32)); break;
		case SHL: push(self, macro_i32_from_val(macro_i32_check(l_i32 << r_i32))); break;
		case SHR: push(self, macro_i32_from_val(macro_i32_check(l_i32 >> r_i32))); break;
		case BIT_AND: push(self, macro_i32_from_val(macro_i32_check((l_i32 & r_i32)))); break;
		case BIT_OR:  push(self, macro_i32_from_val(macro_i32_check((l_i32 | r_i32)))); break;
		case BIT_XOR: push(self, macro_i32_from_val(macro_i32_check((l_i32 ^ r_i32)))); break;
		case BIT_NOT: push(self, macro_i32_from_val(~l_i32)); break;
		default:
			//  raise runtime error, unexpected binary operation
			runtime_error(self, "[VirtualMachine::read_binary]\n"
				"[line %d] where at runtime, '%s'.\n"
				"\tExpected i32 for left and right operands, Found left: %s, right: %s.\n",
				get_rle_line(&frame->closure->fn->chunk.lines, current_code_index(frame)),
				macro_ops_to_string(op),
				macro_type_name_ptr(l_slot),
				macro_type_name_ptr(r_slot)
			);
			result = interpret_runtime_error;
			break;
		}
	}
	else if (macro_matches_ptr(r_slot, VAL_I64) && macro_matches_ptr(l_slot, VAL_I64)) {
		int64_t r_i64 = macro_as_i64(pop(self));
		int64_t l_i64 = macro_as_i64(pop(self));
		switch (op) {
		case ADD: push(self, macro_i64_from_val(macro_i64_check(l_i64 + r_i64))); break;
		case SUB: push(self, macro_i64_from_val(macro_i64_check(l_i64 - r_i64))); break;
		case MUL: push(self, macro_i64_from_val(macro_i64_check(l_i64 * r_i64))); break;
		case DIV: push(self, macro_i64_from_val(macro_i64_check(l_i64 / r_i64))); break;
		case MOD: push(self, macro_i64_from_val(macro_i64_check(l_i64 % r_i64))); break;
		case EQ:  push(self, macro_bool_from_val(l_i64 == r_i64)); break;
		case NEQ: push(self, macro_bool_from_val(l_i64 != r_i64)); break;
		case LT:  push(self, macro_bool_from_val(l_i64 < r_i64)); break;
		case GT:  push(self, macro_bool_from_val(l_i64 > r_i64)); break;
		case LTE: push(self, macro_bool_from_val(l_i64 <= r_i64)); break;
		case GTE: push(self, macro_bool_from_val(l_i64 >= r_i64)); break;
		case AND: push(self, macro_bool_from_val(l_i64 && r_i64)); break;
		case OR:  push(self, macro_bool_from_val(l_i64 || r_i64)); break;
		case XOR: push(self, macro_bool_from_val(l_i64 ^ r_i64)); break;
		case SHL: push(self, macro_i64_from_val(macro_i64_check(l_i64 << r_i64))); break;
		case SHR: push(self, macro_i64_from_val(macro_i64_check(l_i64 >> r_i64))); break;
		case BIT_AND: push(self, macro_i64_from_val(macro_i64_check((l_i64 & r_i64)))); break;
		case BIT_OR:  push(self, macro_i64_from_val(macro_i64_check((l_i64 | r_i64)))); break;
		case BIT_XOR: push(self, macro_i64_from_val(macro_i64_check((l_i64 ^ r_i64)))); break;
		case BIT_NOT: push(self, macro_i64_from_val(~l_i64)); break;
		default:
			//  raise runtime error, unexpected binary operation
			runtime_error(self, "[VirtualMachine::read_binary]\n"
				"[line %d] where at runtime, '%s'.\n"
				"\tExpected i64 for left and right operands, Found left: %s, right: %s.\n",
				get_rle_line(&frame->closure->fn->chunk.lines, current_code_index(frame)),
				macro_ops_to_string(op),
				macro_type_name_ptr(l_slot),
				macro_type_name_ptr(r_slot)
			);
			result = interpret_runtime_error;
			break;
		}
	}
	else if (macro_matches_ptr(r_slot, VAL_F64) && macro_matches_ptr(l_slot, VAL_F64)) {
		double r_f64 = macro_as_f64(pop(self));
		double l_f64 = macro_as_f64(pop(self));
		switch (op) {
		case ADD: push(self, macro_f64_from_val(l_f64 + r_f64)); break;
		case SUB: push(self, macro_f64_from_val(l_f64 - r_f64)); break;
		case MUL: push(self, macro_f64_from_val(l_f64 * r_f64)); break;
		case DIV: push(self, macro_f64_from_val(l_f64 / r_f64)); break;
		case EQ:  push(self, macro_bool_from_val(l_f64 == r_f64)); break;
		case NEQ: push(self, macro_bool_from_val(l_f64 != r_f64)); break;
		case LT:  push(self, macro_bool_from_val(l_f64 < r_f64)); break;
		case GT:  push(self, macro_bool_from_val(l_f64 > r_f64)); break;
		case LTE: push(self, macro_bool_from_val(l_f64 <= r_f64)); break;
		case GTE: push(self, macro_bool_from_val(l_f64 >= r_f64)); break;
		default:
			//  raise runtime error, unexpected binary operation
			runtime_error(self, "[VirtualMachine::read_binary]\n"
				"[line %d] where at runtime, '%s'.\n"
				"\tExpected f64 for left and right operands, Found left: %s, right: %s.\n",
				get_rle_line(&frame->closure->fn->chunk.lines, current_code_index(frame)),
				macro_ops_to_string(op),
				macro_type_name_ptr(l_slot),
				macro_type_name_ptr(r_slot)
			);
			result = interpret_runtime_error;
			break;
		}
	}
	else if (macro_matches_ptr(r_slot, VAL_BOOL) && macro_matches_ptr(l_slot, VAL_BOOL)) {
		bool r_bool = macro_as_bool(pop(self));
		bool l_bool = macro_as_bool(pop(self));
		switch (op) {
		case AND: push(self, macro_bool_from_val(l_bool && r_bool)); break;
		case OR:  push(self, macro_bool_from_val(l_bool || r_bool)); break;
		case XOR: push(self, macro_bool_from_val(l_bool ^ r_bool)); break;
		default:
			//  raise runtime error, unexpected binary operation
			runtime_error(self, "[VirtualMachine::read_binary]\n"
				"[line %d] where at runtime, '%s'.\n"
				"\tExpected bool for left and right operands, Found left: %s, right: %s.\n",
				get_rle_line(&frame->closure->fn->chunk.lines, current_code_index(frame)),
				macro_ops_to_string(op),
				macro_type_name_ptr(l_slot),
				macro_type_name_ptr(r_slot)
			);
			result = interpret_runtime_error;
			break;
		}
	}
	else {
		//  raise runtime error, unexpected binary operation
		runtime_error(self, "[VirtualMachine::read_binary]\n"
			"[line %d] where at runtime, '%s'.\n"
			"Expected number[(i32,i32), (i64,i64), (f64,f64), (bool,bool)] for left and right operands, "
			"Found left: %s, right: %s.\n",
			get_rle_line(&frame->closure->fn->chunk.lines, current_code_index(frame)),
			macro_ops_to_string(op),
			macro_type_name_ptr(l_slot),
			macro_type_name_ptr(r_slot)
		);
		result = interpret_runtime_error;
	}

	return result;
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
#define macro_read_constant() (frame->closure->fn->chunk.constants.values[macro_read_byte()])
/* read a short from the current instruction pointer */
#define macro_runtime_error_raised(result)			\
    do {											\
        if (result == interpret_runtime_error) {	\
            return result;							\
        }											\
    } while (false)
/* read a string from the constant pool */
#define macro_read_string() (macro_as_string(macro_read_constant()))
/* read a short from the current instruction pointer */
#define macro_read_short()											\
	(frame->ip +=2,													\
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
		disassemble_instruction(&frame->closure->fn->chunk, (int)(frame->ip - frame->closure->fn->chunk.code));
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
		case op_greater:		macro_runtime_error_raised(read_binary(self, frame, GT)); break;
		case op_greater_equal:	macro_runtime_error_raised(read_binary(self, frame, GTE)); break;
		case op_less:			macro_runtime_error_raised(read_binary(self, frame, LT)); break;
		case op_less_equal:		macro_runtime_error_raised(read_binary(self, frame, LTE)); break;
		case op_add: {
			Value* r_slot = peek(self, 0);
			Value* l_slot = peek(self, 1);
			macro_check_emptyptr(r_slot);
			macro_check_emptyptr(l_slot);
			if (macro_is_string(*l_slot) && macro_is_string(*r_slot)) {
				concatenate_string(self);
			}
			else { macro_runtime_error_raised(read_binary(self, frame, ADD)); } break;
		}
		case op_subtract:		macro_runtime_error_raised(read_binary(self, frame, SUB)); break;
		case op_multiply:		macro_runtime_error_raised(read_binary(self, frame, MUL)); break;
		case op_divide:			macro_runtime_error_raised(read_binary(self, frame, DIV)); break;
		case op_print: {
			print_value(pop(self)); printf("\n"); break;
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
					get_rle_line(&frame->closure->fn->chunk.lines, current_code_index(frame)),
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
					get_rle_line(&frame->closure->fn->chunk.lines, current_code_index(frame)),
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
		case op_get_upvalue: {
			uint8_t slot = macro_read_byte();
			// why need -1?:
			// because the first slot is top-level closure,in constant pool,
			// so the upvalue index need remove top-level closure index.
			push(self, *(frame->closure->upvalue_ptrs[slot - 1]->location));
			break;
		}
		case op_set_upvalue: {
			uint8_t slot = macro_read_byte();
			*frame->closure->upvalue_ptrs[slot - 1]->location = *peek(self, 0);
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
		case op_break: {
			// break context loop to outer context loop or fn context.
			// uint8_t break_offset = macro_read_byte();
			// frame->ip += break_offset;
			break;
		}
		case op_closure: {
			Fn* fn = macro_as_fn(macro_read_constant());
			Closure* closure = new_closure(fn);
			push(self, macro_obj_from_val(closure));

			for (int i = 0; i < closure->upvalue_count; i++) {
				upvalue_info_t upv_info = macro_read_byte();
				bool is_local = upv_info >> 7;
				uint8_t index = upv_info & 0b0111'1111;
				if (is_local) {
					closure->upvalue_ptrs[i] = capture_upvalue(&self->open_upv_ptr, frame->base_pointer + index);
				}
				else {
					closure->upvalue_ptrs[i] = frame->closure->upvalue_ptrs[index];
				}
			}
			break;
		}
		case op_close_upvalue:
			close_upvalues(&self->open_upv_ptr, self->stack_top - 1);
			pop(self);
			break;
		case op_return: {
			Value result = pop(self);	// pop the return value
			close_upvalues(&self->open_upv_ptr, frame->base_pointer);
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