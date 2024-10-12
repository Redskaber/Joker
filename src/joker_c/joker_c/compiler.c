/*  
* “open area of research” and a “dark art”.
* 
*  compiler:
*	worker 1: scanner user source code			(source code -> tokens)
* 	worker 2: parser source code to bytecode	(tokens -> bytecode)
*	
*  Single-pass compilers: 
*		don’t need much surrounding context to understand a piece of syntax.
* 
*  Two-pass compilers: 
*		need to understand the entire program before generating bytecode.
* 
*  function_pointer_array: 
*		a common pattern in C to store function pointers in an array.
*		first column: prev_sufix_parse_function_pointer_array
*		second cloumn: middle_parse_function_pointer_array
*		third column: next_sufix_parse_function_pointer_array
*
*  left-associative operators(左结合运算符):
* 		a + b + c => (a + b) + c
*		对于左结合运算符，编译器会先编译左边的表达式，然后再编译右边的表达式。
*       对右操作数使用高一级的优先级，这样可以确保左操作数已经在栈顶。
*  
*  right-associative operators(右结合运算符):
* 		a = b = c => a = (b = c)
*		对于右结合运算符，编译器会先编译右边的表达式，然后再编译左边的表达式。
*       使用与当前操作符相同的优先级，这样可以确保右操作数已经在栈顶。
* 
*  Pratt解析器：
*		Pratt解析器是一种自顶向下的解析器，它使用了一种称为“自上而下的预测”的技术。
*		它通过分析表达式的语法结构来确定操作符的优先级，并根据优先级来决定如何组合表达式。
*		Pratt解析器的工作原理是：
*			首先，它扫描表达式的输入，并生成一系列的标记。
*			然后，它使用一系列的规则来解析标记，这些规则描述了如何组合标记以生成表达式。
*			最后，它生成字节码，该字节码可以执行表达式的计算。
*		Pratt解析器的优点是：
*			它可以处理任意的表达式，而不管它们的语法结构如何。
*			它可以处理任意的左结合和右结合运算符。
*			它可以处理任意的运算符，而不管它们的优先级如何。
*		Pratt解析器的缺点是：
*			它需要编写大量的规则，以便它可以正确地解析表达式。
*			它需要维护一个栈，以便它可以正确地组合表达式。
*			它需要处理错误，以便它可以报告语法和语义错误。
* 
* 
*/

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "parser.h"

#ifdef debug_print_code
#include "debug.h"
#endif


/*
* TODO: implement
* Then the runtime error will be reported on the wrong line. 
* Here, it would show the error on line 2, even though the - is on line 1. 
* A more robust approach would be to store the token’s line 
* before compiling the operand and then pass that into emitByte(),
*/


void init_compiler(Compiler* self) {
	self->local_count = 0;
	self->scope_depth = 0;
}
void free_compiler(Compiler* self) {
}

void compile_begin_scope(Compiler* self) {
	self->scope_depth++;
}
void compile_end_scope(Compiler* self) {
	self->scope_depth--;
}



static void end_compiler(Parser* parser, Chunk* chunk) {
	emit_byte(parser, chunk, op_return);
#ifdef debug_print_code
	if (!parser->had_error) {
		disassemble_chunk(chunk, "=================code================");
	}
#endif
}

CompileResult compile(VirtualMachine* vm, const char* source) {
	Scanner scanner;
	init_scanner(&scanner, source);
	scan_tokens(&scanner);
	
	Parser parser;
	init_parser(&parser, scanner.tokens);

	Compiler compiler;
	init_compiler(&compiler);

	vm->compiler = &compiler;
	parse_tokens(&parser, vm);

	end_compiler(&parser, vm->chunk);

	/*
	* Parser:
	*	At the end of the function, when function memory is freed, the local variable parser is also freed, 
	*	so there is no need to release here because there is no allocation of heap memory.
	*
	*	This free_parser (& parser) is not necessary because the parser is released in the end_compiler function. 
	*	This is just a reset initialization operation on the parser
	* Scanner:
	*   The scanner here performs the operation of releasing heap memory because the scanner owns the tokens list, 
	*	which obtains memory allocation on the heap and needs to be released after it is used up.
	*   So the scanner release operation here is necessary.
	* 
	*   However, other attributes of the scanner itself do not need to be released 
	*   because they are local variables and will be automatically released after the function is executed.
	*/
	free_parser(&parser); 
	free_scanner(&scanner); 

	// TODO: return compile status can be improved(e.g. return detailed error message and more status information)
	return parser.had_error ? compile_error : compile_success;
}

