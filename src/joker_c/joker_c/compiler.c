/*  
* ��open area of research�� and a ��dark art��.
* 
*  compiler:
*	worker 1: scanner user source code			(source code -> tokens)
* 	worker 2: parser source code to bytecode	(tokens -> bytecode)
*	
*  Single-pass compilers: 
*		don��t need much surrounding context to understand a piece of syntax.
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
*  left-associative operators(���������):
* 		a + b + c => (a + b) + c
*		������������������������ȱ�����ߵı��ʽ��Ȼ���ٱ����ұߵı��ʽ��
*       ���Ҳ�����ʹ�ø�һ�������ȼ�����������ȷ����������Ѿ���ջ����
*  
*  right-associative operators(�ҽ�������):
* 		a = b = c => a = (b = c)
*		�����ҽ������������������ȱ����ұߵı��ʽ��Ȼ���ٱ�����ߵı��ʽ��
*       ʹ���뵱ǰ��������ͬ�����ȼ�����������ȷ���Ҳ������Ѿ���ջ����
* 
*  Pratt��������
*		Pratt��������һ���Զ����µĽ���������ʹ����һ�ֳ�Ϊ�����϶��µ�Ԥ�⡱�ļ�����
*		��ͨ���������ʽ���﷨�ṹ��ȷ�������������ȼ������������ȼ������������ϱ��ʽ��
*		Pratt�������Ĺ���ԭ���ǣ�
*			���ȣ���ɨ����ʽ�����룬������һϵ�еı�ǡ�
*			Ȼ����ʹ��һϵ�еĹ�����������ǣ���Щ���������������ϱ�������ɱ��ʽ��
*			����������ֽ��룬���ֽ������ִ�б��ʽ�ļ��㡣
*		Pratt���������ŵ��ǣ�
*			�����Դ�������ı��ʽ�����������ǵ��﷨�ṹ��Ρ�
*			�����Դ�����������Ϻ��ҽ���������
*			�����Դ������������������������ǵ����ȼ���Ρ�
*		Pratt��������ȱ���ǣ�
*			����Ҫ��д�����Ĺ����Ա���������ȷ�ؽ������ʽ��
*			����Ҫά��һ��ջ���Ա���������ȷ����ϱ��ʽ��
*			����Ҫ��������Ա������Ա����﷨���������
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
* A more robust approach would be to store the token��s line 
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

