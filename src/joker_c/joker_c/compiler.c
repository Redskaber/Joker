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
#include "error.h"
#include "memory.h"
#include "string.h"
#include "compiler.h"
#include "scanner.h"
#include "parser.h"

/*
* TODO: implement
* Then the runtime error will be reported on the wrong line.
* Here, it would show the error on line 2, even though the - is on line 1.
* A more robust approach would be to store the token��s line
* before compiling the operand and then pass that into emitByte(),
*/

static void free_local_variables(LocalVariable local[], int count) {
	for (int i = 0; i < count; i++) {
		free_token(local[i].name);
	}
}

void init_compiler(Compiler* self, FnType type) {
	self->enclosing = NULL;
	self->fn = NULL;
	self->local_count = 0;
	self->scope_depth = 0;
	self->fn_type = type;
	self->fn = new_fn();	// top-level function e.g. main()

	LocalVariable* local = &self->locals[self->local_count++];
	local->depth = 0;
	local->is_captured = false;
	local->name = new_token("TOP", 0, 3, token_identifier); // name is pointer, don't used struct object.
}

void free_compiler(Compiler* self) {
	free_fn(self->fn);
	free_local_variables(self->locals, self->local_count);
	// reset compiler
	self->local_count = 0;
	self->scope_depth = 0;
	self->fn_type = type_script;
	self->fn = NULL;
}

Chunk* current_chunk(Compiler* self) {
	return &self->fn->chunk;
}

void compile_begin_scope(Compiler* self) {
	self->scope_depth++;
}
void compile_end_scope(Compiler* self) {
	self->scope_depth--;
}

Fn* compile(VirtualMachine* vm, const char* source) {
	Scanner scanner;
	init_scanner(&scanner, source);
	scan_tokens(&scanner);

	Parser parser;
	init_parser(&parser, scanner.tokens);

	Compiler top_compiler;
	init_compiler(&top_compiler, type_script);
	vm->compiler = &top_compiler;

	Fn* fn = parse_tokens(&parser, vm);

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
	return parser.had_error ? NULL : fn;
}