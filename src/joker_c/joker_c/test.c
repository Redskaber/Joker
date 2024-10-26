#include <stdio.h>

#include "vm.h"
#include "chunk.h"
#include "debug.h"
#include "test.h"

/*
* test_op_return():
*	@test_object: op_return
*	@test_precautions: op_return instruction can pop one value from Value Stack and return it.
*/
void test_op_return() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);

	/* add some constants to the chunk, return their offsets */
	/* so: constant_instruction => (uint8_t)op_code + (uint8_t)value_offset */
	write_constant(&chunk, macro_i32_from_val(1000), 1);

	/* op_return will add to code array, and add line to line array.
	* after op_return: from Value Stack pop one value, and return it.
	*
	* so: simple_instruction => (uint8_t)op_code
	*/
	write_chunk(&chunk, op_return, 1);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_op_constant():
*	@test_object: op_constant
*	@test_precautions: op_constant instruction can push one value to Value Stack.
*						But it's not stop vm run, need op_return auxiliary.
*/
void test_op_constant() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);

	/*  so: constant_instruction => (uint8_t)op_code + (uint8_t)value_offset */
	write_constant(&chunk, macro_i32_from_val(1000), 2);

	/* so: simple_instruction => (uint8_t)op_code */
	write_chunk(&chunk, op_return, 2);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_op_constant_long():
*	@test_object: op_constant_long
*	@test_precautions: op_constant_long instruction can push one value to Value Stack.
*						But it's not stop vm run, need op_return auxiliary.
*/
void test_op_constant_long() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);

	/*  so: constant_long_instruction => (uint8_t)op_code + (uint16_t)value_offset */
	// op_constant range: 0x00 ~ 0xff(255)
	// op_constant_long range: 0x0100 ~ 0xffff(65535)

	// 0x0000 ~ 0x00ff(255): op_constant
	for (int i = 0; i < 256; i++) {
		write_constant(&chunk, macro_i32_from_val(1000), 3);
	}
	// 0x0100 ~ 0xffff(65535): op_constant_long
	for (int i = 0; i < 5; i++) {
		write_constant(&chunk, macro_i32_from_val(1000), 3);
	}

	write_chunk(&chunk, op_return, 3);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_op_negate():
*	@test_object: op_negate
*	@test_precautions: op_negate instruction can pop one value from Value Stack, and push its negative value to Value Stack.
* 						But it's not stop vm run, need op_return auxiliary.
*/
void test_op_negate() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);
	/*  so: constant_long_instruction => (uint8_t)op_code + (uint16_t)value_offset */
	write_constant(&chunk, macro_i32_from_val(1000), 4);

	write_chunk(&chunk, op_negate, 4);				// write: operation code
	write_chunk(&chunk, op_return, 4);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_op_add():
*	@test_object: op_add
*	@test_precautions: op_add instruction can pop two values from Value Stack, and push their sum to Value Stack.
* 						But it's not stop vm run, need op_return auxiliary.
*/
void test_op_add() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);
	write_constant(&chunk, macro_i32_from_val(1000), 5);				// [1000]		left operand
	write_constant(&chunk, macro_i32_from_val(2000), 5);				// [1000][2000] right operand

	write_chunk(&chunk, op_add, 5);					// write: operation code
	write_chunk(&chunk, op_return, 5);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_op_subtract():
*	@test_object: op_subtract
*	@test_precautions: op_subtract instruction can pop two values from Value Stack, and push their difference to Value Stack.
* 						But it's not stop vm run, need op_return auxiliary.
*/
void test_op_subtract() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);
	write_constant(&chunk, macro_i32_from_val(1000), 6);				// [1000]		left operand
	write_constant(&chunk, macro_i32_from_val(2000), 6);				// [1000][2000] right operand

	write_chunk(&chunk, op_subtract, 6);				// write: operation code
	write_chunk(&chunk, op_return, 6);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_op_multiply():
*	@test_object: op_multiply
*	@test_precautions: op_multiply instruction can pop two values from Value Stack, and push their product to Value Stack.
* 						But it's not stop vm run, need op_return auxiliary.
*/
void test_op_multiply() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);

	write_constant(&chunk, macro_i32_from_val(1000), 7);				// [1000]		left operand
	write_constant(&chunk, macro_i32_from_val(2000), 7);				// [1000][2000] right operand

	write_chunk(&chunk, op_multiply, 7);				// write: operation code
	write_chunk(&chunk, op_return, 7);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, & chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_op_divide():
*	@test_object: op_divide
*	@test_precautions: op_divide instruction can pop two values from Value Stack, and push their quotient to Value Stack.
* 						But it's not stop vm run, need op_return auxiliary.
*/
void test_op_divide() {
	// create a virtual machine instance
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);

	write_constant(&chunk, macro_i32_from_val(1000), 8);				// [1000]		left operand
	write_constant(&chunk, macro_i32_from_val(2000), 8);				// [1000][2000] right operand

	write_chunk(&chunk, op_divide, 8);				// write: operation code
	write_chunk(&chunk, op_return, 8);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_expression_1():
*	@test_object: expression 1 * 2 + 3  = 5
*	@test_precautions: used Value Stack model to simulate expression tree.
*						The default for_each_order is preorder traversal.
*/
void test_expression_1() {
	// create a virtual machine instance}
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);

	/* expression: 1 * 2 + 3  = 5
	* so:
	*	default for_each_order:
	*		preorder traversal of expression tree
	*		(left, root, right) => order(left -> right -> root)
	*
	*				+
	*			*		3
	*		1		2
	*
	*  preorder traversal: 1, 2, *, 3, +
	*/
	write_constant(&chunk, macro_i32_from_val(1), 1001);				// Value Array: [1]
	write_constant(&chunk, macro_i32_from_val(2), 1001);				// Value Array: [1][2]
	write_constant(&chunk, macro_i32_from_val(3), 1001);				// Value Array: [1][2][3]

	write_chunk(&chunk, op_add, 1001);
	write_chunk(&chunk, op_return, 1001);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_expression_2():
*	@test_object: expression 1 + 2 * 3  = 7
*	@test_precautions: used Value Stack model to simulate expression tree.
*						The default for_each_order is preorder traversal.
*/
void test_expression_2() {
	// create a virtual machine instance}
	VirtualMachine vm;
	init_virtual_machine(&vm);

	Chunk chunk;
	init_chunk(&chunk);

	/* expression: 1 + 2 * 3  = 7
	* so:
	*	default for_each_order:
	*		preorder traversal of expression tree.
	*		(left, root, right) => order(left -> right -> root)
	*
	*				+
	*		1			*
	*				2		3
	*
	*  preorder traversal: 1, 2, 3, *, +
	*/
	write_constant(&chunk, macro_i32_from_val(1), 1002);				// Value Array: [1]
	write_constant(&chunk, macro_i32_from_val(2), 1002);				// Value Array: [1][2]
	write_constant(&chunk, macro_i32_from_val(3), 1002);				// Value Array: [1][2][3]

	write_chunk(&chunk, op_multiply, 1002);
	write_chunk(&chunk, op_add, 1002);
	write_chunk(&chunk, op_return, 1002);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_expression_3():
* 	@test_object: expression 3 - 2 - 1  = 0
*	@test_precautions: used Value Stack model to simulate expression tree.
*						The default for_each_order is preorder traversal.
*/
void test_expression_3() {
	// create a virtual machine instance}
	VirtualMachine vm;
	init_virtual_machine(&vm);
	Chunk chunk;
	init_chunk(&chunk);
	/* expression: 3 - 2 - 1  = 0
	* so:
	*	default for_each_order:
	*		preorder traversal of expression tree.
	*		(left, root, right) => order(left -> right -> root)
	*
	*				-
	*			-		1
	*		3		2
	*
	*  preorder traversal: 3, 2, -, 1, -
	*/
	write_constant(&chunk, macro_i32_from_val(1), 1003);				// Value Array: [1]
	write_constant(&chunk, macro_i32_from_val(2), 1003);				// Value Array: [1][2]
	write_constant(&chunk, macro_i32_from_val(3), 1003);				// Value Array: [1][2][3]

	write_chunk(&chunk, op_subtract, 1003);
	write_chunk(&chunk, op_return, 1003);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}

/* test_expression_4():
* 	@test_object: expression 1 + 2 * 3 - 4 / -5
* 	@test_precautions: used Value Stack model to simulate expression tree.
*						The default for_each_order is preorder traversal.
*/
void test_expression_4() {
	// create a virtual machine instance}
	VirtualMachine vm;
	init_virtual_machine(&vm);
	Chunk chunk;
	init_chunk(&chunk);

	/* expression: 1 + 2 * 3 - 4 / -5 = 7.8
	* so:
	*	default for_each_order:
	*		preorder traversal of expression tree.
	*		(left, root, right) => order(left -> right -> root)
	*
	* 					-
	*			+				/
	*		1		*		4		-5
	*			2	    3
	*
	*
	*  preorder traversal: 1, 2, 3, *, +, 4, -5, /, -
	*/
	write_constant(&chunk, macro_i32_from_val(1), 1003);				// Stack: [1]
	write_constant(&chunk, macro_i32_from_val(2), 1003);				// Stack: [1][2]
	write_constant(&chunk, macro_i32_from_val(3), 1003);				// Stack: [1][2][3]

	write_chunk(&chunk, op_multiply, 1004);			// Stack: [1][6]
	write_chunk(&chunk, op_add, 1004);				// Stack: [7]

	write_constant(&chunk, macro_i32_from_val(4), 1003);				// Stack: [7][4]
	write_constant(&chunk, macro_i32_from_val(5), 1003);				// Stack: [7][4][5]

	write_chunk(&chunk, op_negate, 1004);			// Stack: [7][4][-5]
	write_chunk(&chunk, op_divide, 1004);			// Stack: [7][-0.8]
	write_chunk(&chunk, op_subtract, 1004);			// Stack: [7.8]
	write_chunk(&chunk, op_return, 1004);

	disassemble_chunk(&chunk, "Bytecode");
	// interpret(&vm, &chunk);

	free_chunk(&chunk);
	free_virtual_machine(&vm);
}