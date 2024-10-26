#include <stdio.h>

#include "object.h"
#include "memory.h"
#include "upvalue.h"

UpvaluePtr new_upvalue(Value* slot) {
	UpvaluePtr upvalue = macro_allocate_object(Upvalue, obj_upvalue);
	upvalue->location = slot;
	upvalue->closed = macro_null_var;
	upvalue->next = NULL;
	return upvalue;
}

void free_upvalue(UpvaluePtr self) {
	if (self->location != NULL) {
		macro_free(Upvalue, self);
	}
}

void print_upvalue(UpvaluePtr self) {
	printf("Upvalue: %p\n", self);
	printf("Location: %p\n", self->location);
}

/*
* vm->open_upv_ptr: upvalue header pointer
* vm --> root                   -----
*	1.prev_upvalue = NULL			|
*   2.curr_upvalue = root         <--
*
* Cmp{ Upvalue } ด๓ --> ะก
*   1.Eq	return
*   2.Insert local
*
* foreach link list find 'local' exist? link
*      ---{108} ||   {74}   || {56}------
*	   V(head)         V(mid)      V(back)
*     [{89}, {87}, {83}, {72}, ..., ]
*/
UpvaluePtr capture_upvalue(UpvaluePtrRef root_ref, Value* local) {
	UpvaluePtr prev_upv = NULL;
	UpvaluePtr curr_upv = *root_ref;

	while (curr_upv != NULL && curr_upv->location > local) {
		prev_upv = curr_upv;
		curr_upv = curr_upv->next;
	}
	/* exist curr_upv && eq */
	if (curr_upv != NULL && curr_upv->location == local) {
		return curr_upv;
	}

	/* foreach over (not in link list) */
	UpvaluePtr creatd_upvalue = new_upvalue(local);
	creatd_upvalue->next = curr_upv;
	if (prev_upv == NULL) {
		*root_ref = creatd_upvalue;			// header
	}
	else {
		prev_upv->next = creatd_upvalue;	// mid || back
	}

	return creatd_upvalue;
}

/*
* root_ref: vm->open_upv_ptr (**Upvalue)
*	Upvalue {
*		Object base;
*		Value* location;    <--- add { closed: Value }, update { location: closed address }
*		struct Upvalue* next;
*	}
*				  V-(2)<-
*              curr_upv | ---------(1)-------->closed
*                 V-(2)->						 |
* {&} root_ref: header -> node -> node -> ...    |
*				  V                              |
*               Value     <--------(1)---------closed
* close prev:
*	Upvalue::location	=> pointer stack slot
* close after:
*	Upvalue::location	=> pointer Upvalue::closed
*	Upvalue::closed		=> stored stack value
*/
void close_upvalues(UpvaluePtrRef root_ref, Value* last) {
	while (
		(*root_ref) != NULL &&
		(*root_ref)->location >= last
		) {
		UpvaluePtr curr_upv = *root_ref;
		curr_upv->closed = *(curr_upv->location);
		curr_upv->location = &(curr_upv->closed);
		*root_ref = curr_upv->next;
	}
}