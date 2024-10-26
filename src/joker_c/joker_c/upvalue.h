#ifndef __joker_upvalue_h__
#define __joker_upvalue_h__

#include <stdint.h>

#include "object.h"

#define upvalue_index_count uint8_count >> 1               // Upvalue count: index range [0, 127] = 128

typedef uint8_t upvalue_info_t;                     // upvalue_info_t: { is_local: 1bit, index: 7bit }

#define macro_as_upvalue_from_obj(object)		((Upvalue*)object)
#define macro_as_upvalue(value)					((Upvalue*)macro_as_obj(value))
#define macro_as_upvalue_from_obj_ptr(obj_ptr)	((Upvalue*)macro_as_obj_ptr(obj_ptr))

#define macro_is_upvalue(value)					is_obj_type(value, obj_upvalue)

/* upvalue node to link list */
typedef struct Upvalue {
	Object base;
	Value* location;			// pointer stack slot
	Value closed;				// closed store upvalue
	struct Upvalue* next;		// pointer next upvalue
} Upvalue, * UpvaluePtr, ** UpvaluePtrs, ** UpvaluePtrRef;

UpvaluePtr new_upvalue(Value* slot);
void free_upvalue(UpvaluePtr self);
void print_upvalue(UpvaluePtr self);
UpvaluePtr capture_upvalue(UpvaluePtrRef root_ref, Value* local);
void close_upvalues(UpvaluePtrRef root_ref, Value* last);

#endif // __joker_upvalue_h__
