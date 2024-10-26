#pragma once

/*
* This file defines the interface of the garbage collector.
* The garbage collector is responsible for freeing memory that is no longer in use.
*
*/

#ifndef __joker_garbage_collector_h__
#define __joker_garbage_collector_h__

#include "common.h"
#include "object.h"

typedef struct GcNode {
	Object* object;
	struct GcNode* next;
} GcNode;

#endif // __joker_garbage_collector_h__