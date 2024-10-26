#pragma once

#ifndef joker_debug_h
#define joker_debug_h

#include "chunk.h"

void disassemble_chunk(Chunk* chunk, const char* name);
int disassemble_instruction(Chunk* chunk, int offset);

#endif /* joker_debug_h */
