#pragma once

#ifndef __joker_error_h__
#define __joker_error_h__


void* warning(const char* fmt, ...);
void* error(const char* fmt, ...);
void* panic(const char* fmt, ...);


#endif // __joker_error_h__
