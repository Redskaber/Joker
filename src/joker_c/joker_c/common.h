#pragma once

#ifndef JOKER_COMMON_H
#define JOKER_COMMON_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
 

typedef int index_t;
typedef int line_t;


#define debug_print_code
#define debug_trace_execution
/* optional struct Option {Some, None} */

#define argument_count_max UINT8_MAX
/* define local variable limit count store max*/
#define uint8_count (UINT8_MAX + 1)         // static const int uint8_count = UINT8_MAX + 1;

/* define exit code for joker */
typedef enum
{
        enum_success = 0,
        enum_file_error = 74,
        enum_invalid_arguments = 64,
        enum_compiler_error = 65,
        enum_scanner_error = 66,
        enum_runtime_error = 70,
        enum_unknown_error = 1
} JokerExitCode;

/* Display error code to user message  */
#define macro_code_to_string(exit_code)                         \
        ( exit_code == enum_success ? "success"                 \
        : exit_code == enum_file_error   ? "file_error"         \
        : exit_code == enum_invalid_arguments   ? "invalid_arguments"     \
        : exit_code == enum_compiler_error ? "compiler_error"   \
        : exit_code == enum_scanner_error  ? "scanner_error"    \
        : exit_code == enum_runtime_error  ? "runtime_error"    \
        : "unknown error")

#endif /* joker_common_h */
