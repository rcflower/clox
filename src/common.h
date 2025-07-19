#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>


// #define NAN_BOXING

// #define DEBUG_PRINT_CODE

// #define DEBUG_TRACE_EXECUTION

// #define DEBUG_STRESS_GC
// #define DEBUG_LOG_GC

typedef uint8_t Byte;

#define HASH_T uint32_t
#define UINT8_COUNT (UINT8_MAX + 1)
#define UINT24_MAX	0xffffff
#define UINT24_COUNT 1<<24

#endif