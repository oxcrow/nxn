#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
//
#include "error.h"

/// Slice of string
typedef struct StringSlice {
	char * str;
	size_t len;
	size_t cap;
} StringSlice;

/// String of maximum 16 characters length
/// Used when we want to pool allocate many strings at once.
typedef struct String16 {
	char str[16];
	uint8_t len;
} String16;

/// Length of NULL terminated string
size_t nstrlen(const char * str, size_t len_max);

/// Character at index i
OptionChar charAt(const StringSlice * str, size_t i);

StringSlice stringSlice_Init(char * str, size_t len_str);
