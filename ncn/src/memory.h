#pragma once
#include <stddef.h>
#include <stdint.h>

/// Slice of void * array
typedef struct VoidSlice {
	void * mem;
	size_t len;
	size_t cap;
} VoidSlice;

/// Safely free memory
#define FREE(POINTER) \
	if ((POINTER) != NULL) { \
		free((void *)POINTER); \
		POINTER = NULL; \
	}

#define xfree(POINTER) FREE(POINTER)

/// Allocate memory and zero initialize it
void * xcalloc(size_t n, size_t size);
