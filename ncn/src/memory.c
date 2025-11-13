#include "memory.h"
//
#include <stddef.h>
//
#include "error.h"

/// Allocate memory and zero initialize it
void * xcalloc(size_t num_elems, size_t elem_size) {
	ASSERT(IS_FINITE_MUL(num_elems, elem_size, SIZE_MAX));

	void * mem = calloc(num_elems, elem_size);
	ASSERT(mem != NULL);

	return mem;
}
