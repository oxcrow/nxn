#pragma once
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/// Error result code (returned from functions)
typedef int Result;

/// Common error codes
enum ErrorCodes {
	OOPS = -77 // Unknown (Use when we don't know the error; or are just lazy)
};

/// Optional char with error code
typedef struct OptionChar {
	Result error;
	char data;
} OptionChar;

/// Extract char data if result is ok; or else return another value
char okOrChar(OptionChar c, char other);

/// Macro to mark sections of code as being incomplete
#define TODO(MESSAGE) \
	fprintf(stderr, "TODO: %s\n", MESSAGE); \
	ASSERT(false)

/// Macro to assert a condition, and propagate errors upwards
#define ENSURE(CONDITION, ERROR_CODE) \
	if ((CONDITION) == false) { \
		return (ERROR_CODE); \
	}

/// Macro to assert a condition, and abort on error
#define ASSERT(CONDITION) \
	if ((CONDITION) == false) { \
		fprintf(stderr, "Assert conditon failed: %s\n", #CONDITION); \
		fprintf(stderr, "Aborting from file: %s:%d\n", __FILE__, __LINE__); \
		abort(); \
	}

/// Macro to assert a pointer is not NULL
#define IS_NOT_NULL(POINTER) ASSERT((POINTER) != NULL)

/// Macro to run functions, check them for errors, and crash.
///
/// Why do we need this?
///
/// I wish we didn't need this; but since C doesn't have error propagation
/// methods, like try in Zig, and ? in Rust, we're forced to use macros like
/// this to propagate the errors upwards through our call-stack.
///
/// Since we can't propagate errors easily; it's better to crash instead.
#define TRY(FUNCTION) \
	FUNCTION; \
	ASSERT(error == 0)

/// Macro to assert if index access is within bounds
#define BOUNDS(INDEX, SIZE) ASSERT((INDEX) < (SIZE))

/// Macro to assert if index access is within bounds
#define BOUNDS2(INDEX1, INDEX2, SIZE) \
	BOUNDS(INDEX1, SIZE); \
	BOUNDS(INDEX2, SIZE)

/// Macro to assert if index access is within bounds
#define BOUNDS3(INDEX1, INDEX2, INDEX3, SIZE) \
	BOUNDS(INDEX1, SIZE); \
	BOUNDS(INDEX2, SIZE); \
	BOUNDS(INDEX3, SIZE)

/// Macro to assert if index access is within bounds
#define BOUNDS4(INDEX1, INDEX2, INDEX3, INDEX4, SIZE) \
	BOUNDS(INDEX1, SIZE); \
	BOUNDS(INDEX2, SIZE); \
	BOUNDS(INDEX3, SIZE); \
	BOUNDS(INDEX4, SIZE)

/// Macro to assert if multiplication can be done without overflow
#define IS_NOT_OVERFLOW_MUL(X, Y, XYMAX) (((X) < (XYMAX / (Y))))

/// Macro to assert if multiplication can be done safely to create non-zero results
#define IS_FINITE_MUL(X, Y, XYMAX) (((X) > 0 && (Y) > 0) && (IS_NOT_OVERFLOW_MUL(X, Y, XYMAX)))
