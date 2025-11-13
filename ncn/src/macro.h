#pragma once

/// Ignore everything that's passed to this macro
#define IGNORE(X) \
	do { \
		(void)(X); \
	} while (0)
