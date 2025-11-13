#include "string.h"
#include <stddef.h>
//
#include "error.h"

size_t nstrlen(const char * str, size_t len_max) {
	ASSERT(str != NULL);
	bool null_found = false;
	size_t len = 0;

	for (size_t i = 0; i < len_max; i++) {
		if (str[i] == '\0') {
			null_found = true;
			len = i;
			break;
		}
	}

	ASSERT(null_found);
	return len;
}

OptionChar charAt(const StringSlice * str, size_t i) {
	if (i < str->len) {
		return (OptionChar){.data = str->str[i], .error = 0};
	}
	return (OptionChar){.data = ' ', .error = 1};
}

StringSlice stringSlice_Init(char * str, size_t len_str) {
	const StringSlice result = {.str = str, .len = len_str};
	return result;
}
