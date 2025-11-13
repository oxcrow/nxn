#include "file.h"
#include <stddef.h>
#include <stdio.h>
//
#include "error.h"
#include "memory.h"

void fileReadToString(StringSlice * content, const char * filename, Result * error) {
	IS_NOT_NULL(content);
	IS_NOT_NULL(filename);
	IS_NOT_NULL(error);

	FILE * file = fopen(filename, "r");
	IS_NOT_NULL(file);

	// Read file size
	fseek(file, 0, SEEK_END);
	const size_t text_size = ftell(file);
	fseek(file, 0, SEEK_SET);

	// Allocate memory for file text + 1 for NULL terminator
	char * text = xcalloc(text_size + 1, sizeof(char));

	// Read the file into string and add NULL terminator at the end
	const size_t bytes_read = fread(text, 1, text_size, file);
	ASSERT(bytes_read == text_size);
	text[text_size] = '\0';

	// Store data
	content->str = text;
	content->len = text_size;
	content->cap = text_size + 1;

	fclose(file);
}
