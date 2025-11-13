#include <stdio.h>
#include <stdlib.h>
//
#include "error.h"
#include "file.h"
#include "lexer.h"
#include "memory.h"
#include "string.h"
#include "token.h"

void shutdown(void) {
	printf("+\n");
}

void compile(const char * filename) {
	Result error = 0;
	StringSlice code = {0};
	TokenSlice tokens = {0};

	TRY(fileReadToString(&code, filename, &error));
	TRY(lexerTokenize(&code, &tokens, &error));

	xfree(code.str);
}

int main(int argc, char ** argv) {
	compile("../test/pass/001-basic/001-return/001.nxn");
	compile("../test/pass/001-basic/002-variable/001.nxn");
	shutdown();
	return 0;
}
