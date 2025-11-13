#include <stdio.h>

void shutdown(void) {
	printf("+\n");
}

int main(int argc, char ** argv) {
	shutdown();
	(void)argc;
	(void)argv;
	return 0;
}
