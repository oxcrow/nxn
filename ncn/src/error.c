#include "error.h"

char okOrChar(OptionChar c, char other) {
	if (c.error == 0) {
		return c.data;
	}
	return other;
}
