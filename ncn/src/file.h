#pragma once
#include "error.h"
#include "string.h"

void fileReadToString(StringSlice * content, const char * filename, Result * er);
