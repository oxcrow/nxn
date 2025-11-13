#pragma once
#include <stddef.h>
//
#include "error.h"
#include "string.h"
#include "token.h"

void lexerTokenize(const StringSlice * code, TokenSlice * tokens, Result * er);
