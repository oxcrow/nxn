#include "lexer.h"
#include <ctype.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
//
#include "error.h"
#include "span.h"
#include "string.h"
#include "token.h"

static bool matches(const char * keyword, const StringSlice * code, size_t i) {
	const size_t len = nstrlen(keyword, 9);
	ASSERT(i < code->len);
	ASSERT(i + len < code->len);
	const char end = code->str[i + len];

	const bool matches_keyword = strncmp(keyword, &code->str[i], len) == 0;
	const bool matches_delimiter = !isalpha(end) && !isdigit(end) && (end != '_');

	if (matches_keyword && matches_delimiter) {
		return true;
	}

	return false;
}

static Token collect(
	const char * keyword,
	const StringSlice * code,
	enum TokenKind kind,
	size_t line_num,
	size_t col_num,
	size_t i
) {
	const uint32_t len = nstrlen(keyword, 9);
	return (Token){
		.kind = kind,
		.loc =
			{.line_num = line_num,
			 .col_span = (Span){.start = col_num, .end = col_num + len},
			 .str_span = (Span){.start = i, .end = i + len}},
		.str = {.token = &code->str[i], .len = len},
	};
}

void lexerTokenize(const StringSlice * code, TokenSlice * tokens, Result * error) {
	IS_NOT_NULL(code);
	IS_NOT_NULL(tokens);
	IS_NOT_NULL(error);

	size_t line_num = 0;
	size_t col_num = 0;

	for (size_t i = 0; i < code->len; i++) {
		const char a = okOrChar(charAt(code, i), ' ');
		Token token = {0};

		switch (a) {
		case ' ':
		case '\t':
			token.kind = tkWHITE;
			col_num++;
			break;
		case '\n':
			token.kind = tkWHITE;
			line_num++;
			col_num = 0;
			break;
		case '\r':
			fprintf(stderr, "Error: Carraige return '\\r' is not allowed!\n");
			ASSERT(false);
			break;
		case 'f':
			if (matches("fn", code, i)) {
				token = collect("fn", code, tkFN, line_num, col_num, i);
			}
			break;
		case 'i':
			if (matches("i32", code, i)) {
				token = collect("i32", code, tkI32, line_num, col_num, i);
			}
			break;
		case 'l':
			if (matches("let", code, i)) {
				token = collect("let", code, tkLET, line_num, col_num, i);
			}
			break;
		case 'm':
			if (matches("mut", code, i)) {
				token = collect("mut", code, tkMUT, line_num, col_num, i);
			}
			break;
		case 'n':
			if (matches("new", code, i)) {
				token = collect("new", code, tkNEW, line_num, col_num, i);
			}
			break;
		case 'r':
			if (matches("return", code, i)) {
				token = collect("return", code, tkRETURN, line_num, col_num, i);
			}
			break;
		case 's':
			if (matches("set", code, i)) {
				token = collect("set", code, tkSET, line_num, col_num, i);
			}
			break;
		case ';':
			token = collect(";", code, tkSEMICOLON, line_num, col_num, i);
			break;
		case '(':
			token = collect("(", code, tkLPAREN, line_num, col_num, i);
			break;
		case ')':
			token = collect(")", code, tkRPAREN, line_num, col_num, i);
			break;
		case '{':
			token = collect("{", code, tkLBRACE, line_num, col_num, i);
			break;
		case '}':
			token = collect("}", code, tkRBRACE, line_num, col_num, i);
			break;
		case '[':
			token = collect("[", code, tkLBRACK, line_num, col_num, i);
			break;
		case ']':
			token = collect("]", code, tkRBRACK, line_num, col_num, i);
			break;
		case '=':
			if (matches("==", code, i)) {
				token = collect("==", code, tkEQEQ, line_num, col_num, i);
			} else if (matches("=", code, i)) {
				token = collect("=", code, tkEQ, line_num, col_num, i);
			}
			break;
		default:
			break;
		}

		if (token.kind != tkNONE && token.kind != tkWHITE) {
			if (0 == 1) {
				printf(
					"%-10s |    Line: %4d, ColumnSpan: (%4d, %4d)\n",
					tokenToString(token.kind),
					token.loc.line_num,
					token.loc.col_span.start,
					token.loc.col_span.end
				);
			}
			col_num += token.str.len;
			i += token.str.len - 1; // -1 to negate extra i++ (at the top)
		} else if (token.kind == tkWHITE) {
			continue;
		} else {
			col_num++;
		}
	}
}
