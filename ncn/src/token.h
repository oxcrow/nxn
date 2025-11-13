#pragma once
#include <stddef.h>
#include <stdint.h>
//
#include "span.h"

enum TokenKind {
	tkNONE = 0,
	tkWHITE,

	// Entities
	tkFN,
	tkSTRUCT,
	tkENUM,
	tkMACRO,

	// Statement operators
	tkLET,
	tkCON,
	tkMUT,
	tkSET,
	tkNEW,
	tkRETURN,
	tkCONTINUE,
	tkBREAK,

	// Branch operators
	tkFOR,
	tkIF,
	tkELSE,

	// Delimiters
	tkAT,
	tkAS,
	tkCOMMA,
	tkCOLON,
	tkSEMICOLON,

	// Postfix operators
	tkHASH,
	tkQUESTION,
	tkEXCLAMATION,

	// Groups
	tkLPAREN,
	tkRPAREN,
	tkLBRACE,
	tkRBRACE,
	tkLBRACK,
	tkRBRACK,

	// Equality operators
	tkNOT,
	tkEQEQ,
	tkEQ,
	tkNE,
	tkLE,
	tkGE,
	tkLT,
	tkGT,

	// Mathematical operators
	tkPLUS,
	tkMINUS,
	tkSTAR,
	tkSLASH,
	tkBAR,

	// Types
	tkTYPE,
	tkBOOL,
	tkI32,
	tkF32,

	// Values
	tkUNDEFINED,
	tkUNITVAL,
	tkTRUE,
	tkFALSE,
	tkI32VAL,
	tkF32VAL,
	tkIDVAL,
};

typedef struct Token {
	enum TokenKind kind;
	struct {
		uint32_t line_num; // Line number
		struct Span col_span; // Column span
		struct Span str_span; // String character span
	} loc;
	struct {
		const char * token; // Where the token is located in code?
		uint32_t len; // Length of token
	} str;
} Token;

typedef struct TokenSlice {
	struct Token * data;
	size_t len;
	size_t cap;
} TokenSlice;

const char * tokenToString(enum TokenKind kind);
