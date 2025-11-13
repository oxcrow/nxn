#include "token.h"

const char * tokenToString(enum TokenKind kind) {
	switch (kind) {
	case tkNONE:
		return "NONE";
	case tkFN:
		return "FN";
	case tkLET:
		return "LET";
	case tkRETURN:
		return "RETURN";
	case tkSEMICOLON:
		return ";";
	case tkLPAREN:
		return "(";
	case tkRPAREN:
		return ")";
	case tkLBRACE:
		return "{";
	case tkRBRACE:
		return "}";
	case tkLBRACK:
		return "[";
	case tkRBRACK:
		return "]";
	case tkEQEQ:
		return "==";
	case tkEQ:
		return "=";
	case tkI32:
		return "I32";
	default:
		return "";
	}
	return "";
}
