package parser

import (
	"fmt"

	ax "nxn/ast"
	cx "nxn/core"
	lx "nxn/lexer"
)

type Parser struct {
	//
}

func ParseCode(lexer lx.Lexer) (ax.Ast, error) {
	parser := Parser{}
	ntokens := len(lexer.Kinds)

	for i := 0; i < ntokens; i++ {
		kind := lexer.Kinds[i]
		switch kind {
		case lx.TOKEN_FN:
			k, e := parseFunction(parser, lexer, i)
			if e != nil {
				panic(fmt.Sprintf("Failed to parse function. %v", e))
			}
			i = k - 1 // +1 will be added later
		}
	}

	cx.Ignore(ntokens)
	return ax.Ast{}, nil
}

//////////////////////////////////////////////////////////////////////

func parseFunction(parser Parser, lexer lx.Lexer, i int) (int, error) {
	k, e := matchToken(lexer, lx.TOKEN_FN, i)
	if e != nil {
		return k, e
	}

	k, e = parseIdent(parser, lexer, k)
	if e != nil {
		return k, e
	}

	k, e = matchToken(lexer, lx.TOKEN_LPAREN, k)
	if e != nil {
		return k, e
	}

	k, e = matchToken(lexer, lx.TOKEN_RPAREN, k)
	if e != nil {
		return k, e
	}

	k, e = parseType(parser, lexer, k)
	if e != nil {
		return k, e
	}

	k, e = parseBody(parser, lexer, k)
	return k, e
}

func parseBody(parser Parser, lexer lx.Lexer, i int) (int, error) {
	matchToken(lexer, lx.TOKEN_LBRACE, i)
	return i, nil
}

func parseType(parser Parser, lexer lx.Lexer, i int) (int, error) {
	k, e := matchToken(lexer, lx.TOKEN_INT, i)
	return k, e
}

func parseIdent(parser Parser, lexer lx.Lexer, i int) (int, error) {
	k, e := matchToken(lexer, lx.TOKEN_IDVAL, i)
	return k, e
}

func matchToken(lexer lx.Lexer, expect lx.TokenKind, i int) (int, error) {
	kind := lexer.Kinds[i]
	if kind != expect {
		panic(fmt.Sprintf("Expected token kind does not match. (Kind: %v, Expect: %v, TokenIndex: %v)", kind, expect, i))
	}
	return i + 1, nil
}

func maybeToken(lexer lx.Lexer, expect lx.TokenKind, i int) (int, error) {
	kind := lexer.Kinds[i]
	if kind != expect {
		return i, nil
	}
	return i + 1, nil
}
