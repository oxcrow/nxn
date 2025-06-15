package parser

import (
	"fmt"

	ax "nxn/ast"
	cx "nxn/core"
	lx "nxn/lexer"
)

type Parser struct {
	lexer *lx.Lexer
}

func ParseCode(lexer lx.Lexer) (ax.Ast, error) {
	parser := Parser{lexer: &lexer}
	_, e := parseCode(&parser, lexer, 0)
	return ax.Ast{}, e
}

func Peek(lexer lx.Lexer, i int) lx.TokenKind {
	if i < len(lexer.Kinds) {
		return lexer.Kinds[i]
	}
	return lx.TOKEN_NONE
}

//////////////////////////////////////////////////////////////////////

func parseCode(parser *Parser, lexer lx.Lexer, i int) (int, error) {
	ntokens := lexer.NumTokens()
	k := i

	for ; k < ntokens; k++ {
		kind := lexer.Kinds[k]
		switch kind {
		case lx.TOKEN_FN:
			kk, e := parseFunction(parser, lexer, k)
			if e != nil {
				panic(fmt.Sprintf("Failed to parse function. %v", e))
			}
			k = kk - 1 // +1 will be added later
		}
	}

	return k, nil
}

func parseFunction(parser *Parser, lexer lx.Lexer, i int) (int, error) {
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

func parseBody(parser *Parser, lexer lx.Lexer, i int) (int, error) {
	k, e := matchToken(lexer, lx.TOKEN_LBRACE, i)
	if e != nil {
		return k, e
	}

	k, e = parseStatements(parser, lexer, k)
	if e != nil {
		return k, e
	}

	k, e = maybeToken(lexer, lx.TOKEN_RBRACE, i)
	return k, e
}

func parseStatements(parser *Parser, lexer lx.Lexer, i int) (int, error) {
	next := Peek(lexer, i)
	k, e := i, cx.Nil()

	switch next {
	case lx.TOKEN_LET:
		k, e = parseLetAssignStmt(parser, lexer, i) // BUG: Implement this
		if e != nil {
			return k, e
		}
	case lx.TOKEN_INTVAL:
		later := Peek(lexer, k)
		switch later {
		case lx.TOKEN_RBRACE:
			// TODO: Parse return expression
		default:
			break
			// return k, fmt.Errorf("Integer expression can not end with illegal token")
		}
	case lx.TOKEN_IDVAL:
		later := Peek(lexer, k)
		switch later {
		case lx.TOKEN_LPAREN:
			// TODO: Parse function invocation
		case lx.TOKEN_LBRACE:
			// TODO: Parse struct construction
		case lx.TOKEN_RBRACE:
			// TODO: Parse return expression
		default:
			break
			// return k, fmt.Errorf("Identifier expression can not end with illegal token, %v", later)
		}
	}

	return k, e
}

func parseLetAssignStmt(parser *Parser, lexer lx.Lexer, i int) (int, error) {
	k, e := matchToken(lexer, lx.TOKEN_LET, i)
	if e != nil {
		return k, e
	}

	k, e = matchToken(lexer, lx.TOKEN_IDVAL, k)
	if e != nil {
		return k, e
	}

	k, e = matchToken(lexer, lx.TOKEN_EQ, k)
	if e != nil {
		return k, e
	}

	k, e = parseLetAssignStmt(parser, lexer, k)
	if e != nil {
		return k, e
	}

	return k, e
}

func parseType(parser *Parser, lexer lx.Lexer, i int) (int, error) {
	ntokens := lexer.NumTokens()
	types := []lx.TokenKind{} // TODO: Store in parser
	idxs := []int{}           // TODO: Store in parser
	k := i

	// Find the type compatible tokens.
	// Store the tokens and their indices in the parser.

loop:
	for ; k < ntokens; k++ {
		kind := Peek(lexer, k)
		switch kind {
		case lx.TOKEN_INT:
			types = append(types, kind)
			idxs = append(idxs, k)
		case lx.TOKEN_IDVAL:
			types = append(types, kind)
			idxs = append(idxs, k)
		default:
			break loop
		}
	}

	// If no type compatible token was found then we assume that the
	// function's type is Unit (), as in fn main() {},
	// thus we default to it.

	if len(types) == 0 {
		types = append(types, lx.TOKEN_UNIT)
		idxs = append(idxs, k) // NOTE: Really? But the token doesn't exist.
	}

	return k, nil
}

func parseIdent(parser *Parser, lexer lx.Lexer, i int) (int, error) {
	k, e := matchToken(lexer, lx.TOKEN_IDVAL, i)
	return k, e
}

func matchToken(lexer lx.Lexer, expect lx.TokenKind, i int) (int, error) {
	kind := Peek(lexer, i)
	if kind != expect {
		return i, fmt.Errorf("Expected token kind does not match. (Kind: %v, Expect: %v, TokenIndex: %v)", kind, expect, i)
	}
	return i + 1, nil
}

func maybeToken(lexer lx.Lexer, expect lx.TokenKind, i int) (int, error) {
	kind := Peek(lexer, i)
	if kind != expect {
		// not an error (we expected the token to not exist)
	}
	return i + 1, nil
}
