package lexer

type TokenKind uint8

const (
	TOKEN_DOC TokenKind = iota
	TOKEN_COM

	TOKEN_FN
	TOKEN_LET

	TOKEN_LPAREN
	TOKEN_RPAREN
	TOKEN_LBRACE
	TOKEN_RBRACE

	TOKEN_EQEQ
	TOKEN_EQ

	TOKEN_UNIT
	TOKEN_INT

	TOKEN_INTVAL
	TOKEN_IDVAL

	TOKEN_NONE
)

type Token struct {
	Kind TokenKind
	Span Span
}

type Span struct {
	Start int
	Endxx int
}
