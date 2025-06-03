package lexer

type Lexer struct {
	Kinds []TokenKind
	Spans []Span
}

func LexCode(code string) (Lexer, error) {
	lexer := Lexer{}
	ic := 0

	for ic < len(code) {
		// Tokenize next word
		token, _ := LexWord(code, ic)

		// Store token
		if token.Kind != TOKEN_NONE {
			lexer.Kinds = append(lexer.Kinds, token.Kind)
			lexer.Spans = append(lexer.Spans, token.Span)
			// Update (+1 will be added later)
			ic = token.Span.Endxx - 1
		}

		// Update
		ic++
	}

	return lexer, nil
}

func LexWord(code string, ic int) (Token, error) {
	c := charAt(code, ic+0)
	d := charAt(code, ic+1)
	e := charAt(code, ic+2)
	f := charAt(code, ic+3)

	span1 := Span{Start: ic, Endxx: ic + 1}
	span2 := Span{Start: ic, Endxx: ic + 2}
	span3 := Span{Start: ic, Endxx: ic + 3}

	none := TOKEN_NONE
	kind := none
	span := span1

	switch c {
	case '(':
		kind = TOKEN_LPAREN
	case ')':
		kind = TOKEN_RPAREN
	case '{':
		kind = TOKEN_LBRACE
	case '}':
		kind = TOKEN_RBRACE
	case '=':
		if c == '=' && d == '=' {
			kind = TOKEN_EQEQ
			span = span2
		} else {
			kind = TOKEN_EQ
		}
	case 'f':
		if c == 'f' && d == 'n' && isdel(e) {
			kind = TOKEN_FN
			span = span2
		}
	case 'i':
		if c == 'i' && d == 'n' && e == 't' && isdel(f) {
			kind = TOKEN_INT
			span = span3
		}
	case 'l':
		if c == 'l' && d == 'e' && e == 't' && isdel(f) {
			kind = TOKEN_LET
			span = span3
		}
	default:
		kind = none
	}

	kind, span = func() (TokenKind, Span) {
		if kind == none {
			if isalpha(c) || c == '_' {
				k, s := lexid(code, ic)
				return k, s
			}
			if isnum(c) {
				k, s := lexint(code, ic)
				return k, s
			}
		}
		return kind, span
	}()

	token := Token{Kind: kind, Span: span}
	return token, nil
}

//////////////////////////////////////////////////////////////////////

func lexid(code string, ic int) (TokenKind, Span) {
	iend := func() int {
		n := len(code)
		for i := ic; i < n; i++ {
			c := code[i]
			if isdel(c) {
				if c != '_' {
					return i
				}
			}
		}
		return n
	}()

	token := TOKEN_IDVAL
	span := Span{Start: ic, Endxx: iend}

	return token, span
}

func lexint(code string, ic int) (TokenKind, Span) {
	iend := func() int {
		n := len(code)
		for i := ic; i < n; i++ {
			c := code[i]
			if isdel(c) {
				return i
			}
		}
		return n
	}()

	token := TOKEN_INTVAL
	span := Span{Start: ic, Endxx: iend}

	return token, span
}

func charAt(code string, ic int) byte {
	if ic < len(code) {
		return code[ic]
	}
	return ' '
}

func isdel(c byte) bool {
	if isalpha(c) || isnum((c)) {
		return false
	}
	return true
}

func isalpha(c byte) bool {
	if c >= 'a' && c <= 'z' {
		return true
	}
	if c >= 'A' && c <= 'Z' {
		return true
	}
	return false
}

func isnum(c byte) bool {
	if c >= '0' && c <= '9' {
		return true
	}
	return false
}
