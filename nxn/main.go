package main

import (
	"fmt"

	qx "modernc.org/qbe"

	cx "nxn/core"
	lx "nxn/lexer"
	px "nxn/parser"
)

func main() {
	const file = "x.n"

	// Read code from file
	code, err := cx.ReadFile(file)
	if err != nil {
		panic(fmt.Sprintf("Failed to read file: %s", err))
	}

	// Tokenize code into tokens
	lexer, err := lx.LexCode(code)
	if err != nil {
		panic(fmt.Sprintf("Failed to tokenize code: %s", err))
	}

	// Parse tokens to create AST
	ast, err := px.ParseCode(lexer)
	if err != nil {
		panic(fmt.Sprintf("Failed to parse code: %s", err))
	}

	cx.Ignore(qx.ADD)
	cx.Ignore(ast)
	bye()
}
