package main

import (
	"fmt"

	lx "nxn/lexer"
)

func main() {
	const file = "x.n"

	// Read code from file
	code, err := readFile(file)
	if err != nil {
		panic(fmt.Sprintf("Failed to read file: %s", err))
	}

	// Tokenize code into tokens
	lexer, err := lx.LexCode(code)
	if err != nil {
		panic(fmt.Sprintf("Failed to tokenize code: %s", err))
	}

	ignore(lexer)
	bye()
}
