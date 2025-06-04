package ast

import cx "nxn/core"

type Ast struct {
	Funcs []Function
}

// Generic AST node
type Node struct {
	Kind   NodeKind // What kind of AST node is this?
	TokIdx cx.Span  // Token index span
	ExpIdx cx.Span  // Expression index span
}

type Function struct {
	Self Node // What kind of AST node is this?

	Id   Node // Identifier
	Type Node // Return type
	Visy Node // Visibility
	Args Node // Arguments
	Body Node // Body

	Exps []Expression
	code *string
}

type Expression struct {
}

type NodeKind uint8

const (
	// Entities
	NODE_FN NodeKind = iota

	// Types
	NODE_UNIT
	NODE_INT

	// Values
	NODE_ID

	// Visibilities
	NODE_PUBLIC  // Visible to public: fn x() ++ {}
	NODE_LOCAL   // Visible to locals: fn x() + {}
	NODE_PRIVATE // Visible to scope:  fn x() {}

	// Arguments
	NODE_ARG // Argument: fn add(x int, y int) int

	// Expressions
	NODE_ASSIGN // Assignment: let x = 0
	NODE_INVOKE // Invocation: let x = X()
	NODE_CREATE // Creation:   let x = Y{A:0, B:99}
	NODE_EXP    // Expression: x, x+y, x+y*z, etc.

	NODE_NONE
)

//////////////////////////////////////////////////////////////////////

func (f *Function) Identifier() string {
	return (*f.code)[0:69]
}
