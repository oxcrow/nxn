package ast

import cx "nxn/core"

type Ast struct {
	Funcs []Function
	code  *string
}

// Generic AST node
type Node struct {
	Kind   NodeKind // What kind of node is this?
	TokIdx cx.Span  // Which tokens made this node?
	AstIdx cx.Span  // Where in the AST is this node and its children stored?
}

type Function struct {
	Self Node // What kind of AST node is this?

	Id   Identifier
	Type Type
	Visy Visibility
	Args Arguments
	Body Body

	code *string
}

type Identifier struct {
	Self Node
}

type Type struct {
	Self Node
}

type Visibility struct {
	Self Node
	Kind VisibilityKind
}

type Arguments struct {
	Self Node
}

type Argument struct {
	Self Node
	Type Type
}

type Body struct {
	Self Node
	Exps []Expression
}

type Expression struct {
	Self Node
}

type NodeKind uint8

const (
	NODE_START NodeKind = iota
	NODE_END

	NODE_FUNCTION

	NODE_CALL
	NODE_ASSIGN

	NODE_VISIBILITY
	NODE_TYPE

	NODE_NONE
)

type VisibilityKind uint8

const (
	VISIBILITY_EXPORT VisibilityKind = iota // Visible to public: fn X() {}
	VISIBILITY_LOCAL                        // Visible to locals: fn x() {}
	VISIBILITY_FILE                         // Visible to file:   fn x() {} or X() {}
)

//////////////////////////////////////////////////////////////////////

func (f *Function) Identifier() string {
	return ""
}
