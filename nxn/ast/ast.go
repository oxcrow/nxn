package ast

import (
	cx "nxn/core"
	lx "nxn/lexer"
)

type Ast struct {
	Fns   []Function
	lexer *lx.Lexer
}

type Function struct {
	Head  NodeHead
	Id    Id
	Type  Type
	Stmts Statements
	Exprs Expressions
	lexer *lx.Lexer
}

type Statements struct {
	LetAssignStmts []LetAssignStmt
	InvokeStmts    []InvokeStmt
}

type Expressions struct {
	InvokeExprs []InvokeExpr
	IntExprs    []IntExpr
	IdExprs     []IdExpr
}

type NodeHead struct {
	Kind     NodeKind
	TokenIdx cx.Span
}

type LetAssignStmt struct {
	Head NodeHead
	Id   Id
	Type Type
	Expr Expression
}

type InvokeStmt struct {
	Head NodeHead
}

type InvokeExpr struct {
	Head NodeHead
	Type Type
}

type IntExpr struct {
	Head NodeHead
	Type Type
}

type IdExpr struct {
	Head NodeHead
	Id   Id
	Type Type
}

type Expression struct {
	Head NodeHead
	Type Type
}

type Id struct {
	Head NodeHead
	Name string
}

type Type struct {
	Kind int
}

type NodeKind uint8

const (
	NODE_FUNCTION NodeKind = iota

	NODE_ASSIGN_STMT
	NODE_INVOKE_STMT
	NODE_INVOKE_EXPR

	NODE_UNIT
	NODE_INT

	NODE_INTVAL
	NODE_IDVAL

	NODE_NONE
)

//////////////////////////////////////////////////////////////////////

func CreateNodeHead(kind NodeKind, tokenIdx cx.Span) NodeHead {
	return NodeHead{Kind: kind, TokenIdx: tokenIdx}
}

func (x *Ast) SetLexer(lexer *lx.Lexer) {
	x.lexer = lexer
}

func (x *Function) SetLexer(lexer *lx.Lexer) {
	x.lexer = lexer
}

func (x *Statements) NumStmts() int {
	return len(x.LetAssignStmts) + len(x.InvokeStmts)
}

func (x *Expressions) NumExprs() int {
	return len(x.InvokeExprs) + len(x.IntExprs) + len(x.IdExprs)
}
