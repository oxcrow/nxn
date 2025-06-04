%{
// NOTE: We do not need this parser generator.
// However it is a good idea to formally define NXN's LL(1) grammar,
// so that others may understand it, and may re-implement it one day.
// It is also a benefical exercise in finding potential bugs in the
// grammar: such as shift-reduce, reduce-reduce, left-recursion,
// and other conflicts that may go unnoticed for a long time,
// and cause tremendous harm.

#include<stdio.h>

// Forward declarations
int yylex();
void yyerror(const char *s);
extern int yylineno;
%}

%token DOC
%token COM

%token FN
%token LET

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

%token EQEQ
%token EQ
%token SEMICOLON

%token UNIT
%token INT

%token INTVAL
%token IDVAL

%token TODO

%%

file: entities

entities
    : // a file may contain no entities
    | function entities // a file may contain one or more functions

function
    : FN IDVAL arguments return body

arguments
    : LPAREN RPAREN // arguments maybe empty

body
    : LBRACE RBRACE // a body maybe empty
    | LBRACE expressions RBRACE // a body maybe made up of multiple expressions

expressions
    : stmt expressions // a body may contain a statement followed by multiple expressions
    | stmt // a body may contain only one statement
    | expr // a body may contain only one expression

stmt
    : assignStmt
    | invokeStmt

expr
    : invokeExpr
    | INTVAL
    | IDVAL

assignStmt
    : LET IDVAL EQ expr SEMICOLON

invokeStmt
    : invokeExpr SEMICOLON

invokeExpr
    : IDVAL LPAREN RPAREN

return
    : // a return type maybe empty in certain conditions
    | type // a return type maybe made up of known types

type
    : unit
    | INT

unit
    : LPAREN RPAREN

%%

void yyerror(const char *s) {
    fprintf(stderr, "Error: %s near (Line: %d)\n", s, yylineno);
}

int main() {
    yyparse();
    return 0;
}
