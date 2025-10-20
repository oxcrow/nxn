%token <int> INTVAL
%token <string> IDVAL

%token FN
%token STRUCT

%token LET
%token CON
%token MUT
%token SET

%token INT

%token SEMICOLON
%token COLON
%token COMMA
%token DOTDOT
%token DOT
%token QUESTION
%token EXCLAMATION
%token HASH
%token EQUAL
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token STAR
%token SLASH

%token RETURN
%token EOF

(* Highest priority at the bottom *)
(* Lowest priority at the top *)
%right EQUAL
%left PLUS MINUS
%left STAR SLASH

%start file
%type  <Ast.file> file

%%

file:
  | EOF { Ast.File { entities = [] } }
  | e = nonempty_list(entities) EOF {Ast.File { entities = e }}

entities:
  | FN i=id; LPAREN RPAREN t=return_types; b=blocks; { Ast.Function {id=i; type'=t; block=b;} }

blocks:
  | LBRACE s=list(statements); RBRACE { Ast.Block {stmts=s} }

statements:
  | LET v=vars; EQUAL e=expressions; SEMICOLON { Ast.LetStmt { vars=v; expr=e; } }
  | RETURN x=expressions; SEMICOLON { Ast.ReturnStmt {expr=x} }

expressions:
  | x=entities; { Ast.EntityExpr { value=x; type'=Ast.NoneType } }
  | x=terminals; { Ast.TerminalExpr { value=x; type'=Ast.NoneType } }
  | i=id; LPAREN RPAREN { Ast.InvokeExpr { value=i; type'=Ast.NoneType } }
  | LPAREN x=expressions; RPAREN { x }

terminals:
  | LPAREN RPAREN { Ast.UnitVal }
  | x=INTVAL; { Ast.IntVal {value=x} }
  | x=id; { Ast.IdVal {value=x} }
  | DOT LBRACE x=separated_list(COMMA,expressions); RBRACE { Ast.StructVal {value=x} }

vars:
  | { [Ast.NoneVar] }
  | v1=var; { [v1] }
  | v1=var; COMMA v2=vars; { v1 :: v2 }

var:
  | s=state; i=id; t=typedec; { Ast.Var {id=i; state=s; type'=t;} }

state:
  | { Ast.ImmutableState }
  | CON { Ast.ImmutableState }
  | MUT { Ast.MutableState }
  | SET { Ast.AssignState }

typedec:
  | { NoneType }
  | COLON? t=types; { t }

return_types:
  | { Ast.UnitType }
  | t=types; { t }

types:
  | LPAREN RPAREN { Ast.UnitType }
  | INT { Ast.IntType }
  | PLUS LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
  | STRUCT LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }

id:
  | loc=locate; i=IDVAL; { Ast.Id {value=i; loc=loc} }

let locate == {
  let startpos: Lexing.position = $startpos in
  let lnum = startpos.pos_lnum in
  let cnum = startpos.pos_cnum - startpos.pos_bol + 2 in
  let loc = Ast.Loc{lnum=lnum; cnum=cnum} in
  loc
}

let locate_node(node) == data=node; {
  let startpos: Lexing.position = $startpos in
  let lnum = startpos.pos_lnum in
  let cnum = startpos.pos_cnum - startpos.pos_bol + 2 in
  let loc = Ast.Loc{lnum=lnum; cnum=cnum} in
  (data, loc)
}
