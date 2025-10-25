%token <int> INTVAL
%token <float> FLTVAL
%token <string> IDVAL

%token FN
%token STRUCT

%token LET
%token CON
%token MUT
%token SET

%token INT
%token FLT

%token AT
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
%left PLUS MINUS
%left STAR SLASH
%nonassoc UPLUS UMINUS
%nonassoc NOT

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
  | LET v=seplist(COMMA,var); EQUAL e=expressions; SEMICOLON { Ast.LetStmt {vars=v; expr=e;} }
  | RETURN x=expressions; SEMICOLON { Ast.ReturnStmt {expr=x} }

expressions:
  | x=postfix; { x }
  | x=binops; { x }
  | x=unops; { x }

postfix:
  | x=exprs; { x }
  | x=exprs; QUESTION { Ast.UnOpExpr {value=x; op=Ast.TryOp; type'=Ast.NoneType} }

binops:
  | x=expressions; PLUS  y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.AddOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; MINUS y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.SubOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; STAR  y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.MulOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; SLASH y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.DivOp; rvalue=y; type'=Ast.NoneType} }

unops:
  | PLUS x=expressions; %prec UPLUS { Ast.UnOpExpr {value=x; op=Ast.PosOp; type'=Ast.NoneType} }
  | MINUS x=expressions; %prec UMINUS { Ast.UnOpExpr {value=x; op=Ast.NegOp; type'=Ast.NoneType} }
  | EXCLAMATION x=expressions; %prec NOT { Ast.UnOpExpr {value=x; op=Ast.NotOp; type'=Ast.NoneType} }

exprs:
  | LPAREN x=expressions; RPAREN { x }
  | x=entities; { Ast.EntityExpr {value=x; type'=Ast.NoneType} }
  | x=terminals; { Ast.TerminalExpr {value=x; type'=Ast.NoneType} }
  | i=id; LPAREN RPAREN { Ast.InvokeExpr {value=i; type'=Ast.NoneType} }

terminals:
  | LPAREN RPAREN { Ast.UnitVal }
  | x=INTVAL; { Ast.IntVal {value=x} }
  | x=FLTVAL; { Ast.FltVal {value=x} }
  | x=id; { Ast.IdVal {value=x} }
  | DOT LBRACE x=seplist(COMMA,expressions); RBRACE { Ast.StructVal {value=x} }
  | PLUS LBRACE x=seplist(COMMA,expressions); RBRACE { Ast.StructVal {value=x} }

var:
  | s=state; i=id; t=typedec; { Ast.Var {id=i; state=s; type'=t;} }

state:
  | { Ast.LetState }
  | CON { Ast.LetState }
  | MUT { Ast.MutState }
  | SET { Ast.SetState }

typedec:
  | { NoneType }
  | COLON? t=types; { t }

return_types:
  | { Ast.UnitType }
  | t=types; { t }

types:
  | LPAREN RPAREN { Ast.UnitType }
  | INT { Ast.IntType }
  | FLT { Ast.FltType }
  | HASH LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
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

seplist(SEP, NODE):
  | { [] }
  | x=NODE; { [x] }
  | x=NODE; SEP y=seplist(SEP,NODE); { x::y }
