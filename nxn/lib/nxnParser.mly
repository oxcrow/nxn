%token <int> INTVAL
%token <float> FLOATVAL
%token <string> IDVAL

%token FN
%token LET

%token INT
%token FLOAT

%token SEMICOLON
%token COMMA
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

(* Lowest priority at the top *)
(* Highest priority at the bottom *)
%right EQUAL
%left PLUS MINUS
%left STAR SLASH

%start file
%type  <NxnAst.file> file

%%

file:
  | EOF { NxnAst.File [] }
  | e = nonempty_list(entities) EOF {NxnAst.File (e)}

entities:
  | FN i=id; LPAREN RPAREN t=types; b=blocks; { NxnAst.Function {id=i; type'=t; block=b;} }

blocks:
  | LBRACE s=list(statements); r=option(return_expression); RBRACE { NxnAst.Block {stmts=(match r with | Some x -> List.rev (x::s) | None -> s)} }

statements:
  | LET v=var; EQUAL e=expressions; SEMICOLON { NxnAst.LetStmt {var=v; expr=e;} }
  | RETURN x=expressions; SEMICOLON { NxnAst.ReturnStmt {expr=x} }

return_expression:
  | x=expressions; { NxnAst.ReturnExprStmt {expr=x} }

expressions:
  | x=terminals; { NxnAst.TerminalExpr {value=x; type'=NxnAst.NoneType} }
  | i=id; LPAREN RPAREN { NxnAst.InvokeExpr {value=i; type'=NxnAst.NoneType} }
  | LPAREN x=expressions; RPAREN { x } (* Is the precedence of ( ) correct? *)

var:
  | i=id; { NxnAst.Var {id=i; type'=NxnAst.NoneType} }
  | LPAREN v=separated_nonempty_list(COMMA, var); RPAREN { NxnAst.TupleVar { var=v; }  }

terminals:
  | LPAREN RPAREN { NxnAst.UnitVal }
  | x=INTVAL; { NxnAst.IntVal {value=x} }
  | x=FLOATVAL; { NxnAst.FloatVal {value=x} }
  | x=id; { NxnAst.IdVal {value=x} }
  | LPAREN x=separated_nonempty_list(COMMA, expressions); RPAREN { NxnAst.TupleVal {value=x} }

types:
  | { NxnAst.UnitType }
  | LPAREN RPAREN { NxnAst.UnitType }
  | INT { NxnAst.IntType }
  | FLOAT { NxnAst.FloatType }
  | LPAREN t=separated_nonempty_list(COMMA, types) RPAREN { NxnAst.TupleType {value=t;} }

id:
  | loc=locate; i=IDVAL; { NxnAst.Id {value=i; loc=loc} }

let locate == {
  let startpos: Lexing.position = $startpos in
  let lnum = startpos.pos_lnum in
  let cnum = startpos.pos_cnum - startpos.pos_bol + 2 in
  let loc = NxnAst.Loc{lnum=lnum; cnum=cnum} in
  loc
}

let locate_node(node) == data=node; {
  let startpos: Lexing.position = $startpos in
  let lnum = startpos.pos_lnum in
  let cnum = startpos.pos_cnum - startpos.pos_bol + 2 in
  let loc = NxnAst.Loc{lnum=lnum; cnum=cnum} in
  (data, loc)
}
