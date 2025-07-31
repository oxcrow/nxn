%token <int> INTVAL
%token <float> FLOATVAL
%token <string> IDVAL

%token FN
%token LET

%token INT
%token FLOAT

%token SEMICOLON
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
    | LBRACE s=list(statements); RBRACE { NxnAst.Block {stmts=s} }

statements:
    | LET i=id; EQUAL e=expressions; { NxnAst.LetStmt {id=i; expr=e;} }
    | RETURN x=expressions; { NxnAst.ReturnStmt {expr=x} }

expressions:
    | x=terminals; { NxnAst.TerminalExpr {value=x; type'=NxnAst.TypeNone} }
    | i=id; LPAREN RPAREN { NxnAst.InvokeExpr {value=i; type'=NxnAst.TypeNone} }

terminals:
    | x=INTVAL; { NxnAst.IntVal {value=x} }
    | x=FLOATVAL; { NxnAst.FloatVal {value=x} }
    | x=id; { NxnAst.IdVal {value=x} }

types:
    | { NxnAst.TypeUnit }
    | LPAREN RPAREN { NxnAst.TypeUnit }
    | INT { NxnAst.TypeInt }
    | FLOAT { NxnAst.TypeFloat }

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
