%token <int> INTVAL
%token <string> IDVAL

%token FN
%token LET

%token INT

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

(* Highest priority at the bottom *)
(* Lowest priority at the top *)
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
    | FN i=id; LPAREN RPAREN t=types; b=blocks; { NxnAst.Function {id=i; typex=t; block=b;} }

blocks:
    | LBRACE s=list(statements); RBRACE { NxnAst.Block {stmts=s} }

statements:
    | LET i=id; EQUAL e=expressions; { NxnAst.LetStmt {id=i; expr=e;} }
    | RETURN x=expressions; { NxnAst.ReturnStmt {expr=x} }

expressions:
    | x=terminals; { NxnAst.TerminalExpr {x=x} }
    | i=id; LPAREN RPAREN { NxnAst.InvokeExpr {x=i} }

terminals:
    | x=INTVAL; { NxnAst.IntVal {x=x} }
    | x=id; { NxnAst.IdVal {x=x} }

types:
    | { NxnAst.TypeUnit }
    | LPAREN RPAREN { NxnAst.TypeUnit }
    | INT { NxnAst.TypeInt }

id:
    | loc=locate; i=IDVAL; { NxnAst.Id {x=i; loc=loc} }

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
