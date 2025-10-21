%token <int> INTVAL
%token <float> FLOATVAL
%token <string> IDVAL

%token FN
%token STRUCT
%token ENUM
%token TRAIT
%token IMPLEMENT

%token LET
%token CON
%token MUT
%token SET

%token LOOP
%token WHILE
%token FOR
%token IF
%token ELSE
%token CONTINUE
%token BREAK
%token MATCH

%token UNSAFE
%token EXTERN
%token PUBLIC
%token LOCAL

%token TYPE
%token INT
%token FLOAT
%token TRUE
%token FALSE
%token UNDEFINED
%token AS

%token EQ
%token NE
%token LE
%token GE
%token LT
%token GT

%token AT
%token SEMICOLON
%token COLON
%token COMMA
%token DOTDOT
%token DOT
%token QUESTION
%token EXCLAMATION
%token AMPERSAND
%token HASH
%token PERCENT
%token DOLLAR
%token EQUAL
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token LANGLE
%token RANGLE
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token CARET

%token RETURN
%token EOF

(* Lowest priority at the top *)
%left PLUS MINUS
%left STAR SLASH
%nonassoc UPLUS UMINUS
%nonassoc UCONREF UMUTREF UDEREF
%nonassoc LE GE LT GT
%nonassoc EQ NE
%nonassoc NOT
(* Highest priority at the bottom *)

%start file
%type  <Ast.file> file

%%

file:
  | EOF { Ast.File { entities = [] } }
  | e = nonempty_list(entities) EOF {Ast.File { entities = e }}

entities:
  | FN i=id; LPAREN a=seplist(COMMA,vars); RPAREN t=return_types; b=blocks;
    { Ast.Function {
          id=i;
          args=a;
          type'=(Ast.FunctionType{args=(List.map (fun var -> match var with | Ast.Var v -> v.type') a); type'=t});
          block=b;} }

blocks:
  | LBRACE s=list(statements); RBRACE { Ast.Block {stmts=s} }

statements:
  | LET v=seplist(COMMA,vars); EQUAL e=expressions; SEMICOLON { Ast.LetStmt {vars=v; expr=e;} }
  | v=seplist(COMMA,expressions) EQUAL e=expressions; SEMICOLON { Ast.SetStmt {vars=v; expr=e;} }
  | RETURN x=expressions; SEMICOLON { Ast.ReturnStmt {expr=x} }
  | x=ifstmts; { x }

ifstmts:
  | IF c=expressions; b=blocks; o=option(elsestmts); { Ast.IfStmt {cond=c; block=b; other=o} }

elsestmts:
  | ELSE IF c=expressions; b=blocks; o=option(elsestmts); { Ast.ElseIfStmt {cond=c; block=b; other=o} }
  | ELSE b=blocks; { Ast.ElseStmt {block=b} }

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
  | x=expressions; EQ    y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.EqOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; NE    y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.NeOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; LE    y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.LeOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; GE    y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.GeOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; LT    y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.LtOp; rvalue=y; type'=Ast.NoneType} }
  | x=expressions; GT    y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.GtOp; rvalue=y; type'=Ast.NoneType} }

unops:
  | PLUS x=expressions; %prec UPLUS { Ast.UnOpExpr {value=x; op=Ast.PosOp; type'=Ast.NoneType} }
  | MINUS x=expressions; %prec UMINUS { Ast.UnOpExpr {value=x; op=Ast.NegOp; type'=Ast.NoneType} }
  | EXCLAMATION x=expressions; %prec NOT { Ast.UnOpExpr {value=x; op=Ast.NotOp; type'=Ast.NoneType} }
  | x=refs; { x }

refs:
  | AMPERSAND x=expressions; %prec UCONREF { Ast.UnOpExpr {value=x; op=Ast.ConRefOp; type'=Ast.NoneType} }
  | AMPERSAND MUT x=expressions; %prec UMUTREF { Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType} }
  | STAR x=expressions; %prec UMUTREF { Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType} }
  | CARET x=expressions; %prec UDEREF { Ast.UnOpExpr {value=x; op=Ast.DerefOp; type'=Ast.NoneType} }

exprs:
  | LPAREN x=expressions; RPAREN { x }
  | x=entities; { Ast.EntityExpr {value=x; type'=Ast.NoneType} }
  | x=terminals; { Ast.TerminalExpr {value=x; type'=Ast.NoneType} }
  | i=id; LPAREN a=seplist(COMMA,expressions); RPAREN { Ast.InvokeExpr {value=i; args=a; type'=Ast.NoneType} }

terminals:
  | UNDEFINED { Ast.UndefinedVal }
  | LPAREN RPAREN { Ast.UnitVal }
  | x=INTVAL; { Ast.IntVal {value=x} }
  | x=FLOATVAL; { Ast.FloatVal {value=x} }
  | x=id; { Ast.IdVal {value=x} }
  | DOT LBRACE x=seplist(COMMA,expressions); RBRACE { Ast.StructVal {value=x} }
  | PLUS LBRACE x=seplist(COMMA,expressions); RBRACE { Ast.StructVal {value=x} }

vars:
  | s=state; i=id; t=typedec; { Ast.Var {id=i; state=s; type'=t;} }

state:
  | { Ast.ConState }
  | CON { Ast.ConState }
  | MUT { Ast.MutState }
  | SET { Ast.SetState }

typedec:
  | { NoneType }
  | COLON t=types; { t }

return_types:
  | { Ast.UnitType }
  | t=types; { t }

types:
  | LPAREN RPAREN { Ast.UnitType }
  | INT { Ast.IntType }
  | FLOAT { Ast.FloatType }
  | HASH LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
  | STRUCT LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
  | AMPERSAND SLASH t=types; SLASH l=option(id); { Ast.ConRefType {life=l; types=t} }
  | AMPERSAND MUT SLASH t=types; SLASH l=option(id); { Ast.MutRefType {life=l; types=t} }

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
