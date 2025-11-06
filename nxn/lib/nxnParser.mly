%{
  let location (x: Lexing.position) =
    let lnum = x.pos_lnum in
    let cnum = x.pos_cnum - x.pos_bol in
    let loc = Ast.Loc{lnum=lnum; cnum=cnum} in
    loc
  ;;

  let position (startpos: Lexing.position) (endpos: Lexing.position) =
    let start = location startpos in
    let end' = location endpos in
    let pos = Ast.Pos{start=start; end'=end'} in
    pos
  ;;
%}

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
%token BOOL
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
%token BAR

%token RETURN
%token EOF

(* Lowest priority at the top *)
%nonassoc LE GE LT GT
%nonassoc EQ NE
%nonassoc NOT
%left PLUS MINUS
%left STAR SLASH
%nonassoc UPLUS UMINUS
%nonassoc UCONREF UMUTREF UDEREF
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
          type'=(Ast.FunctionType{
                     args=(List.map (fun var -> match var with | Ast.Var v -> v.type') a);
                     type'=t});
          block=b;
          pos=(position $startpos $endpos)
    } }

blocks:
  | LBRACE s=list(statements); RBRACE { Ast.Block {stmts=s; pos=(position $startpos $endpos)} }

statements:
  | LET v=seplist(COMMA,vars); EQUAL e=expressions; SEMICOLON { Ast.LetStmt {vars=v; expr=e; pos=(position $startpos $endpos)} }
  | SET x=expressions; SEMICOLON { Ast.SetStmt {label=None; expr=x; pos=(position $startpos $endpos)} }
  | v=seplist(COMMA,expressions) EQUAL e=expressions; SEMICOLON { Ast.AssignStmt {vars=v; expr=e; pos=(position $startpos $endpos)} }
  | RETURN x=expressions; SEMICOLON { Ast.ReturnStmt {expr=x; pos=(position $startpos $endpos)} }
  | x=expressions; SEMICOLON { Ast.InvokeStmt {expr=x; pos=(position $startpos $endpos)} }
  | x=ifstmts; { x }

ifstmts:
  | IF c=expressions; b=blocks; o=option(elsebranch); { Ast.IfStmt {expr=Ast.IfExpr{cond=c; block=b; other=o; is_stmt=true; type'=Ast.NoneType; pos=(position $startpos $endpos)}; pos=(position $startpos $endpos)} }

ifexprs:
  | IF c=expressions; b=blocks; o=option(elsebranch); { Ast.IfExpr {cond=c; block=b; other=o; is_stmt=false; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
elseifexprs:
  | ELSE IF c=expressions; b=blocks; o=option(elsebranch); { Ast.ElseIfExpr {cond=c; block=b; other=o; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
elseexprs:
  | ELSE b=blocks; { Ast.ElseExpr {block=b; type'=Ast.NoneType; pos=(position $startpos $endpos)} }

elsebranch:
  | x=elseifexprs; { x }
  | x=elseexprs; { x }

expressions:
  | LPAREN x=expressions; RPAREN { x }
  | x=blocks; { Ast.BlockExpr {block=x; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=postfix; { x }
  | x=binops; { x }
  | x=unops; { x }

postfix:
  | x=exprs; { x }
  | x=exprs; QUESTION { Ast.UnOpExpr {value=x; op=Ast.TryOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }

binops:
  | x=expressions; PLUS  y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.AddOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; MINUS y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.SubOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; STAR  y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.MulOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; SLASH y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.DivOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=conds; { x }

conds:
  | x=expressions; EQ y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.EqOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; NE y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.NeOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; LE y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.LeOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; GE y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.GeOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; LT y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.LtOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=expressions; GT y=expressions; { Ast.BinOpExpr {lvalue=x; op=Ast.GtOp; rvalue=y; type'=Ast.NoneType; pos=(position $startpos $endpos)} }

unops:
  | PLUS x=expressions; %prec UPLUS { Ast.UnOpExpr {value=x; op=Ast.PosOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | MINUS x=expressions; %prec UMINUS { Ast.UnOpExpr {value=x; op=Ast.NegOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | EXCLAMATION x=expressions; %prec NOT { Ast.UnOpExpr {value=x; op=Ast.NotOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=refs; { x }

refs:
  | AMPERSAND x=expressions; %prec UCONREF { Ast.UnOpExpr {value=x; op=Ast.ConRefOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | AMPERSAND MUT x=expressions; %prec UMUTREF { Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | STAR x=expressions; %prec UMUTREF { Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | CARET x=expressions; %prec UDEREF { Ast.UnOpExpr {value=x; op=Ast.DerefOp; type'=Ast.NoneType; pos=(position $startpos $endpos)} }

exprs:
  | x=entities; { Ast.EntityExpr {value=x; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | x=terminals; { Ast.TerminalExpr {value=x; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | i=id; LPAREN a=seplist(COMMA,expressions); RPAREN { Ast.InvokeExpr {value=i; args=a; type'=Ast.NoneType; pos=(position $startpos $endpos)} }
  | AT x=ifexprs; { x }

terminals:
  | UNDEFINED { Ast.UndefinedVal }
  | LPAREN RPAREN { Ast.UnitVal }
  | x=bools;  { Ast.BoolVal {value=x} }
  | x=INTVAL; { Ast.IntVal {value=x} }
  | x=FLOATVAL; { Ast.FloatVal {value=x} }
  | x=id; { Ast.IdVal {value=x} }
  | DOT LBRACE x=seplist(COMMA,expressions); RBRACE { Ast.StructVal {value=x} }

bools:
  | TRUE { true }
  | FALSE { false }

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
  | BOOL { Ast.BoolType }
  | INT { Ast.IntType }
  | FLOAT { Ast.FloatType }
  | HASH LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
  | STRUCT LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
  | AMPERSAND l=option(life); t=types; { Ast.ConRefType {life=l; types=t} }
  | STAR l=option(life); t=types; { Ast.MutRefType {life=l; types=t} }

life:
  | l=id; BAR { l }

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
