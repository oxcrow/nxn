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

  let pos (loc: Lexing.position * Lexing.position) =
    let startpos, endpos = loc in
    position startpos endpos
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
%token NEW
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
%token I32
%token U32
%token FLOAT
%token NOT
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
%token TILDE
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
%nonassoc UTRY UMOVE UCONREF UMUTREF UDEREF
(* Highest priority at the bottom *)

%start file
%type  <Ast.file> file

%%

file:
    | EOF { Ast.File { entities = []; filename = "_" } }
    | e = nonempty_list(entities) EOF {Ast.File { entities = e; filename = "_" }}

entities:
    | FN i=id; LPAREN a=seplist(COMMA,vars); RPAREN t=return_types; b=blocks;
        { Ast.Function {
            id=i;
            args=a;
            type'=(Ast.FunctionType{
                args=(List.map (fun var -> match var with | Ast.Var v -> v.type' | Ast.NoneVar -> Ast.NoneType) a);
                type'=t});
            block=b;
            pos=(pos $loc)
        } }
     | STRUCT i=id; LBRACE RBRACE { Ast.Struct {id=i; pos=(pos $loc)} }

blocks:
    | LBRACE s=list(statements); RBRACE { Ast.Block {stmts=s; pos=(pos $loc)} }

statements:
    | LET v=seplist(COMMA,vars); EQUAL e=expressions; SEMICOLON
        { Ast.LetStmt {vars=v; expr=e; pos=(pos $loc)} }
    | SET x=expressions; SEMICOLON
        { Ast.SetStmt {label=None; expr=x; pos=(pos $loc)} }
    | v=seplist(COMMA,expressions) EQUAL e=expressions; SEMICOLON
        { Ast.AssignStmt {vars=v; expr=e; pos=(pos $loc)} }
    | RETURN x=expressions; SEMICOLON
        { Ast.ReturnStmt {expr=x; pos=(pos $loc)} }
    | x=expressions; SEMICOLON
        { Ast.InvokeStmt {expr=x; pos=(pos $loc)} }
    | x=blocks;
        { Ast.BlockStmt {block=x; pos=(pos $loc)} }
    | x=ifstmts; { x }

expressions:
     | LPAREN x=expressions; RPAREN { x }
     | DOLLAR x=blocks; { Ast.BlockExpr {block=x; type'=Ast.NoneType; pos=(pos $loc)} }
     | x=postfix; { x }
     | x=binops; { x }
     | x=unops; { x }

postfix:
    | x=exprs; { x }
    | x=postfix; QUESTION %prec UTRY
        { Ast.UnOpExpr {value=x; op=Ast.TryOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=postfix; EXCLAMATION %prec UMOVE
        { Ast.UnOpExpr {value=x; op=Ast.MoveOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=postfix; AMPERSAND %prec UCONREF
        { Ast.UnOpExpr {value=x; op=Ast.ConRefOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=postfix; AMPERSAND MUT %prec UMUTREF
        { Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=postfix; AT %prec UMUTREF
        { Ast.UnOpExpr {value=x; op=Ast.MutRefOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=postfix; HASH %prec UDEREF
        { Ast.UnOpExpr {value=x; op=Ast.DerefOp; type'=Ast.NoneType; pos=(pos $loc)} }

binops:
    | x=expressions; PLUS  y=expressions; %prec PLUS
        { Ast.BinOpExpr {lvalue=x; op=Ast.AddOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; MINUS y=expressions; %prec MINUS
        { Ast.BinOpExpr {lvalue=x; op=Ast.SubOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; STAR  y=expressions; %prec STAR
        { Ast.BinOpExpr {lvalue=x; op=Ast.MulOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; SLASH y=expressions; %prec SLASH
        { Ast.BinOpExpr {lvalue=x; op=Ast.DivOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=conds; { x }

conds:
    | x=expressions; EQ y=expressions; %prec EQ
        { Ast.BinOpExpr {lvalue=x; op=Ast.EqOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; NE y=expressions; %prec NE
        { Ast.BinOpExpr {lvalue=x; op=Ast.NeOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; LE y=expressions; %prec LE
        { Ast.BinOpExpr {lvalue=x; op=Ast.LeOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; GE y=expressions; %prec GE
        { Ast.BinOpExpr {lvalue=x; op=Ast.GeOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; LT y=expressions; %prec LT
        { Ast.BinOpExpr {lvalue=x; op=Ast.LtOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=expressions; GT y=expressions; %prec GT
        { Ast.BinOpExpr {lvalue=x; op=Ast.GtOp; rvalue=y; type'=Ast.NoneType; pos=(pos $loc)} }

unops:
    | PLUS x=expressions; %prec UPLUS
        { Ast.UnOpExpr {value=x; op=Ast.PosOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | MINUS x=expressions; %prec UMINUS
        { Ast.UnOpExpr {value=x; op=Ast.NegOp; type'=Ast.NoneType; pos=(pos $loc)} }
    | NOT x=expressions; %prec NOT
        { Ast.UnOpExpr {value=x; op=Ast.NotOp; type'=Ast.NoneType; pos=(pos $loc)} }

exprs:
    | x=entities;
        { Ast.EntityExpr {value=x; type'=Ast.NoneType; pos=(pos $loc)} }
    | x=terminals;
        { Ast.TerminalExpr {value=x; type'=Ast.NoneType; pos=(pos $loc)} }
    | i=id; LPAREN a=seplist(COMMA,expressions); RPAREN
        { Ast.InvokeExpr {value=i; args=a; type'=Ast.NoneType; pos=(pos $loc)} }
    | DOLLAR x=ifexprs; { x }

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
    | s=state; sh=shadow; i=id; t=typedec; { Ast.Var {id=i; state=s; shadow=sh; type'=t;} }

shadow:
    |  { false }
    | NEW { true }
    | PLUS { true }

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
    | I32 { Ast.IntType }
    | U32 { Ast.IntType }
    | FLOAT { Ast.FloatType }
    | HASH LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
    | STRUCT LBRACE t=separated_nonempty_list(COMMA,types); RBRACE { Ast.StructType {types=t} }
    | AMPERSAND l=option(life); t=types; { Ast.ConRefType {life=l; types=t} }
    | MUT AMPERSAND l=option(life); t=types; { Ast.MutRefType {life=l; types=t} }
    | AT l=option(life); t=types; { Ast.MutRefType {life=l; types=t} }

life:
    | l=id; BAR { l }

id:
    | i=IDVAL; { Ast.Id {value=i; pos=(pos $loc)} }

ifstmts:
    | IF c=expressions; b=blocks; o=option(elsebranch);
        { Ast.IfStmt{expr=Ast.IfExpr{cond=c; block=b; other=o; is_stmt=true; type'=Ast.NoneType; pos=(pos $loc)}; pos=(pos $loc)} }
ifexprs:
    | IF c=expressions; b=blocks; o=option(elsebranch);
        { Ast.IfExpr {cond=c; block=b; other=o; is_stmt=false; type'=Ast.NoneType; pos=(pos $loc)} }
elseifexprs:
    | ELSE IF c=expressions; b=blocks; o=option(elsebranch);
        { Ast.ElseIfExpr {cond=c; block=b; other=o; type'=Ast.NoneType; pos=(pos $loc)} }
elseexprs:
    | ELSE b=blocks; { Ast.ElseExpr {block=b; type'=Ast.NoneType; pos=(pos $loc)} }
elsebranch:
    | x=elseifexprs; { x }
    | x=elseexprs; { x }

seplist(SEP, NODE):
    | { [] }
    | x=NODE; { [x] }
    | x=NODE; SEP y=seplist(SEP,NODE); { x::y }

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
