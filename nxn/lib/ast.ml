(* Silence annoying position/location spam when printing AST. *)
type loc = Loc of { lnum : int; cnum : int }
type pos = Pos of { start : loc; end' : loc }

let show_loc _ = ""
let pp_loc _ _ = ()
let show_pos _ = ""
let pp_pos _ _ = ()

type file = File of { entities : entities list } [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id; args : vars list; type' : types; block : blocks; pos : pos }
  | Struct of int
  | Enum

and blocks = Block of { stmts : statements list; pos : pos }

and statements =
  | LetStmt of { vars : vars list; expr : expressions; pos : pos }
  | SetStmt of { label : id option; expr : expressions; pos : pos }
  | AssignStmt of { vars : expressions list; expr : expressions; pos : pos }
  | ReturnStmt of { expr : expressions; pos : pos }
  | InvokeStmt of { expr : expressions; pos : pos }
  | IfStmt of { expr : expressions; pos : pos }
  | NoneStmt

and expressions =
  | TerminalExpr of { value : terminals; type' : types; pos : pos }
  | InvokeExpr of { value : id; args : expressions list; type' : types; pos : pos }
  | BinOpExpr of {
      lvalue : expressions;
      op : binop;
      rvalue : expressions;
      type' : types;
      pos : pos;
    }
  | UnOpExpr of { value : expressions; op : unop; type' : types; pos : pos }
  | IfExpr of {
      cond : expressions;
      block : blocks;
      other : expressions option;
      is_stmt : bool;
      type' : types;
      pos : pos;
    }
  | ElseIfExpr of {
      cond : expressions;
      block : blocks;
      other : expressions option;
      type' : types;
      pos : pos;
    }
  | ElseExpr of { block : blocks; type' : types; pos : pos }
  | EntityExpr of { value : entities; type' : types; pos : pos }
  | BlockExpr of { block : blocks; type' : types; pos : pos }

and binop =
  | AddOp
  | SubOp
  | MulOp
  | DivOp
  | RemOp
  | ExpOp
  | EqOp
  | NeOp
  | LeOp
  | GeOp
  | LtOp
  | GtOp
  | NoneOp

and unop = PosOp | NegOp | NotOp | TryOp | ConRefOp | MutRefOp | DerefOp

and terminals =
  | UndefinedVal
  | UnitVal
  | BoolVal of { value : bool }
  | IntVal of { value : int }
  | FloatVal of { value : float }
  | IdVal of { value : id }
  | StructVal of { value : expressions list }

and vars = Var of { id : id; state : state; type' : types } | NoneVar

and types =
  | UnitType
  | BoolType
  | IntType
  | FloatType
  | DerivedType of { id : id }
  | FunctionType of { args : types list; type' : types }
  | StructType of { types : types list }
  | ConRefType of { life : id option; types : types }
  | MutRefType of { life : id option; types : types }
  | NoneType

and traits =
  | DefaultTrait
  | DisplayTrait
  | DebugTrait
  | DerefTrait
  | DropTrait
  | CopyTrait
  | CloneTrait
  | SyncTrait
  | SendTrait
  | NoneTrait

and state = ConState | MutState | SetState
and id = Id of { value : string; loc : loc }
