type file = File of { entities : entities list } [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id; args : vars list; type' : types; block : blocks }
  | Struct of int
  | Enum

and blocks = Block of { stmts : statements list }

and statements =
  | LetStmt of { vars : vars list; expr : expressions }
  | SetStmt of { label : id option; expr : expressions }
  | AssignStmt of { vars : expressions list; expr : expressions }
  | ReturnStmt of { expr : expressions }
  | InvokeStmt of { expr : expressions }
  | IfStmt of { expr : expressions }
  | NoneStmt

and expressions =
  | TerminalExpr of { value : terminals; type' : types }
  | InvokeExpr of { value : id; args : expressions list; type' : types }
  | BinOpExpr of { lvalue : expressions; op : binop; rvalue : expressions; type' : types }
  | UnOpExpr of { value : expressions; op : unop; type' : types }
  | IfExpr of {
      cond : expressions;
      block : blocks;
      other : expressions option;
      type' : types;
    }
  | ElseIfExpr of {
      cond : expressions;
      block : blocks;
      other : expressions option;
      type' : types;
    }
  | ElseExpr of { block : blocks; type' : types }
  | EntityExpr of { value : entities; type' : types }
  | BlockExpr of { block : blocks; type' : types }

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
and loc = Loc of { lnum : int; cnum : int }
