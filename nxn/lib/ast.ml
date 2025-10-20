type file = File of { entities : entities list } [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id; type' : types; block : blocks }
  | Struct of int
  | Enum

and blocks = Block of { stmts : statements list }

and statements =
  | LetStmt of { vars : vars list; expr : expressions }
  | ReturnStmt of { expr : expressions }
  | NoneStmt

and expressions =
  | EntityExpr of { value : entities; type' : types }
  | TerminalExpr of { value : terminals; type' : types }
  | InvokeExpr of { value : id; type' : types }

and terminals =
  | UnitVal
  | IntVal of { value : int }
  | FloatVal of { value : float }
  | IdVal of { value : id }
  | StructVal of { value : expressions list }

and vars = Var of { id : id; state : state; type' : types } | NoneVar

and types =
  | UnitType
  | IntType
  | FloatType
  | DerivedType of { id : id }
  | StructType of { types : types list }
  | NoneType

and state = ImmutableState | MutableState | AssignState
and id = Id of { value : string; loc : loc }
and loc = Loc of { lnum : int; cnum : int }
