type file = File of entities list [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id option; type' : types; block : blocks }
  | Struct
  | Enum

and blocks = Block of { stmts : statements list }

and statements =
  | LetStmt of { vars : vars list; expr : expressions }
  | ReturnStmt of { expr : expressions }

and expressions =
  | EntityExpr of { value : entities }
  | TerminalExpr of { value : terminals }
  | InvokeExpr of { value : id }

and terminals =
  | UnitVal
  | IntVal of { value : int }
  | FloatVal of { value : float }
  | IdVal of { value : id }

and vars = Var of { id : id; state : state; type' : types option } | NoneVar
and types = UnitType | IntType | FloatType | DerivedType of { id : id } | NoneType
and state = ImmutableState | MutableState | AssignState
and id = Id of { value : string; loc : loc }
and loc = Loc of { lnum : int; cnum : int }
