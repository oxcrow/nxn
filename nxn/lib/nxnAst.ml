type loc = Loc of { lnum : int; cnum : int }

(* Silence annoying Location output when printing AST. *)
let show_loc _ = ""
let pp_loc _ _ = ()

type file = File of entities list [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id; typex : types; block : blocks }
  | Struct
  | Enum

and blocks = Block of { stmts : statements list }

and statements =
  | LetStmt of { vars : vars list; expr : expressions }
  | ReturnStmt of { expr : expressions }

and expressions =
  | TerminalExpr of { value : terminals }
  | InvokeExpr of { value : id }

and terminals =
  | UnitVal
  | IntVal of { value : int }
  | FloatVal of { value : float }
  | IdVal of { value : id }

and vars = Var of { id : id; state : state; type' : types option }

and types =
  | UnitType
  | IntType
  | FloatType
  | DerivedType of { id : id }
  | NoneType

and state = ConstantState | MutableState | VariableState
and id = Id of { value : string; loc : loc }
