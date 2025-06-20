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
  | LetStmt of { id : id; expr : expressions }
  | ReturnStmt of { expr : expressions }

and expressions =
  | TerminalExpr of { value : terminals }
  | InvokeExpr of { value : id }

and terminals = IntVal of { value : int } | IdVal of { value : id }
and types = UnitType | IntType | DerivedType of { id : id }
and id = Id of { value : string; loc : loc }
