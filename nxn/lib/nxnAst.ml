type loc = Loc of { lnum : int; cnum : int }

(* Silence annoying Location output when printing AST. *)
let show_loc _ = ""
let pp_loc _ _ = ()

type file = File of entities list [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id; typex : types; block : blocks }
  | Struct
  | Enum
[@@deriving show { with_path = false }]

and blocks = Block of { stmts : statements list }
[@@deriving show { with_path = false }]

and statements =
  | LetStmt of { id : id; expr : expressions }
  | ReturnStmt of { expr : expressions }
[@@deriving show { with_path = false }]

and expressions = TerminalExpr of { x : terminals } | InvokeExpr of { x : id }
[@@deriving show { with_path = false }]

and terminals = IntVal of { x : int } | IdVal of { x : id }
[@@deriving show { with_path = false }]

and types = TypeUnit | TypeInt | TypeDerived of { id : id }
[@@deriving show { with_path = false }]

and id = Id of { x : string; loc : loc }
[@@deriving show { with_path = false }]
