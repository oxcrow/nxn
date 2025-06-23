type loc = Loc of { lnum : int; cnum : int }

(* Silence annoying Location output when printing AST. *)
let show_loc _ = ""
let pp_loc _ _ = ()

type file = File of entities list [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id; type' : types; block : blocks }
  | Struct
  | Enum
[@@deriving show { with_path = false }]

and blocks = Block of { stmts : statements list }
[@@deriving show { with_path = false }]

and statements =
  | LetStmt of { id : id; expr : expressions }
  | ReturnStmt of { expr : expressions }
[@@deriving show { with_path = false }]

and expressions =
  | TerminalExpr of { x : terminals; type' : types }
  | InvokeExpr of { x : id; type' : types }
[@@deriving show { with_path = false }]

and terminals = IntVal of { x : int } | IdVal of { x : id }
[@@deriving show { with_path = false }]

and types = TypeUnit | TypeInt | TypeDerived of { id : id } | TypeNone
[@@deriving show { with_path = false }]

and id = Id of { x : string; loc : loc }
[@@deriving show { with_path = false }]

module Get = struct
  module Entity = struct
    let id x =
      match x with
      | Function f -> ( match f.id with Id i -> i.x)
      | _ -> Error.todo @@ "Entity Id." ^ Error.loc

    let ty x =
      match x with
      | Function f -> f.type'
      | _ -> Error.todo @@ "Entity type." ^ Error.loc
  end

  module Id = struct
    let id x = match x with Id i -> i.x
  end

  let id x = Id.id x
end

module Set = struct
  module Stmt = struct
    let with_expr x e =
      match x with
      | LetStmt y -> LetStmt { id = y.id; expr = e }
      | ReturnStmt _ -> ReturnStmt { expr = e }
  end

  module Expr = struct
    let with_type x t =
      match x with
      | TerminalExpr y -> TerminalExpr { x = y.x; type' = t }
      | InvokeExpr y -> InvokeExpr { x = y.x; type' = t }
  end
end
