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
  | TerminalExpr of { value : terminals; type' : types }
  | InvokeExpr of { value : id; type' : types }
[@@deriving show { with_path = false }]

and terminals =
  | IntVal of { value : int }
  | FloatVal of { value : float }
  | IdVal of { value : id }
[@@deriving show { with_path = false }]

and types =
  | TypeUnit
  | TypeInt
  | TypeFloat
  | TypeDerived of { id : id }
  | TypeNone
[@@deriving show { with_path = false }]

and id = Id of { value : string; loc : loc }
[@@deriving show { with_path = false }]

module Get = struct
  module Entity = struct
    let id x =
      match x with
      | Function f -> ( match f.id with Id i -> i.value)
      | _ -> Error.todo @@ "Entity Id." ^ Error.loc

    let type' x =
      match x with
      | Function f -> f.type'
      | _ -> Error.todo @@ "Entity type." ^ Error.loc
  end

  module Expr = struct
    let type' x =
      match x with TerminalExpr e -> e.type' | InvokeExpr e -> e.type'
  end

  module Stmt = struct
    let type' x =
      match x with
      | ReturnStmt s -> Expr.type' s.expr
      | _ -> Error.todo @@ "Stmt type." ^ Error.loc
  end

  module Id = struct
    let id x = match x with Id i -> i.value
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
      | TerminalExpr y -> TerminalExpr { value = y.value; type' = t }
      | InvokeExpr y -> InvokeExpr { value = y.value; type' = t }
  end
end
