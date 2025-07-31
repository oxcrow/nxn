type loc = Loc of { lnum : int; cnum : int }

(* Silence annoying Location output when printing AST. *)
let show_loc _ = ""
let pp_loc _ _ = ()

type file = File of entities list [@@deriving show { with_path = false }]

and entities =
  | Function of { id : id; type' : types; block : blocks }
  | Struct
  | Enum

and blocks = Block of { stmts : statements list }

and statements =
  | LetStmt of { var : var; expr : expressions }
  | ReturnStmt of { expr : expressions }
  | ReturnExprStmt of { expr : expressions }

and expressions =
  | TerminalExpr of { value : terminals; type' : types }
  | InvokeExpr of { value : id; type' : types }

and var = Var of { id : id; type' : types } | TupleVar of { var : var list }

and terminals =
  | UnitVal
  | IntVal of { value : int }
  | FloatVal of { value : float }
  | IdVal of { value : id }
  | TupleVal of { value : expressions list }

and types =
  | UnitType
  | IntType
  | FloatType
  | DerivedType of { id : id }
  | TupleType of { value : types list }
  | NoneType

and id = Id of { value : string; loc : loc }

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
      | ReturnExprStmt s -> Expr.type' s.expr
      | _ -> Error.todo @@ "Stmt type." ^ Error.loc
  end

  module Var = struct
    let id x =
      match x with
      | Var v -> v.id
      | _ -> Error.todo @@ "Variable id." ^ Error.loc

    let ids x = List.map (fun y -> id y) x

    let id_str x =
      match x with
      | Var v -> ( match v.id with Id i -> i.value)
      | _ -> Error.todo @@ "Variable id." ^ Error.loc

    let type' x =
      match x with
      | Var v -> v.type'
      | _ -> Error.todo @@ "Variable id." ^ Error.loc
  end

  module Id = struct
    let id x = match x with Id i -> i.value
  end

  let ids x = Var.ids x
  let id x = Id.id x
end

module Set = struct
  module Stmt = struct
    let with_expr x e =
      match x with
      | LetStmt y -> LetStmt { var = y.var; expr = e }
      | ReturnStmt _ -> ReturnStmt { expr = e }
      | ReturnExprStmt _ -> ReturnExprStmt { expr = e }

    let with_var x v =
      match x with
      | LetStmt y -> LetStmt { var = v; expr = y.expr }
      | _ -> Error.never "Never use with anything else except let statements"
  end

  module Expr = struct
    let with_type x t =
      match x with
      | TerminalExpr y -> TerminalExpr { value = y.value; type' = t }
      | InvokeExpr y -> InvokeExpr { value = y.value; type' = t }
  end
end
