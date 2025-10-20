(* Location *)
external loc : string = "%loc_LOC"

module Stmt = struct
  let with_expr x e =
    match x with
    | Ast.LetStmt y -> Ast.LetStmt { vars = y.vars; expr = e }
    | Ast.ReturnStmt _ -> Ast.ReturnStmt { expr = e }
  ;;

  let with_vars x v =
    match x with
    | Ast.LetStmt y -> Ast.LetStmt { vars = v; expr = y.expr }
    | _ -> Error.Failure.never loc "Never use with anything else except let statements"
  ;;
end

module Expr = struct
  let with_type x t =
    match x with
    | Ast.TerminalExpr y -> Ast.TerminalExpr { value = y.value; type' = t }
    | Ast.InvokeExpr y -> Ast.InvokeExpr { value = y.value; type' = t }
  ;;
end
