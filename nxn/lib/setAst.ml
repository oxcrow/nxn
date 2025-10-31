(* Location *)
external loc : string = "%loc_LOC"

module Stmt = struct
  let with_expr x e =
    match x with
    | Ast.LetStmt y -> Ast.LetStmt { vars = y.vars; expr = e }
    | Ast.SetStmt y -> Ast.SetStmt { label = y.label; expr = e }
    | Ast.ReturnStmt _ -> Ast.ReturnStmt { expr = e }
    | Ast.InvokeStmt _ -> Ast.InvokeStmt { expr = e }
    | Ast.IfStmt _ -> Ast.IfStmt { expr = e }
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
    | Ast.InvokeExpr y -> Ast.InvokeExpr { value = y.value; args = y.args; type' = t }
    | Ast.BinOpExpr y ->
        Ast.BinOpExpr { lvalue = y.lvalue; op = y.op; rvalue = y.rvalue; type' = t }
    | Ast.UnOpExpr y -> Ast.UnOpExpr { value = y.value; op = y.op; type' = t }
  ;;

  let with_args x a =
    match x with
    | Ast.InvokeExpr y -> Ast.InvokeExpr { value = y.value; args = a; type' = y.type' }
    | _ -> Error.Failure.never loc "Never use with anything else except invoke exprs"
  ;;
end
