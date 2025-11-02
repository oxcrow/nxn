module Id = struct
  let value x = match x with Ast.Id y -> y.value
  let loc x = match x with Ast.Id y -> y.loc
end

module Type = struct end

module Loc = struct
  let line_num x = match x with Ast.Loc y -> y.lnum
  let column_num x = match x with Ast.Loc y -> y.cnum
end

module Expr = struct
  let type' x =
    match x with
    | Ast.TerminalExpr y -> y.type'
    | Ast.InvokeExpr y -> y.type'
    | Ast.BinOpExpr y -> y.type'
    | Ast.UnOpExpr y -> y.type'
    | Ast.IfExpr y -> y.type'
    | Ast.ElseIfExpr y -> y.type'
    | Ast.ElseExpr y -> y.type'
    | Ast.BlockExpr y -> y.type'
    | _ -> failwith @@ "Implement method to get expresison type."
  ;;

  let pos x =
    match x with
    | Ast.TerminalExpr y -> y.pos
    | Ast.InvokeExpr y -> y.pos
    | Ast.BinOpExpr y -> y.pos
    | Ast.UnOpExpr y -> y.pos
    | Ast.IfExpr y -> y.pos
    | Ast.ElseIfExpr y -> y.pos
    | Ast.ElseExpr y -> y.pos
    | Ast.BlockExpr y -> y.pos
    | _ -> failwith @@ "Implement method to get expresison type."
  ;;
end

module Stmt = struct
  let vars x = match x with Ast.LetStmt y -> y.vars
  let type' x = match x with Ast.SetStmt y -> Expr.type' y.expr
end
