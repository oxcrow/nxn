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
  let type' x = match x with Ast.TerminalExpr y -> y.type'
end

module Stmt = struct
  let vars x = match x with Ast.LetStmt y -> y.vars
  let type' x = match x with Ast.SetStmt y -> Expr.type' y.expr
end
