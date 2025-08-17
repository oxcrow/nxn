module Id = struct
  let value x = match x with Ast.Id y -> y.value
  let loc x = match x with Ast.Id y -> y.loc
end

module Loc = struct
  let line_num x = match x with Ast.Loc y -> y.lnum
  let column_num x = match x with Ast.Loc y -> y.cnum
end
