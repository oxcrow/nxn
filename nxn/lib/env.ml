type vars = Var of { type' : Ast.types; shadow : bool }

type file =
  | File of {
      name : string;
      functions : (string * Ast.types) list;
      structs : (string * Ast.types) list;
      enums : (string * Ast.types) list;
      vars : (string * Ast.types) list;
      varx : (string * vars) list;
    }

type module' = Module of { name : string; files : file list }
type library = Library of { name : string; modules : module' list }

module Add = struct
  module File = struct
    let function_type id type' env =
      match env with
      | File x ->
          File
            {
              name = x.name;
              functions = (id, type') :: x.functions;
              structs = x.structs;
              enums = x.enums;
              vars = x.vars;
              varx = x.varx;
            }
    ;;

    let var_type id type' env =
      match env with
      | File x ->
          File
            {
              name = x.name;
              functions = x.functions;
              structs = x.structs;
              enums = x.enums;
              vars = (id, type') :: x.vars;
              varx = x.varx;
            }
    ;;

    let var id var env =
      match env with
      | File x ->
          File
            {
              name = x.name;
              functions = x.functions;
              structs = x.structs;
              enums = x.enums;
              vars = x.vars;
              varx = (id, var) :: x.varx;
            }
    ;;
  end
end
