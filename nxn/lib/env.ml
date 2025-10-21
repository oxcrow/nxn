type record = Record of { id : string; type' : Ast.types }

type file =
  | File of {
      name : string;
      functions : (string * Ast.types) list;
      structs : (string * Ast.types) list;
      enums : (string * Ast.types) list;
      vars : (string * Ast.types) list;
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
            }
    ;;
  end
end
