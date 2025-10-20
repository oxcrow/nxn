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

module Get = struct
  module File = struct
    let function_type id env =
      match env with
      | File x ->
          let type' = List.assoc_opt id x.functions in
          type'
    ;;

    let var_type id env =
      match env with
      | File x ->
          let type' = List.assoc_opt id x.vars in
          type'
    ;;

    let num_functions env = match env with File x -> List.length x.functions
    let num_structs env = match env with File x -> List.length x.structs
    let num_enums env = match env with File x -> List.length x.enums
    let num_vars env = match env with File x -> List.length x.vars
  end
end

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
