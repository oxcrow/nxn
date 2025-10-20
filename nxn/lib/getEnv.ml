module File = struct
  let function_type id env =
    match env with
    | Env.File x ->
        let type' = List.assoc_opt id x.functions in
        type'
  ;;

  let var_type id env =
    match env with
    | Env.File x ->
        let type' = List.assoc_opt id x.vars in
        type'
  ;;

  let num_functions env = match env with Env.File x -> List.length x.functions
  let num_structs env = match env with Env.File x -> List.length x.structs
  let num_enums env = match env with Env.File x -> List.length x.enums
  let num_vars env = match env with Env.File x -> List.length x.vars
end
