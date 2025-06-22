let location = " (" ^ __LOC__ ^ ")"

module Entity = struct
  let id x =
    match x with
    | NxnAst.Function f -> ( match f.id with Id i -> i.x)
    | _ -> Error.todo @@ "Entity Id." ^ location

  let ty x =
    match x with
    | NxnAst.Function f -> f.typex
    | _ -> Error.todo @@ "Entity type." ^ location
end
