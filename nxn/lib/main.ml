open Error

let unit = ()
let write = print_endline
let location = " (" ^ __LOC__ ^ ")"

(** Parse code and create AST *)
let parse (code : string) =
  let buf = Lexing.from_string code in
  let ast =
    try NxnParser.file NxnLexer.token buf
    with NxnParser.Error ->
      let lnum = buf.lex_curr_p.pos_lnum in
      let cnum = buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol in
      let pos =
        "(Line: " ^ string_of_int lnum ^ ", Column: " ^ string_of_int cnum ^ ")"
      in
      raise @@ Error ("Parser failed! Around position: " ^ pos)
  in
  ast

(** Create environment *)
let envir ast =
  let open NxnAst in
  (* Create Environment with entities (function, struct, etc.)

    1. Iterate through the top level entities to find their id and type
    2. Store the entities' id and type in an association list
    3. Create an environment using the association list

    Validation:
    1. Ensure that there are no duplicate entities with the same name
  *)

  (* Create environment list *)
  let envlist entities =
    List.map
      (fun entity ->
        match entity with
        | Function _ ->
            let id = Ast.Entity.id entity in
            let ty = Ast.Entity.ty entity in
            (id, ty)
        | _ -> todo @@ "Environment for entity." ^ location)
      entities
  in

  (* Validate environment *)
  let validate env =
    let rec count id n = function
      | [] -> n
      | [ (x, _) ] -> if x = id then n + 1 else n
      | (x, _) :: t -> if x = id then count id (n + 1) t else count id n t
    in
    let counts = List.map (fun (id, _) -> count id 0 env) env in
    (* We should report which identifier has multiple ocurrances *)
    List.iter
      (fun n -> if n > 1 then failwith "Multiple occurances of identifier.")
      counts;
    env
  in

  let env = match ast with File entities -> envlist entities |> validate in
  env

(** Print AST for debugging. *)
let printast ast = write ("+ " ^ NxnAst.show_file ast ^ "\n")

(** Execution starts here *)
let main =
  let code = File.read_file_content "x.nxn" in
  let ast = parse code in
  let env = envir ast in

  ignore env;
  write "-*-";
  unit
