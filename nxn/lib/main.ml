let unit = ()
let write = print_endline

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
      failwith ("Parser failed! Around position: " ^ pos)
  in
  ast

(** Create environment *)
let envir ast =
  (* Create Environment with entities (function, struct, etc.)

    1. Iterate through the top level entities to find their id and type
    2. Store the entities' id and type in an association list
    3. Create an environment using the association list

    Validation:
    1. Ensure that there are no duplicate entities with the same name
  *)

  (* Create environment record list *)
  let envlist entities =
    List.map
      (fun entity ->
        match entity with
        | NxnAst.Function _ ->
            let id = NxnAst.Get.Entity.id entity in
            let type' = NxnAst.Get.Entity.type' entity in
            (id, type')
        | _ -> Error.todo @@ "Environment for entity." ^ Error.loc)
      entities
  in

  (* Validate environment records *)
  let validate records =
    let rec count id n = function
      | [] -> n
      | (x, _) :: tl -> if x = id then count id (n + 1) tl else count id n tl
    in
    let counts = List.map (fun (id, _) -> count id 0 records) records in
    (* We should report which identifier has multiple ocurrances *)
    List.iter
      (fun n -> if n > 1 then failwith "Multiple occurances of identifier.")
      counts;
    records
  in

  let functions =
    match ast with NxnAst.File entities -> envlist entities |> validate
  in
  let env =
    Env.File
      { name = "main.nxn"; functions; structs = []; enums = []; vars = [] }
  in

  env

(** Infer and check types *)
let infer ast env =
  (* Recursively  infer and check types

     1. Iterate through all functions
     2. Iterate through all statements inside each function
     3. Iterate through all expressions inside each statement
     4. Infer types for all expressions then store them in AST

    In the end we will create a statically typed AST where every
    statement and expression's type is resolved without ambiguity.
  *)
  let infer_expr expr env =
    let type', expr =
      match expr with
      | NxnAst.TerminalExpr t -> (
          match t.x with
          | NxnAst.IntVal _ ->
              let type' = NxnAst.TypeInt in
              (type', NxnAst.Set.Expr.with_type expr type')
          | NxnAst.IdVal i ->
              let id = NxnAst.Get.id i.x in
              let type' = Env.Get.File.var_type id env |> Error.some in
              (type', NxnAst.Set.Expr.with_type expr type'))
      | NxnAst.InvokeExpr i ->
          let id = NxnAst.Get.id i.x in
          let type' = Env.Get.File.function_type id env |> Error.some in
          (type', NxnAst.Set.Expr.with_type expr type')
    in
    (* write @@ NxnAst.show_expressions expr ^ " -> " ^ NxnAst.show_types type'; *)
    (type', expr)
  in

  let infer_statements stmts env =
    let infer_stmt env stmt =
      match stmt with
      | NxnAst.LetStmt ls ->
          let id = NxnAst.Get.id ls.id in
          let type', expr = infer_expr ls.expr env in
          let stmt = NxnAst.Set.Stmt.with_expr stmt expr in
          let env = Env.Add.File.var_type id type' env in
          Some (env, stmt)
      | NxnAst.ReturnStmt rs ->
          let _, expr = infer_expr rs.expr env in
          let stmt = NxnAst.Set.Stmt.with_expr stmt expr in
          Some (env, stmt)
    in

    let infer_stmts env stmts =
      let rec aux env acc = function
        | [] -> List.rev acc
        | hd :: tl ->
            let env, hd = infer_stmt env hd |> Error.some in
            aux env (hd :: acc) tl
      in
      aux env [] stmts
    in

    let stmts = infer_stmts env stmts in
    stmts
  in

  let infer_entities entities env =
    let entities =
      List.map
        (fun entity ->
          match entity with
          | NxnAst.Function f ->
              let block =
                match f.block with
                | NxnAst.Block b ->
                    let stmts = infer_statements b.stmts env in
                    let block = NxnAst.Block { stmts } in
                    block
              in
              NxnAst.Function { id = f.id; type' = f.type'; block }
          | _ -> Error.todo @@ "Infer entity type." ^ Error.loc)
        entities
    in
    entities
  in

  let hir =
    match ast with
    | NxnAst.File entities -> NxnAst.File (infer_entities entities env)
  in

  hir

(** Lower / desugar the High level AST to C99 compatible AST. *)
let lower ast env =
  ignore ast;
  ignore env;
  unit

(** Print AST for debugging. *)
let printast ast = write ("+ " ^ NxnAst.show_file ast ^ "\n")

(** Execution starts here *)
let main =
  let code = File.read_file_content "x.nxn" in
  let ast = parse code in
  let env = envir ast in
  let hir = infer ast env in

  printast hir;
  write "+";
  unit
