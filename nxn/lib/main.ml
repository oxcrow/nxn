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

(** Infer types *)
let infer ast env =
  (* Recursively infer types

     1. Iterate through all functions
     2. Iterate through all statements inside each function
     3. Iterate through all expressions inside each statement
     4. Infer types for all expressions then store them in AST

    In the end we will create a statically typed AST where every
    statement and expression's type is resolved without ambiguity.

    Note: However note that the types will not be checked until later.
    For example, this function won't check if the returned statement
    has the same type as is expected by function's return type.
    It won't check the types passsed as function arguments. etc.

    I don't know if that is a good or bad idea.
  *)
  let rec infer_expr_type expr env =
    let type' =
      match expr with
      | NxnAst.TerminalExpr term -> (
          match term.value with
          | NxnAst.UnitVal -> NxnAst.UnitType
          | NxnAst.IntVal _ -> NxnAst.IntType
          | NxnAst.FloatVal _ -> NxnAst.FloatType
          | NxnAst.IdVal id ->
              let id = NxnAst.Get.id id.value in
              let type' =
                match Env.Get.File.var_type id env with
                | Some type' -> type'
                | None ->
                    failwith @@ "Identifier"
                    ^ Util.quote_space_of_string id
                    ^ "does not exist in environment."
                    ^ Util.paren_group_of_string Error.loc
              in
              type'
          | NxnAst.TupleVal tuple ->
              let expr_type =
                List.map (fun expr -> infer_expr_type expr env) tuple.value
              in
              let type' = NxnAst.TupleType { value = expr_type } in
              type')
      | NxnAst.InvokeExpr invoke ->
          let id = NxnAst.Get.id invoke.value in
          let type' =
            match Env.Get.File.function_type id env with
            | Some type' -> type'
            | None ->
                failwith @@ "Identifier"
                ^ Util.quote_space_of_string id
                ^ "does not exist in environment."
                ^ Util.paren_group_of_string Error.loc
          in
          type'
    in
    type'
  in

  let rec infer_expr expr env =
    let type', expr =
      match expr with
      | NxnAst.TerminalExpr term -> (
          match term.value with
          | NxnAst.UnitVal ->
              let type' = infer_expr_type expr env in
              let expr = NxnAst.Set.Expr.with_type expr type' in
              (type', expr)
          | NxnAst.IntVal _ ->
              let type' = infer_expr_type expr env in
              let expr = NxnAst.Set.Expr.with_type expr type' in
              (type', expr)
          | NxnAst.FloatVal _ ->
              let type' = infer_expr_type expr env in
              let expr = NxnAst.Set.Expr.with_type expr type' in
              (type', expr)
          | NxnAst.IdVal _ ->
              let type' = infer_expr_type expr env in
              let expr = NxnAst.Set.Expr.with_type expr type' in
              (type', expr)
          | NxnAst.TupleVal tuple ->
              let infer_tuple_values values =
                let rec aux values acc =
                  match values with
                  | [] -> List.rev acc
                  | hd :: tl ->
                      let _type', expr = infer_expr hd env in
                      let acc = expr :: acc in
                      aux tl acc
                in
                aux values []
              in
              let infer_tuple_types values =
                let rec aux values acc =
                  match values with
                  | [] -> List.rev acc
                  | hd :: tl ->
                      let type' = NxnAst.Get.Expr.type' hd in
                      let acc = type' :: acc in
                      aux tl acc
                in
                aux values []
              in
              let values = infer_tuple_values tuple.value in
              let types = infer_tuple_types values in
              let type' = NxnAst.TupleType { value = types } in
              let expr =
                NxnAst.TerminalExpr
                  { value = NxnAst.TupleVal { value = values }; type' }
              in
              (type', expr))
      | NxnAst.InvokeExpr _ ->
          let type' = infer_expr_type expr env in
          let expr = NxnAst.Set.Expr.with_type expr type' in
          (type', expr)
    in
    (* write @@ NxnAst.show_expressions expr ^ " -> " ^ NxnAst.show_types type'; *)
    (type', expr)
  in

  let infer_statements stmts env =
    let infer_stmt env stmt =
      match stmt with
      | NxnAst.LetStmt ls ->
          (* Infer variable types *)
          let rec infer_vars vars type' =
            let vars =
              match vars with
              | NxnAst.Var v -> NxnAst.Var { id = v.id; type' }
              | NxnAst.TupleVar v ->
                  let vars =
                    List.map
                      (fun (var, type') -> infer_vars var type')
                      (List.combine v.var
                         (match type' with
                         | TupleType t -> t.value
                         | _ -> Error.never "TupleVar's type is not TupleType."))
                  in
                  NxnAst.TupleVar { var = vars }
            in
            vars
          in
          (* Extend environment with new variables *)
          let rec extend_env env vars =
            let env =
              match vars with
              | NxnAst.Var v ->
                  let id = NxnAst.Get.id v.id in
                  let type' = v.type' in
                  Env.Add.File.var_type id type' env
              | NxnAst.TupleVar v ->
                  let rec aux env var =
                    match var with
                    | [] -> env
                    | hd :: tl ->
                        let env = extend_env env hd in
                        aux env tl
                  in
                  aux env v.var
            in
            env
          in
          let type', expr = infer_expr ls.expr env in
          let vars = infer_vars ls.var type' in
          let stmt = NxnAst.Set.Stmt.with_expr stmt expr in
          let stmt = NxnAst.Set.Stmt.with_var stmt vars in
          let env = extend_env env vars in
          Some (env, stmt)
      | NxnAst.ReturnStmt rs ->
          let _, expr = infer_expr rs.expr env in
          let stmt = NxnAst.Set.Stmt.with_expr stmt expr in
          Some (env, stmt)
      | NxnAst.ReturnExprStmt rs ->
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

(** Type check the High level AST *)
let check ast =
  (* Recursively check types

     1. Iterate through all functions
     2. Iterate through all statements inside each function
     3. Check types for functions, structs, variables, etc.

    In the end we will have a completely type checked AST.
  *)
  let check_return_stmts stmts type' =
    List.iter
      (fun stmt ->
        match stmt with
        | NxnAst.ReturnStmt _ ->
            if NxnAst.Get.Stmt.type' stmt <> type' then
              failwith @@ "Function return type check failed." ^ Error.loc
        | NxnAst.ReturnExprStmt _ ->
            if NxnAst.Get.Stmt.type' stmt <> type' then
              failwith @@ "Function return type check failed." ^ Error.loc
        | _ -> unit)
      stmts
  in

  let check_entities entities =
    let check entity =
      match entity with
      | NxnAst.Function f -> (
          match f.block with
          | NxnAst.Block b -> check_return_stmts b.stmts f.type')
      | _ -> Error.todo @@ "Check entity type." ^ Error.loc
    in
    List.iter check entities
  in

  let _ = match ast with NxnAst.File entities -> check_entities entities in

  ast

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
  let hir = infer ast env |> check in

  printast hir;
  write "+";
  unit
