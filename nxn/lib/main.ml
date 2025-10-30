open Error.Failure
open Error.Unwrap
open Utils.String
open Utils.Tuple

let unit = ()
let write = print_endline

(* Location *)
external loc : string = "%loc_LOC"

(** Print AST for debugging. *)
let printast ast = write ("+ " ^ Ast.show_file ast ^ "\n")

(** Parse nxn code and create AST *)
let parse (code : string) =
  let buf = Lexing.from_string code in
  let error_pos (buf : Lexing.lexbuf) =
    let lnum = buf.lex_curr_p.pos_lnum in
    let cnum = buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol in
    let pos =
      "Around approximate position: " ^ "(Line: " ^ string_of_int lnum ^ ", Column: "
      ^ string_of_int cnum ^ ")"
    in
    pos
  in
  let ast =
    try NxnParser.file NxnLexer.token buf with
    | NxnParser.Error -> failwith ("Parser error: " ^ error_pos buf)
    | Failure msg -> failwith ("Parser error: " ^ error_pos buf ^ ", With message: " ^ msg)
  in
  ast
;;

(** Type infer AST nodes *)
let infer ast =
  (* Create environment *)
  let envir ast =
    (* Create environment record list of functions *)
    let envfns entities =
      List.map
        (fun entity ->
          match entity with
          | Ast.Function f ->
              let id = GetAst.Id.value f.id in
              let type' = f.type' in
              (id, type')
          | _ -> never loc "Only functions are allowed.")
        (List.filter
           (fun e -> match e with Ast.Function _ -> true | _ -> false)
           entities)
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
        (fun n -> if n > 1 then never loc "Multiple occurances of stupid identifier.")
        counts;
      records
    in

    let functions = match ast with Ast.File f -> envfns f.entities |> validate in

    let env =
      Env.File { name = "x.nxn"; functions; structs = []; enums = []; vars = [] }
    in
    env
  in

  (* Extend environment with new variables *)
  let rec extend_env env vars =
    let extend env var =
      match var with
      | Ast.Var v ->
          let id = GetAst.Id.value v.id in
          let type' = v.type' in
          Env.Add.File.var_type id type' env
    in
    match vars with
    | [] -> env
    | hd :: tl ->
        let env = extend env hd in
        extend_env env tl
  in

  let rec infer_expr_type env expr =
    let type' =
      match expr with
      | Ast.TerminalExpr term -> (
          match term.value with
          | Ast.UndefinedVal -> Ast.NoneType
          | Ast.UnitVal -> Ast.UnitType
          | Ast.IntVal _ -> Ast.IntType
          | Ast.FloatVal _ -> Ast.FloatType
          | Ast.IdVal o ->
              let id = GetAst.Id.value o.value in
              let type' =
                match GetEnv.File.var_type id env with
                | Some type' -> type'
                | None -> error loc (quote id ^ " does not exist in environment.")
              in
              type'
          | Ast.StructVal o ->
              let types = List.map (infer_expr_type env) o.value in
              let type' = Ast.StructType { types } in
              type')
      | Ast.InvokeExpr o ->
          let id = GetAst.Id.value o.value in
          let type', argtypes =
            match GetEnv.File.function_type id env with
            | Some type' -> (
                match type' with
                | Ast.FunctionType t -> (t.type', t.args)
                | _ -> never loc "")
            | None -> error loc (quote id ^ " does not exist in environment.")
          in
          (* Validate that the function argument types match *)
          let _ =
            if
              List.map (fun arg -> infer_expr_type env arg) o.args
              <> List.map
                   (fun argtype ->
                     match argtype with
                     | Ast.ConRefType t -> Ast.ConRefType { life = None; types = t.types }
                     | Ast.MutRefType t -> Ast.MutRefType { life = None; types = t.types }
                     | _ -> argtype)
                   argtypes
            then never loc "Function argument types don't match"
          in
          type'
      | Ast.BinOpExpr o ->
          (* BUG: Binary expression tests fail in cases like if n == 0 {}
             because n is of type IdVal, and 0 is IntVal, thus their types don't match.
             To fix this we need to type infer unary/binary expressions without errors. *)
          let ltype = infer_expr_type env o.lvalue in
          let rtype = infer_expr_type env o.rvalue in
          let type' =
            if ltype = rtype then ltype else error loc "Binary operator types mismatch."
          in
          type'
      | Ast.UnOpExpr o ->
          let value_type = infer_expr_type env o.value in
          let type' =
            match o.op with
            | Ast.PosOp -> value_type
            | Ast.NegOp -> value_type
            | Ast.NotOp -> value_type
            | Ast.ConRefOp -> Ast.ConRefType { life = None; types = value_type }
            | Ast.MutRefOp -> Ast.MutRefType { life = None; types = value_type }
            | Ast.DerefOp -> value_type
            | _ -> todo loc "Infer unary expression type"
          in
          type'
    in
    type'
  in

  let rec infer_expr env expr =
    let type', expr =
      match expr with
      | Ast.TerminalExpr term -> (
          match term.value with
          | Ast.UndefinedVal ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.UnitVal ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.IntVal _ ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.FloatVal _ ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.IdVal _ ->
              let type' = infer_expr_type env expr in
              let expr = SetAst.Expr.with_type expr type' in
              (type', expr)
          | Ast.StructVal o ->
              let result = List.map (infer_expr env) o.value in
              let types = List.map first result in
              let exprs = List.map second result in
              let value = Ast.StructVal { value = exprs } in
              let type' = Ast.StructType { types } in
              let expr = Ast.TerminalExpr { value; type' } in
              (type', expr))
      | Ast.InvokeExpr o ->
          let type' = infer_expr_type env expr in
          let _, args = List.map (infer_expr env) o.args |> List.split in
          let expr = SetAst.Expr.with_args expr args in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.BinOpExpr o ->
          let expr =
            Ast.BinOpExpr
              {
                lvalue = second (infer_expr env o.lvalue);
                op = o.op;
                rvalue = second (infer_expr env o.rvalue);
                type' = o.type';
              }
          in
          let type' = infer_expr_type env expr in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.UnOpExpr o ->
          let expr =
            Ast.UnOpExpr
              { value = second (infer_expr env o.value); op = o.op; type' = o.type' }
          in
          let type' = infer_expr_type env expr in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.IfExpr o ->
          (* TODO *)
          (Ast.NoneType, expr)
      | Ast.ElseIfExpr o ->
          (* TODO *)
          (Ast.NoneType, expr)
      | Ast.ElseExpr o ->
          (* TODO *)
          (Ast.NoneType, expr)
    in
    (type', expr)
  in

  let rec infer_vars vars type' =
    let infer_var var type' =
      match var with Ast.Var v -> Ast.Var { id = v.id; state = v.state; type' }
    in
    let vars =
      match vars with
      | [] -> never loc "No variables exist to infer."
      | [ var ] -> [ infer_var var type' ]
      | vars ->
          let data = List.combine vars (match type' with Ast.StructType s -> s.types) in
          let vars = List.map (fun (v, s) -> infer_var v s) data in
          vars
    in
    vars
  in

  let rec infer_stmt env stmt =
    match stmt with
    | Ast.LetStmt s ->
        let type', expr = infer_expr env s.expr in
        let vars = infer_vars s.vars type' in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        let stmt = SetAst.Stmt.with_vars stmt vars in
        let env = extend_env env (GetAst.Stmt.vars stmt) in
        (env, stmt)
    | Ast.SetStmt s ->
        (* TODO: Implement set statement *)
        (env, stmt)
    | Ast.ReturnStmt s ->
        let _, expr = infer_expr env s.expr in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        (env, stmt)
    | Ast.InvokeStmt s ->
        let _, expr = infer_expr env s.expr in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        (env, stmt)
    | Ast.IfStmt s ->
        (* TODO: Implement if statement *)
        (env, stmt)
    | Ast.ElseIfStmt s ->
        (* TODO: Implement else if statement *)
        (env, stmt)
    | Ast.ElseStmt s ->
        (* TODO: Implement else statement *)
        (env, stmt)
    | _ -> todo loc "Infer statement."
  (* Infer list of statements *)
  and infer_stmts env stmts =
    let rec aux env stmts acc =
      match stmts with
      | [] -> List.rev acc
      | hd :: tl ->
          let env, hd = infer_stmt env hd in
          aux env tl (hd :: acc)
    in
    aux env stmts []
  (* Infer entities *)
  and infer_entity env enty =
    match enty with
    | Ast.Function f ->
        let block =
          match f.block with
          | Ast.Block b ->
              let env = extend_env env f.args in
              let stmts = infer_stmts env b.stmts in
              Ast.Block { stmts }
        in
        Ast.Function { id = f.id; args = f.args; type' = f.type'; block }
    | _ -> todo loc "Infer entity." unit
  in

  let infer env =
    let tst = match ast with Ast.File f -> List.map (infer_entity env) f.entities in
    tst
  in

  let tst = Ast.File { entities = envir ast |> infer } in
  tst
;;

(** Compile a section of code *)
let compile code =
  let ast = parse code in
  let tst = infer ast in
  unit
;;

(** Execution starts here *)
let main =
  let code = File.read_file_content "x.nxn" in
  compile code;
  write "-*-";
  unit
;;
