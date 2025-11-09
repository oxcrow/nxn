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

(** Evalulate error message *)
let errormsg filename pos =
  let lnum, cnum = pos in
  let line0 =
    "\n (Around approximate position: " ^ "(Line: " ^ string_of_int lnum ^ ", Column: "
    ^ string_of_int cnum ^ ") of File: \"" ^ filename ^ "\")\n"
  in

  let lines = String.split_on_char '\n' (File.read_file_content filename) in
  let line lnum =
    let code =
      match lnum > 0 && lnum < List.length lines with
      | true -> List.nth lines (lnum - 1)
      | false -> ""
    in
    " : " ^ Format.sprintf "%*d" 5 lnum ^ " ┊ " ^ code
  in

  let black = "\027[0m" in
  let red = "\027[31m" in

  let line1 = line (lnum - 2) ^ "\n" in
  let line2 = line (lnum - 1) ^ "\n" in
  let line3 = red ^ line (lnum - 0) ^ black ^ "\n" in
  let line4 = line (lnum + 1) ^ "\n" in
  let line5 = line (lnum + 2) ^ "\n" in

  let text = line1 ^ line2 ^ line3 ^ line4 ^ line5 in
  let msg = line0 ^ text in
  msg
;;

(** Parse nxn code and create AST *)
let parse code filename =
  let buf = Lexing.from_string code in

  let errorpos (buf : Lexing.lexbuf) =
    let lnum = buf.lex_curr_p.pos_lnum in
    let cnum = buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol in
    (lnum, cnum)
  in

  let parsed_ast =
    try NxnParser.file NxnLexer.token buf with
    | NxnParser.Error ->
        let pos = errorpos buf in
        let reason = errormsg filename pos in
        error loc ("Parser error: " ^ reason)
    | Failure msg ->
        let pos = errorpos buf in
        let reason = errormsg filename pos in
        error loc ("Parser error: " ^ reason ^ ", With message: " ^ msg)
  in

  let ast =
    match parsed_ast with Ast.File f -> Ast.File { entities = f.entities; filename }
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
              let xpos = GetAst.Entity.xpos entity in
              let id = GetAst.Id.value f.id in
              let type' = f.type' in
              (id, type', xpos)
          | _ -> never loc "Only functions are allowed.")
        (List.filter
           (fun e -> match e with Ast.Function _ -> true | _ -> false)
           entities)
    in

    (* Validate environment records *)
    let validate records =
      let rec count id n records =
        match records with
        | [] -> n
        | (x, _, _) :: tl -> if x = id then count id (n + 1) tl else count id n tl
      in
      let counts = List.map (fun (id, _, xpos) -> (count id 0 records, xpos)) records in

      (* We should report which identifier has multiple ocurrances *)
      let filename = GetAst.File.filename ast in
      List.iter
        (fun (n, xpos) ->
          if n > 1 then
            warn loc ("Multiple occurances of identifier. " ^ errormsg filename xpos))
        counts;
      List.iter
        (fun (n, _) -> if n > 1 then error loc "Multiple occurances of identifier.")
        counts;

      (* Remove position from records *)
      let result = List.map (fun (id, type', _) -> (id, type')) records in
      result
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

  let rec simplify_type type' =
    let type' =
      match type' with
      | Ast.ConRefType t -> Ast.ConRefType { life = None; types = t.types }
      | Ast.MutRefType t -> Ast.MutRefType { life = None; types = t.types }
      | _ -> type'
    in
    type'
  in

  let rec infer_expr_type env expr =
    let type' =
      match expr with
      | Ast.TerminalExpr term -> (
          match term.value with
          | Ast.UndefinedVal -> Ast.NoneType
          | Ast.UnitVal -> Ast.UnitType
          | Ast.BoolVal _ -> Ast.BoolType
          | Ast.IntVal _ -> Ast.IntType
          | Ast.FloatVal _ -> Ast.FloatType
          | Ast.IdVal o ->
              let id = GetAst.Id.value o.value in
              let type' =
                match GetEnv.File.var_type id env with
                | Some type' -> type'
                | None ->
                    error loc
                      (quote id ^ " does not exist in environment."
                      ^ errormsg (GetAst.File.filename ast) (GetAst.Id.xpos o.value))
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
            | None ->
                error loc
                  (quote id ^ " does not exist in environment."
                  ^ errormsg (GetAst.File.filename ast) (GetAst.Id.xpos o.value))
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
          let match_types env lvalue rvalue =
            let ltype = infer_expr_type env lvalue |> simplify_type in
            let rtype = infer_expr_type env rvalue |> simplify_type in
            let type' =
              if ltype = rtype then ltype else error loc "Binary operator types mismatch."
            in
            type'
          in
          let value_type = match_types env o.lvalue o.rvalue in
          let type' =
            match o.op with
            | Ast.EqOp -> Ast.BoolType
            | Ast.NeOp -> Ast.BoolType
            | Ast.LeOp -> Ast.BoolType
            | Ast.GeOp -> Ast.BoolType
            | Ast.LtOp -> Ast.BoolType
            | Ast.GtOp -> Ast.BoolType
            | _ -> value_type
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
            | Ast.TryOp -> value_type
            | _ -> todo loc "Infer unary expression type"
          in
          type'
    in
    type'
  (* Infer expression *)
  and infer_expr env expr =
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
          | Ast.BoolVal o ->
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
              let pos = GetAst.Expr.pos expr in
              let expr = Ast.TerminalExpr { value; type'; pos } in
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
                pos = o.pos;
              }
          in
          let type' = infer_expr_type env expr in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.UnOpExpr o ->
          let expr =
            Ast.UnOpExpr
              {
                value = second (infer_expr env o.value);
                op = o.op;
                type' = o.type';
                pos = o.pos;
              }
          in
          let type' = infer_expr_type env expr in
          let expr = SetAst.Expr.with_type expr type' in
          (type', expr)
      | Ast.IfExpr o ->
          let validate_branch_type expr =
            let rec aux expr type' =
              match expr with
              | Ast.IfExpr o -> (
                  verify loc (o.type' = type') "if branch type doesn't match";
                  match o.other with Some x -> aux x type' | None -> unit)
              | Ast.ElseIfExpr o -> (
                  verify loc (o.type' = type') "else if branch type doesn't match";
                  match o.other with Some x -> aux x type' | None -> unit)
              | Ast.ElseExpr o ->
                  verify loc (o.type' = type') "else branch type doesn't match"
              | _ -> never loc "Only if/else expressions are allowed."
            in
            aux expr (GetAst.Expr.type' expr);
            expr
          in
          let validate_else_branch expr =
            let rec aux expr =
              match expr with
              | Ast.IfExpr o -> (
                  match o.is_stmt with
                  | true -> true
                  | false -> ( match o.other with Some x -> aux x | None -> false))
              | Ast.ElseIfExpr o -> (
                  match o.other with Some x -> aux x | None -> false)
              | Ast.ElseExpr _ -> true
              | _ -> never loc "Only if/else expressions are allowed."
            in
            let valid = aux expr in
            verify loc valid "else branch not found for conditional expression";
            expr
          in
          let block = infer_block env o.block in
          let type' = infer_block_type block in
          let expr =
            Ast.IfExpr
              {
                cond =
                  (let type', expr = infer_expr env o.cond in
                   verify loc (type' = Ast.BoolType) "Only bool values can be conditions";
                   expr);
                block;
                other =
                  (match o.other with
                  | Some x -> Some (infer_expr env x |> second)
                  | None -> None);
                is_stmt = o.is_stmt;
                type';
                pos = o.pos;
              }
            |> validate_branch_type |> validate_else_branch
          in
          (type', expr)
      | Ast.ElseIfExpr o ->
          let block = infer_block env o.block in
          let type' = infer_block_type block in
          let expr =
            Ast.ElseIfExpr
              {
                cond =
                  (let type', expr = infer_expr env o.cond in
                   verify loc (type' = Ast.BoolType) "Only bool values can be conditions";
                   expr);
                block;
                other =
                  (match o.other with
                  | Some x -> Some (infer_expr env x |> second)
                  | None -> None);
                type';
                pos = o.pos;
              }
          in
          (type', expr)
      | Ast.ElseExpr o ->
          let block = infer_block env o.block in
          let type' = infer_block_type block in
          let expr = Ast.ElseExpr { block; type'; pos = o.pos } in
          (type', expr)
      | Ast.BlockExpr o ->
          let block = infer_block env o.block in
          let type' = infer_block_type block in
          let expr = Ast.BlockExpr { block; type'; pos = o.pos } in
          (type', expr)
    in
    (type', expr)
  (* Infer statement *)
  and infer_stmt env stmt =
    match stmt with
    | Ast.LetStmt s ->
        let type', expr = infer_expr env s.expr in
        let vars = infer_vars s.vars type' in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        let stmt = SetAst.Stmt.with_vars stmt vars in
        let env = extend_env env (GetAst.Stmt.vars stmt) in
        (env, stmt)
    | Ast.SetStmt s ->
        let _, expr = infer_expr env s.expr in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        (env, stmt)
    | Ast.AssignStmt s ->
        (* TODO: Implement assign statement *)
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
        let _, expr = infer_expr env s.expr in
        let stmt = SetAst.Stmt.with_expr stmt expr in
        (env, stmt)
    | Ast.BlockStmt s ->
        let block = infer_block env s.block in
        verify loc
          (infer_block_type block = Ast.UnitType)
          ("Block statements can not return values with set statements."
          ^ errormsg (GetAst.File.filename ast)
              (match block with
              | Ast.Block b -> (
                  let sets =
                    List.filter
                      (fun s -> match s with Ast.SetStmt _ -> true | _ -> false)
                      b.stmts
                  in
                  match List.length sets with
                  | 1 -> GetAst.Stmt.xpos (List.nth sets 0)
                  | _ -> (0, 0))));
        let stmt = Ast.BlockStmt { block; pos = s.pos } in
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
  (* Infer block type using set statements *)
  and infer_block_type block =
    match block with
    | Ast.Block b -> (
        let sets =
          List.filter (fun s -> match s with Ast.SetStmt _ -> true | _ -> false) b.stmts
        in
        match List.length sets with
        | 0 -> Ast.UnitType
        | 1 ->
            let first = List.nth sets 0 in
            let type' = GetAst.Stmt.type' first in
            type'
        | _ -> never loc "A block may have only zero or one set statement")
  (* Infer block *)
  and infer_block env block =
    let block =
      match block with
      | Ast.Block b ->
          let stmts = infer_stmts env b.stmts in
          Ast.Block { stmts; pos = b.pos }
    in
    block
  (* Infer entities *)
  and infer_entity env enty =
    match enty with
    | Ast.Function f ->
        let env = extend_env env f.args in
        let block = infer_block env f.block in
        Ast.Function { id = f.id; args = f.args; type' = f.type'; block; pos = f.pos }
    | Ast.Struct _ -> enty
    | _ -> todo loc "Infer entity." unit
  in

  let infer env =
    let tst = match ast with Ast.File f -> List.map (infer_entity env) f.entities in
    tst
  in

  (* Create typed ast by infering its entities *)
  let tst =
    match ast with
    | Ast.File f -> Ast.File { entities = envir ast |> infer; filename = f.filename }
  in
  tst
;;

(** Emit LLVM IR from compiled AST *)
let emit cfg = unit

(** Compile a file *)
let compile_file file =
  let code = File.read_file_content file in
  let ast = parse code file in
  let tst = infer ast in
  true
;;

(** Compile code and report error *)
let compile file =
  let _success =
    try compile_file file with
    | Failure msg ->
        write msg;
        false
    | exn ->
        write @@ Printexc.to_string exn;
        false
    | _ ->
        todo loc "Unknown error raised.";
        false
  in
  unit
;;

(** Test if file can be compiled *)
let pass file = compile file

(** Test if file can not be compiled *)
let fail file =
  let success =
    try compile_file file with
    | Failure _ -> false
    | exn -> false
    | _ ->
        todo loc "Unknown error raised.";
        false
  in
  verify loc (success = false) "Code did not fail?"
;;

(** Run integration tests *)
let validate () =
  fail "test/fail/001-01.nxn";
  pass "test/pass/001-01.nxn";
  pass "test/pass/001-02.nxn";
  pass "test/pass/001-03.nxn";
  pass "test/pass/001-04.nxn";
  pass "test/pass/002-01.nxn";
  pass "test/pass/002-02.nxn";
  pass "test/pass/002-03.nxn";
  unit
;;

(** Execution starts here *)
let main =
  compile "x.nxn" |> validate;
  write "+";
  unit
;;
