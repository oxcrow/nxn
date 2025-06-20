let unit = ()
let write = print_endline

exception Error of string

(** Parse Ix code and create AST *)
let parse (code : string) =
  let buf = Lexing.from_string code in
  let ast =
    try Ok (NxnParser.file NxnLexer.token buf)
    with NxnParser.Error ->
      let lnum = buf.lex_curr_p.pos_lnum in
      let cnum = buf.lex_curr_p.pos_cnum - buf.lex_curr_p.pos_bol in
      let pos =
        "(Line: " ^ string_of_int lnum ^ ", Column: " ^ string_of_int cnum ^ ")"
      in
      Error ("Parser failed! Around position: " ^ pos)
  in
  ast

(** Print AST for debugging. *)
let printast ast = write ("+ " ^ NxnAst.show_file ast ^ "\n")

(** Execution starts here *)
let main =
  let code = File.read_file_content "x.n" in
  let ast =
    match parse code with
    | Error e -> raise @@ Error e (* Can we improve this? *)
    | Ok x -> x
  in

  ignore code;
  ignore ast;
  write "-*-";
  unit
