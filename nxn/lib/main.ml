open Utils.String
open Error.Unwrap

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
  unit;
  unit
;;

(** Execution starts here *)
let main =
  let code = File.read_file_content "x.nxn" in
  let ast = parse code in
  let tst = infer ast in

  write "-*-";
  unit
;;
