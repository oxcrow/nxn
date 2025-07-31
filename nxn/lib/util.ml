let char_list_of_string s = List.init (String.length s) (String.get s)
let string_of_char_list l = String.of_seq (List.to_seq l)

(** Quote a string *)
let quote_of_string s = "\"" ^ s ^ "\""

(** Quote a string with space surrounding it *)
let quote_space_of_string s = " \"" ^ s ^ "\" "

(** Group a string with parenthesis surrounding it *)
let paren_group_of_string s = "(" ^ s ^ ")"

(** Convert string to Nim programming language's style *)
let nim_case s =
  (*
    1. Remove all underscores from the string
    2. Lowercase rest of the alphabets *except* for the first one
  *)
  let lowercase c =
    match c with
    | '_' -> Error.never @@ "_ can not be lowercased." ^ Error.loc
    | '0' .. '9' -> c
    | 'a' .. 'z' -> c
    | 'A' .. 'Z' -> Char.lowercase_ascii c
    | _ -> Error.never @@ "Did we forget to convert a char?" ^ Error.loc
  in

  let is_not_under_score = function '_' -> false | _ -> true in

  let s1 = char_list_of_string s in
  let s2 = s1 |> List.filter is_not_under_score in
  let s3 = s2 |> List.tl |> List.map lowercase in
  let s4 = List.hd s2 :: s3 |> string_of_char_list in

  s4
