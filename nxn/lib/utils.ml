(** Tuple utils *)
module Tuple = struct
  let first t = match t with a, _ -> a
  let second t = match t with _, b -> b
end

(** String utils *)
module String = struct
  let sp = " "
  let nl = "\n"
  let quote s = "\"" ^ s ^ "\""
  let paren s = "(" ^ s ^ ")"
end
