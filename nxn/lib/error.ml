exception Error of string

(** Unwrap result *)
let ok = function Ok x -> x | Error e -> raise @@ Error e

(** Unwrap option *)
let some = function Some x -> x | None -> raise @@ Error "Option is None"

(** Unwrap result or return default value *)
let ok_or y = function Ok x -> x | Error _ -> y

(** Unwrap option or return default value *)
let some_or y = function Some x -> x | None -> y

(** Unreachable code *)
let unreachable x = failwith @@ "Unreachable: " ^ x

(** Code not yet implemented *)
let todo x = failwith @@ "Implement: " ^ x
