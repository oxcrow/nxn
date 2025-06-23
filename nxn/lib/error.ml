(* Location *)
external loc : string = "%loc_LOC"

(** Location *)
let pos loc = "(" ^ loc ^ ")"

(** Raise error *)
let throw x = failwith x

(** Unwrap result *)
let ok = function Ok x -> x | Error e -> failwith e

(** Unwrap option *)
let some = function Some x -> x | None -> failwith "Option is None"

(** Unwrap result or return default value *)
let ok_or y = function Ok x -> x | Error _ -> y

(** Unwrap option or return default value *)
let some_or y = function Some x -> x | None -> y

(** Unreachable code *)
let never x = failwith @@ "Unreachable: " ^ x

(** Code not yet implemented *)
let todo x = failwith @@ "Implement: " ^ x
