(* Control flow graph block *)
type block = Block of { id : id; nodes : nodes list; next : block option }
[@@deriving show { with_path = false }]

and nodes =
  | LetNode
  | BranchNode of { left : block; right : block }
  (* Anything else? *)
  | NoneNode

(* Unique integer identifier that's valid within the block *)
and id = Id of { value : int }
