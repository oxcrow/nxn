(** Unique node identifier.

    It is necessary for us to create unique indentifiers for our HIR/AST nodes. The
    simplest way to do this would be to use a mutable integer counter. Everytime this
    function is called, the counter is incremented, and a new identifier is returned.

    WARNING: This is not thread safe. *)
let unique_node_id () =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter
;;
