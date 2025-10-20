open Utils.String

module Failure = struct
  let todo loc x = failwith @@ "Not yet implemented: " ^ x ^ sp ^ paren loc
  let never loc x = failwith @@ "Unreachable code executed: " ^ x ^ sp ^ paren loc
  let error loc x = failwith @@ "Error: " ^ x ^ sp ^ paren loc
end

module Unwrap = struct
  let some loc x =
    match x with Some y -> y | None -> failwith @@ "Option is None." ^ sp ^ paren loc
  ;;

  let ok loc x =
    match x with Ok y -> y | Error _ -> failwith @@ "Result is Error." ^ sp ^ paren loc
  ;;
end
