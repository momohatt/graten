open Torch

(* Unsound case: Combination of optional argument function and ELetIn in arguments
 * Assertion is not inserted at |f x|.
 *)

let opt ?(x = 0) ?(y = 0) f = f x

let f (x : tensor([])) = x

let a =
  let x = Serialize.load ~filename:Sys.argv.(1) in
  opt ~y:(let x = 1 in x) (fun y -> let _ = f x in y)
