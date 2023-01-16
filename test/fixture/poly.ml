open Torch

(* Shape polymorphism *)

let t1 =
  let f x y = Tensor.zeros [x; y]
  in f

let t2 =
  let g =
    let f x y = Tensor.zeros [x; y]
    in f
  in g
