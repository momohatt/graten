open Torch

let f1 ?(x = 3) ?(y = 5) z =
  Tensor.zeros [x; y; z]

let t1 = f1 ~x:4 ~y:6 7
let t2 = f1      ~y:6 7
let t3 = f1           7
let t4 = f1 ~x:4      7

let f2 ~x =
  Tensor.zeros [x]

let t5 = f2 ~x:5

let f3 ?x =
  match x with
  | None -> 0
  | Some x -> (x : int)

let t6 = f3 ~x:3
