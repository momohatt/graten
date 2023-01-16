open Torch

let t1 x y : tensor([x; y]) =
  if x = y then
    Tensor.zeros [x; x]
  else
    Tensor.zeros [x; y]

let t2 x y : tensor([x; y]) =
  if x <> y then
    Tensor.zeros [x; y]
  else
    Tensor.zeros [x; x]

let t3 x y z : tensor([x; y; z]) =
  if x = y && y = z then
    Tensor.zeros [x; x; x]
  else
    Tensor.zeros [x; y; z]

let t4 x y z : tensor([x; y; z]) =
  if x <> y || y <> z then
    Tensor.zeros [x; y; z]
  else
    Tensor.zeros [x; x; x]

let f1 n : tensor([2]) =
  if n = 0
  then Tensor.zeros [2 + n]
  else Tensor.zeros [2]

let rec f2 : n:int -> tensor([2 * (n + 1)]) =
  fun n ->
  if n = 0
  then Tensor.zeros [2]
  else Tensor.cat_ ~dim:0 (Tensor.zeros [2]) (f2 (n - 1))
