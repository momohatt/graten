open Torch

let batch_size = 32

let f1 (x : tensor([24; 24])) =
  x

let f2 (x : tensor([32; 1; 24; 24])) =
  Tensor.max_pool2d ~ksize:(1, 1) x

let f3 (x : tensor) =
  x

let f4 (x : { v:tensor | len v.shape = 2 }) =
  x
