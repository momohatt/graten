open Torch

let vs = Var_store.create ~name:"test" ()

(** linear layer *)

let l1 x y =
  Layer.linear vs ~input_dim:x y

let t1 = (l1 4 5) (Tensor.zeros [2; 3; 4])

(** dropout layer *)

let l2 =
  Tensor.dropout ~p:0.5 ~is_training:true 

let t2 = l2 (Tensor.zeros [2; 3; 4])

let l3 xs = l1 4 5 xs |> l2

(** Pooling layers *)

let l4 x y =
  Tensor.(max_pool2d ~ksize:(2, 2) (ones [2; 2; x; y]))

let l5 x y =
  Tensor.(max_pool2d ~padding:(1, 1) ~ksize:(2, 2) (ones [2; 2; x; y]))

let l6 x y =
  Tensor.(max_pool2d ~ksize:(2, 2) ~stride:(1, 1) (ones [2; 2; x; y]))

let t3 = l6 5 5

(** Convolution layers *)

let l7 x y =
  Layer.conv2d_ vs ~ksize:2 ~stride:1 ~input_dim:x y (Tensor.ones [32; x; 128; 128])

let l8 x y =
  let conv = Layer.conv2d_ vs ~ksize:2 ~stride:1 ~input_dim:x y in
  conv
