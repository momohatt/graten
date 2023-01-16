open Torch

let device = Device.cuda_if_available ()
let vs = Var_store.create ~device ~name:"cnn" ()

(* [n; 28; 28] -> [n; 10] *)
let model =
  let conv2d1 = Layer.conv2d_ vs ~ksize:5 ~stride:1 ~input_dim:1 32 in
  let conv2d2 = Layer.conv2d_ vs ~ksize:5 ~stride:1 ~input_dim:32 64 in
  let linear  = Layer.linear vs ~input_dim:1024 Mnist_helper.label_count in
  fun is_training (xs : tensor([nth 0 v.shape; 28; 28])) ->
    Tensor.reshape ~shape:[ -1; 1; 28; 28 ] xs
    (* tensor(['n; 1; 'h; 'w]) -> tensor(['n; 32; 'h-4; 'w-4]) *)
    |> Layer.forward conv2d1
    (* tensor(['s1; 's2; 'h; 'w]) -> tensor(['s1; 's2; 'h//2; 'w//2]) *)
    |> Tensor.max_pool2d ~ksize:(2, 2)
    (* tensor(['n; 32; 'h; 'w]) -> tensor(['n; 64; 'h-4; 'w-4]) *)
    |> Layer.forward conv2d2
    (* tensor(['s1; 's2; 'h; 'w]) -> tensor(['s1; 's2; 'h//2; 'w//2]) *)
    |> Tensor.max_pool2d ~ksize:(2, 2)
    |> Tensor.reshape ~shape:[ -1; 1024 ]
    (* tensor('S @ [1024]) -> tensor('S @ [1024]) *)
    |> Layer.forward linear

let f (t : tensor([20; 10])) = t

let g (t : tensor([30; 10])) = t

let x =
  let train_model = model true in
  let img = Serialize.load ~filename:"" in
  let _ = f (train_model img) in
  f img

let h x =
  let img = Serialize.load ~filename:"" in
  if x > 0 then f img else g img
