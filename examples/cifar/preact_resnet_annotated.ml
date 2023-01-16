(* Pre-activation ResNet model for the CIFAR-10 dataset.

   The dataset can be downloaded from https://www.cs.toronto.edu/~kriz/cifar.html, files
   should be placed in the data/ directory.

   This model uses the pre-activation variant of ResNet18 as introduced in:
     Identity Mappings in Deep Residual Networks
     Kaiming He et al., 2016
     https://arxiv.org/abs/1603.05027

   This reaches ~94.6% accuracy.
*)
open Base
open Torch

let batch_size = 128
let epochs = 150

let lr_schedule ~batch_idx:_ ~batches_per_epoch:_ ~epoch_idx =
  if epoch_idx < 50 then 0.1 else if epoch_idx < 100 then 0.01 else 0.001

let conv2d ?(padding = 1) ?(ksize = 3) = Layer.conv2d_ ~ksize ~padding ~use_bias:false

let basic_block vs ~stride ~input_dim output_dim =
  let conv2d1 = conv2d vs ~stride ~input_dim output_dim in
  let conv2d2 = conv2d vs ~stride:1 ~input_dim:output_dim output_dim in
  let bn1 = Layer.batch_norm2d vs input_dim in
  let bn2 = Layer.batch_norm2d vs output_dim in
  let shortcut
    : ~xs:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim } -> ~out:{ v:tensor | v.shape = xs.shape && len v.shape = 4 && nth 1 v.shape = input_dim }
      -> tensor([nth 0 xs.shape; output_dim; (nth 2 xs.shape - 1) // stride + 1; (nth 3 xs.shape - 1) // stride + 1]) = (* ANNOT: 3 *)
    if stride = 1 && input_dim = output_dim
    then fun ~xs ~out:_ -> xs
    else (
      let conv2d = conv2d vs ~padding:0 ~ksize:1 ~stride ~input_dim output_dim in
      fun ~xs:_ ~out -> Layer.forward conv2d out)
  in
  fun xs ~is_training ->
    let out = Layer.forward_ bn1 ~is_training xs |> Tensor.relu in
    let shortcut = shortcut ~xs ~out in
    Layer.forward conv2d1 out
    |> Layer.forward_ bn2 ~is_training
    |> Tensor.relu
    |> Layer.forward conv2d2
    |> fun ys -> Tensor.( + ) ys shortcut

let rec block_stack vs ~stride ~depth ~input_dim output_dim =
  (* EDIT: use recursive function to remove List.init and List.fold *)
  let rec loop
  :  i:int -> xs:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool
  -> tensor([nth 0 xs.shape; output_dim; (nth 2 xs.shape - 1) // stride + 1; (nth 3 xs.shape - 1) // stride + 1]) (* ANNOT: 2 *)
  = fun i ->
    if i = 0
    then basic_block vs ~stride ~input_dim output_dim
    else fun xs ~is_training ->
      loop (i - 1) xs ~is_training
      |> basic_block vs ~stride:1 ~input_dim:output_dim output_dim ~is_training
  in loop (depth - 1)

let resnet vs =
  let conv2d = conv2d vs ~stride:1 ~input_dim:3 64 in
  let stack1 = block_stack vs ~stride:1 ~depth:2 ~input_dim:64 64 in
  let stack2 = block_stack vs ~stride:2 ~depth:2 ~input_dim:64 128 in
  let stack3 = block_stack vs ~stride:2 ~depth:2 ~input_dim:128 256 in
  (* The output there should be of size 512 but this requires more than
     2GB of memory.
  *)
  let stack4 = block_stack vs ~stride:2 ~depth:2 ~input_dim:256 256 in
  let linear = Layer.linear vs ~input_dim:256 Cifar_helper.label_count in
  Layer.of_fn_ (fun xs ~is_training ->
      let batch_size = Tensor.shape xs |> List.hd_exn in
      Tensor.reshape xs ~shape:Cifar_helper.[ -1; image_c; image_w; image_h ]
      |> Layer.forward conv2d
      |> stack1 ~is_training
      |> stack2 ~is_training
      |> stack3 ~is_training
      |> stack4 ~is_training
      |> Tensor.avg_pool2d ~ksize:(4, 4)
      |> Tensor.reshape ~shape:[ batch_size; -1 ]
      |> Layer.forward linear)

let model vs =
  Model.{ model_name = "preact-resnet"
  ; model = resnet vs
  ; epochs
  ; lr_schedule
  ; batch_size
  }
