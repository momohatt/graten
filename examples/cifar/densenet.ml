(* DenseNet model for the CIFAR-10 dataset.
   https://arxiv.org/pdf/1608.06993.pdf

   The dataset can be downloaded from https://www.cs.toronto.edu/~kriz/cifar.html, files
   should be placed in the data/ directory.

   This reaches 94.2% accuracy.
*)
open Base
open Torch

let sub = Var_store.sub
let batch_size = 64
let epochs = 150

let lr_schedule ~batch_idx:_ ~batches_per_epoch:_ ~epoch_idx =
  if epoch_idx < 50 then 0.1 else if epoch_idx < 150 then 0.01 else 0.001

let conv2d vs ?(stride = 1) ?(padding = 0) =
  Layer.conv2d_ vs ~stride ~use_bias:false ~padding

let dense_layer vs ~bn_size ~growth_rate ~input_dim =
  let inter_dim = bn_size * growth_rate in
  let bn1 = Layer.batch_norm2d (sub vs "norm1") input_dim in
  let conv1 = conv2d (sub vs "conv1") ~ksize:1 ~input_dim inter_dim in
  let bn2 = Layer.batch_norm2d (sub vs "norm2") inter_dim in
  let conv2 =
    conv2d (sub vs "conv2") ~ksize:3 ~padding:1 ~input_dim:inter_dim growth_rate
  in
  Layer.of_fn_ (fun xs ~is_training ->
      Layer.forward_ bn1 xs ~is_training
      |> Tensor.relu
      |> Layer.forward conv1
      |> Layer.forward_ bn2 ~is_training
      |> Tensor.relu
      |> Layer.forward conv2
      (* EDIT: replace Tensor.cat with Tensor.cat_ *)
      |> Tensor.cat_ ~dim:1 xs)

let dense_block vs ~bn_size ~growth_rate ~num_layers ~input_dim =
  (* EDIT: Remove Layer.sequential_ *)
  let rec loop =
    fun i xs ~is_training ->
      if i = 0
      then Layer.id_ ~is_training xs
      else
        let vs = sub vs (Printf.sprintf "denselayer%d" (1 + i)) in
        loop (i - 1) xs ~is_training
        |> dense_layer vs ~bn_size ~growth_rate ~input_dim:(input_dim + ((i - 1) * growth_rate)) ~is_training
  in Layer.of_fn_ (loop num_layers)

let transition vs ~input_dim output_dim =
  let bn = Layer.batch_norm2d (sub vs "norm") input_dim in
  let conv = conv2d (sub vs "conv") ~ksize:1 ~input_dim output_dim in
  Layer.of_fn_ (fun xs ~is_training ->
      Layer.forward_ bn xs ~is_training
      |> Tensor.relu
      |> Layer.forward conv
      |> Tensor.avg_pool2d ~stride:(2, 2) ~ksize:(2, 2))

(* EDIT: Change ~block to 4 args *)
let densenet vs ~growth_rate ~c1 ~c2 ~c3 ~c4 ~init_dim ~bn_size ~num_classes =
  let f_vs = sub vs "features" in
  let conv0 = conv2d (sub f_vs "conv0") ~ksize:3 ~padding:2 ~input_dim:3 init_dim in
  let bn0 = Layer.batch_norm2d (sub f_vs "norm0") init_dim in
  (* EDIT: unroll foldi *)
  let dense_block' ~i ~num_layers ~num_features =
    ( num_features + num_layers * growth_rate
    , dense_block (sub f_vs (Printf.sprintf "dense_block%d" i)) ~bn_size ~growth_rate ~num_layers ~input_dim:num_features ) in
  let transition' ~i ~num_features =
    ( num_features / 2
    , transition (sub f_vs (Printf.sprintf "transition%d" i)) ~input_dim:num_features (num_features / 2) ) in
  let (num_features, block1) = dense_block' ~i:1 ~num_layers:c1 ~num_features:init_dim in
  let (num_features, trans1) = transition'  ~i:1                ~num_features in
  let (num_features, block2) = dense_block' ~i:2 ~num_layers:c2 ~num_features in
  let (num_features, trans2) = transition'  ~i:2                ~num_features in
  let (num_features, block3) = dense_block' ~i:3 ~num_layers:c3 ~num_features in
  let (num_features, trans3) = transition'  ~i:3                ~num_features in
  let (num_features, block4) = dense_block' ~i:4 ~num_layers:c4 ~num_features in
  let bn5 = Layer.batch_norm2d (sub f_vs "norm5") num_features in
  let linear = Layer.linear (sub vs "classifier") ~input_dim:num_features num_classes in
  Layer.of_fn_ (fun xs ~is_training ->
      Layer.forward conv0 xs
      |> Layer.forward_ bn0 ~is_training
      |> Tensor.relu
      |> Layer.forward_ block1 ~is_training
      |> Layer.forward_ trans1 ~is_training
      |> Layer.forward_ block2 ~is_training
      |> Layer.forward_ trans2 ~is_training
      |> Layer.forward_ block3 ~is_training
      |> Layer.forward_ trans3 ~is_training
      |> Layer.forward_ block4 ~is_training
      |> Layer.forward_ bn5 ~is_training
      |> fun features -> Tensor.relu features
      |> Tensor.avg_pool2d ~ksize:(4, 4) ~stride:(4, 4)
      |> Tensor.view ~size:[ Tensor.shape features |> List.hd_exn; -1 ]
      |> Layer.forward linear)

let densenet121 vs =
  densenet
    vs
    ~growth_rate:32
    ~c1:6 ~c2:12 ~c3:24 ~c4:16 (* ~block_config:(6, 12, 24, 16) *)
    ~init_dim:64
    ~bn_size:4
    ~num_classes:10

let model vs =
  Model.{ model_name = "densenet121"
  ; model = densenet121 vs
  ; epochs
  ; lr_schedule
  ; batch_size
  }
