open Base
open Torch

let conv2d vs ?(ksize = 3) ~stride ?(padding = 1) =
  Layer.conv2d_ vs ~ksize ~stride ~use_bias:false ~padding

let sub = Var_store.sub

(* downsample
 * :  var_store -> ~stride:int -> ~input_dim:int -> int
 * -> { v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
 * -> ~is_training:bool -> tensor
 *)
let downsample vs ~stride ~input_dim output_dim =
  if stride <> 1 || input_dim <> output_dim
  then (
    (* tensor(['s1; input_dim; 'h; 'w])
       -> tensor(['s1; output_dim; ('h - 1) // stride + 1; ('w - 1) // stride + 1]) *)
    let conv = conv2d (sub vs "0") ~stride ~ksize:1 ~padding:0 ~input_dim output_dim in
    let bn = Layer.batch_norm2d (sub vs "1") output_dim in
    Layer.of_fn_ (fun xs ~is_training ->
        Layer.forward conv xs |> Layer.forward_ bn ~is_training)
  )
  else
    (* tensor(['s1; input_dim; 'h; 'w])
       -> tensor(['s1; input_dim; 'h; 'w]) *)
    Layer.id_

(* basic_block
 * :  var_store -> ~stride:int -> ~input_dim:int -> int
 * -> { v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
 * -> ~is_training:bool -> tensor
 *)
let basic_block vs ~stride ~input_dim output_dim =
  let conv1 = conv2d (sub vs "conv1") ~stride ~input_dim output_dim in
  let bn1 = Layer.batch_norm2d (sub vs "bn1") output_dim in
  let conv2 = conv2d (sub vs "conv2") ~stride:1 ~input_dim:output_dim output_dim in
  let bn2 = Layer.batch_norm2d (sub vs "bn2") output_dim in
  let downsample = downsample (sub vs "downsample") ~stride ~input_dim output_dim in
  Layer.of_fn_ (fun xs ~is_training ->
      Layer.forward conv1 xs
      |> Layer.forward_ bn1 ~is_training
      |> Tensor.relu
      |> Layer.forward conv2
      |> Layer.forward_ bn2 ~is_training
      |> Tensor.( + ) (Layer.forward_ downsample xs ~is_training)
      |> Tensor.relu)

(* bottleneck_block
 * :  var_store -> ~expansion:int -> ~stride:int -> ~input_dim:int -> int
 * -> { v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
 * -> ~is_training:bool -> tensor
 *)
let bottleneck_block vs ~expansion ~stride ~input_dim output_dim =
  let expanded_dim = expansion * output_dim in
  let conv1 =
    conv2d (sub vs "conv1") ~stride:1 ~padding:0 ~ksize:1 ~input_dim output_dim
  in
  let bn1 = Layer.batch_norm2d (sub vs "bn1") output_dim in
  let conv2 = conv2d (sub vs "conv2") ~stride ~input_dim:output_dim output_dim in
  let bn2 = Layer.batch_norm2d (sub vs "bn2") output_dim in
  let conv3 =
    conv2d
      (sub vs "conv3")
      ~stride:1
      ~padding:0
      ~ksize:1
      ~input_dim:output_dim
      expanded_dim
  in
  let bn3 = Layer.batch_norm2d (sub vs "bn3") expanded_dim in
  let downsample = downsample (sub vs "downsample") ~stride ~input_dim expanded_dim in
  Layer.of_fn_ (fun xs ~is_training ->
      Layer.forward conv1 xs
      |> Layer.forward_ bn1 ~is_training
      |> Tensor.relu
      |> Layer.forward conv2
      |> Layer.forward_ bn2 ~is_training
      |> Tensor.relu
      |> Layer.forward conv3
      |> Layer.forward_ bn3 ~is_training
      |> Tensor.( + ) (Layer.forward_ downsample xs ~is_training)
      |> Tensor.relu)

(* resnet
 * :  num_classes:int -> var_store -> ~block:string -> int -> int -> int -> int
 * -> xs:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 }
 * -> ~is_training:bool -> { v:tensor | v.shape = [nth 0 xs.shape; num_classes] }
 *)
let resnet num_classes vs ~block:_ c1 c2 c3 c4 =
  let block = basic_block in
  let e = 1 (* or... bottleneck_block ~expansion:4, 4 *)
  in
  let rec make_layer
    = fun vs ~stride ~cnt ~input_dim output_dim ->
      let vs = sub vs (Int.to_string cnt) in
      if cnt = 0
      then block vs ~stride ~input_dim output_dim
      else Layer.of_fn_ (fun xs ~is_training ->
        Layer.forward_ (make_layer vs ~stride ~cnt:(cnt - 1) ~input_dim output_dim) xs ~is_training
        |> Layer.forward_ (block vs ~stride:1 ~input_dim:(output_dim * e) output_dim) ~is_training)
  in
  let conv1 = conv2d (sub vs "conv1") ~stride:2 ~padding:3 ~ksize:7 ~input_dim:3 64 in    (* [n; 3; h; w] -> [n; 64; (h - 1) // 2 + 1; (w - 1) // 2 + 1] *)
  let bn1 = Layer.batch_norm2d (sub vs "bn1") 64 in                                       (* [s1; 64; s3; s4] -> [s1; 64; s3; s4] *)
  let layer1 = make_layer (sub vs "layer1") ~stride:1 ~cnt:c1 ~input_dim:64 64 in         (* [n; 64; h; w] -> [n; 64; h; w] *)
  let layer2 = make_layer (sub vs "layer2") ~stride:2 ~cnt:c2 ~input_dim:(64 * e) 128 in  (* [n; 64; h; w] -> [n; 128; (h - 1) // 2 + 1; (w - 1) // 2 + 1] *)
  let layer3 = make_layer (sub vs "layer3") ~stride:2 ~cnt:c3 ~input_dim:(128 * e) 256 in (* [n; 128; h; w] -> [n; 256; (h - 1) // 2 + 1; (w - 1) // 2 + 1] *)
  let layer4 = make_layer (sub vs "layer4") ~stride:2 ~cnt:c4 ~input_dim:(256 * e) 512 in (* [n; 256; h; w] -> [n; 512; (h - 1) // 2 + 1; (w - 1) // 2 + 1] *)
  let fc =
    Layer.linear (sub vs "fc") ~input_dim:(512 * e) num_classes
    (*
    match num_classes with
    | Some num_classes -> Layer.linear (sub vs "fc") ~input_dim:(512 * e) num_classes
    | None -> Layer.id
    *)
  in
  Layer.of_fn_ (fun xs ~is_training ->                    (* [n; 3; h; w] *)
      let batch_size = Tensor.shape xs |> List.hd_exn in
      Layer.forward conv1 xs                              (* [n; 64; (h - 1) // 2 + 1; (w - 1) // 2 + 1] *)
      |> Layer.forward_ bn1 ~is_training
      |> Tensor.relu
      |> Tensor.max_pool2d ~padding:(1, 1) ~ksize:(3, 3) ~stride:(2, 2) (* [n; 64; (h - 1) // 2 // 2 + 1; (w - 1) // 2 // 2 + 1] *)
      |> Layer.forward_ layer1 ~is_training               (* [n; 64; (h - 1) // 2 // 2 + 1; (w - 1) // 2 // 2 + 1] *)
      |> Layer.forward_ layer2 ~is_training               (* [n; 128; (h - 1) // 2 // 2 // 2 + 1; (w - 1) // 2 // 2 // 2 + 1] *)
      |> Layer.forward_ layer3 ~is_training               (* [n; 256; (h - 1) // 2 // 2 // 2 + 1; (w - 1) // 2 // 2 // 2 + 1] *)
      |> Layer.forward_ layer4 ~is_training               (* [n; 512; (h - 1) // 2 // 2 // 2 + 1; (w - 1) // 2 // 2 // 2 + 1] *)
      |> Tensor.adaptive_avg_pool2d ~output_size:[ 1; 1 ] (* [n; 512; 1; 1] *)
      |> Tensor.view ~size:[ batch_size; -1 ]             (* [n; 512] *)
      |> Layer.forward fc)                                (* [n; num_classes] *)

let resnet18 num_classes vs = resnet num_classes vs ~block:`basic 2 2 2 2
let resnet34 num_classes vs = resnet num_classes vs ~block:`basic 3 4 6 3

(*
let resnet50 num_classes vs =
  resnet num_classes vs ~block:`bottleneck ~layers:(3, 4, 6, 3)

let resnet101 num_classes vs =
  resnet num_classes vs ~block:`bottleneck ~layers:(3, 4, 23, 3)

let resnet152 num_classes vs =
  resnet num_classes vs ~block:`bottleneck ~layers:(3, 8, 36, 3)
*)
