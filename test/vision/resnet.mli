(*
val downsample
  :  forall n h w.
     var_store
  -> stride:int
  -> input_dim:int
  -> output_dim:int
  -> tensor([n; input_dim; h; w])
  -> is_training:bool
  -> tensor([n; output_dim; (h - 1) // stride + 1; (w - 1) // stride + 1])
  *)

val basic_block
  :  Torch.Var_store.t
  -> ~stride:int
  -> ~input_dim:int
  -> output_dim:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; output_dim; (nth 2 x.shape - 1) // stride + 1; (nth 3 x.shape - 1) // stride + 1])

val bottleneck_block
  :  Torch.Var_store.t
  -> ~expansion:int
  -> ~stride:int
  -> ~input_dim:int
  -> output_dim:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; expansion * output_dim; (nth 2 x.shape - 1) // stride + 1; (nth 3 x.shape - 1) // stride + 1])

val resnet
  :  num_classes:int
  -> Torch.Var_store.t
  (* -> block:(var_store -> stride:int -> input_dim:int -> output_dim:int ->
               tensor([s1; input_dim; h; w]) -> is_training:bool ->
               tensor([s1; e * output_dim; (h - 1) // stride + 1; (w - 1) // stride + 1]),
               { e:int })  *)
  -> ~block:string
  -> int
  -> int
  -> int
  -> int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; num_classes])
