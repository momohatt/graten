val downsample
  :  Torch.Var_store.t -> ~stride:int -> ~input_dim:int -> int
  -> { v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool -> tensor

val basic_block
  :  Torch.Var_store.t -> ~stride:int -> ~input_dim:int -> int
  -> { v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool -> tensor

val bottleneck_block
  :  Torch.Var_store.t -> ~expansion:int -> ~stride:int -> ~input_dim:int -> int
  -> { v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool -> tensor

val resnet
  :  int -> Torch.Var_store.t -> ~block:string -> int -> int -> int -> int
  -> { v:tensor | len v.shape = 4 && nth 1 v.shape = 3 }
  -> ~is_training:bool
  -> tensor (* { v:tensor | v.shape = [nth 0 xs.shape; num_classes] } *)
