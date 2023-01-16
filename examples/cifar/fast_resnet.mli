val conv_bn
  :  Torch.Var_store.t
  -> ~c_in:int
  -> ~c_out:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = c_in }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; c_out; nth 2 x.shape; nth 3 x.shape])

val layer
  :  Torch.Var_store.t
  -> ~c_in:int
  -> ~c_out:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = c_in }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; c_out; nth 2 x.shape // 2; nth 3 x.shape // 2])

val fast_resnet
  :  Torch.Var_store.t
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 && (nth 2 v.shape // 32) * (nth 3 v.shape // 32) = 1 }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; 10])

val model : Torch.Var_store.t -> Model.t
