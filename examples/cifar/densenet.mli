val dense_layer
  :  Torch.Var_store.t
  -> ~bn_size:int
  -> ~growth_rate:int
  -> ~input_dim:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; nth 1 x.shape + growth_rate; nth 2 x.shape; nth 3 x.shape])

val dense_block
  :  Torch.Var_store.t
  -> ~bn_size:int
  -> ~growth_rate:int
  -> ~num_layers:int
  -> ~input_dim:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; nth 1 x.shape + num_layers * growth_rate; nth 2 x.shape; nth 3 x.shape])

val transition
  :  Torch.Var_store.t
  -> ~input_dim:int
  -> output_dim:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; output_dim; nth 2 x.shape // 2; nth 3 x.shape // 2])

val model : Torch.Var_store.t -> Model.t
