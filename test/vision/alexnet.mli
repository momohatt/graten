val features
  :  Torch.Var_store.t
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 }
  -> tensor([nth 0 x.shape; 256; (nth 2 x.shape - 63) // 64; (nth 3 x.shape - 63) // 64])

val classifier
  :  num_classes:int
  -> Torch.Var_store.t
  -> x:{ v:tensor | last v.shape = 9216 }
  -> ~is_training:bool
  -> tensor(append (init x.shape) [num_classes])

val alexnet
  :  num_classes:int
  -> Torch.Var_store.t
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 }
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; num_classes])
