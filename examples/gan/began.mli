val upsample
  :  xs:{ v:tensor | len v.shape = 4 }
  -> { v:tensor | v.shape = [nth 0 xs.shape; nth 1 xs.shape; 2 * nth 2 xs.shape; 2 * nth 3 xs.shape] }

val avg_pool2d
  :  ?padding:int
  -> x:{ v:tensor | len v.shape = 4 }
  -> { v:tensor | v.shape = [nth 0 x.shape; nth 1 x.shape; (nth 2 x.shape + 2 * padding) // 2; (nth 3 x.shape + 2 * padding) // 2] }

val decoder
  :  Torch.Var_store.t
  -> { v:tensor | prod (append (init v.shape) [4096]) = 65536 && last v.shape = 64 }
  -> { v:tensor | v.shape = [16; 3; 128; 128] }

val encoder
  :  Torch.Var_store.t
  -> { v:tensor | nth 0 v.shape * (nth 2 v.shape // 16) * (nth 3 v.shape // 16) = 1024 && nth 1 v.shape = 3 && len v.shape = 4 }
  -> { v:tensor | v.shape = [16; 64] }

val create_discriminator
  :  Torch.Var_store.t
  -> { v:tensor | len v.shape = 4 && nth 1 v.shape = 3 && nth 0 v.shape * (nth 2 v.shape // 16) * (nth 3 v.shape // 16) = 1024 }
  -> { v:tensor | v.shape = [16; 3; 128; 128] }
