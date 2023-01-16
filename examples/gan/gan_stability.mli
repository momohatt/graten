val resnet_block
  :  Torch.Var_store.t
  -> ~input_dim:int
  -> output_dim:int
  -> x:{ v:tensor | nth 1 v.shape = input_dim && len v.shape = 4 }
  -> { v:tensor | v.shape = [nth 0 x.shape; output_dim; nth 2 x.shape; nth 3 x.shape] }

val create_generator
  :  Torch.Var_store.t
  -> { v:tensor | prod (append (init v.shape) [8192]) = 131072 && last v.shape = 128 }
  -> { v:tensor | v.shape = [16; 3; 128; 128] }

val create_discriminator
  :  Torch.Var_store.t
  -> { v:tensor | nth 0 v.shape * 512 * ((nth 2 v.shape + 31) // 32) * ((nth 3 v.shape + 31) // 32) = 131072 && nth 1 v.shape = 3 && len v.shape = 4 }
  -> { v:tensor | v.shape = [16; 1] }
