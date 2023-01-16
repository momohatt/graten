val create_generator
  :  Torch.Var_store.t
  -> rand_input:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 128 }
  -> { v:tensor | v.shape = [nth 0 rand_input.shape; 3; 16 * nth 2 rand_input.shape + 48; 16 * nth 3 rand_input.shape + 48] }

val create_discriminator
  :  Torch.Var_store.t
  -> { v:tensor | len v.shape = 4 && nth 1 v.shape = 3 && nth 0 v.shape * ((nth 2 v.shape - 48) // 16) * ((nth 3 v.shape - 48) // 16) = 32 }
  -> { v:tensor | v.shape = [32] }

val rand : unit -> { v:tensor | v.shape = [32; 128; 1; 1] }
