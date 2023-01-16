val w_scale_layer
  :  Torch.Var_store.t
  -> ~size:int
  -> x:{ v:tensor | nth 1 v.shape = sz && len v.shape = 4 }
  -> { v:tensor | v.shape = [nth 0 x.shape; nth 1 x.shape; nth 2 x.shape; nth 3 x.shape] }

val norm_conv_block
  :  ~vs:Torch.Var_store.t
  -> ~ksize:int -> ~padding:int -> ~input_dim:int -> dim:int
  -> x:{ v:tensor | nth 1 v.shape = input_dim && len v.shape = 4 }
  -> { v:tensor | v.shape = [nth 0 x.shape; dim; (nth 2 x.shape + 2 * padding - ksize) + 1; (nth 3 x.shape + 2 * padding - ksize) + 1] }

val norm_upscale_conv_block
  :  ~vs:Torch.Var_store.t
  -> ~ksize:int -> ~padding:int -> ~input_dim:int -> dim:int
  -> x:{ v:tensor | nth 1 v.shape = input_dim && len v.shape = 4 }
  -> { v:tensor | v.shape = [nth 0 x.shape; dim; (2 * nth 2 x.shape + 2 * padding - ksize) + 1; (2 * nth 3 x.shape + 2 * padding - ksize) + 1] }

val create_generator
  :  Torch.Var_store.t
  -> x:{ v:tensor | nth 1 v.shape = 512 && len v.shape = 4 }
  -> { v:tensor | v.shape = [nth 0 x.shape; 3; 256 * nth 2 x.shape + 768; 256 * nth 3 x.shape + 768] }
