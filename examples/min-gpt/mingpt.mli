val causal_self_attention
  :  Torch.Var_store.t
  -> cfg:{ v:config | v.n_embd = v.n_head * (v.n_embd // v.n_head) }
  -> x:tensor([nth 0 v.shape; cfg.block_size; cfg.n_embd])
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; nth 1 x.shape; nth 2 x.shape])

val block
  :  Torch.Var_store.t
  -> cfg:{ v:config | v.n_embd = v.n_head * (v.n_embd // v.n_head) }
  -> x:tensor([nth 0 v.shape; cfg.block_size; cfg.n_embd])
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; nth 1 x.shape; nth 2 x.shape])

val gpt
  :  Torch.Var_store.t
  (* TODO: Why doesn't inference result catch v.n_embd % v.n_head = 0 ? *)
  -> cfg:config
  -> x:tensor([nth 0 v.shape; cfg.block_size])
  -> ~is_training:bool
  -> tensor([nth 0 x.shape; nth 1 x.shape; cfg.vocab_size])

val sample
  :  Torch.Var_store.t
  -> cfg:config
  -> (x:tensor([nth 0 v.shape; cfg.block_size]) -> bool -> tensor([nth 0 x.shape; cfg.block_size; cfg.vocab_size]))
  -> Torch.text_helper.t
  -> Torch.Device.t
  -> string

val train
  :  Torch.Var_store.t
  -> cfg:{ v:config | v.block_size = 128 }
  -> (x:tensor([nth 0 v.shape; 128]) -> ~is_training:bool -> tensor([nth 0 x.shape; nth 1 x.shape; cfg.vocab_size]))
  -> Torch.text_helper.t
  -> unit
