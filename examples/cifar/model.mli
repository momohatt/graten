type t =
  { model_name : string
  ; model : x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 && (nth 2 v.shape // 32) * (nth 3 v.shape // 32) = 1 }
            -> ~is_training:bool
            -> tensor([nth 0 x.shape; 10])
  ; epochs : int
  ; lr_schedule : ~batch_idx:int -> ~batches_per_epoch:int -> ~epoch_idx:int -> float
  ; batch_size : int
  }
