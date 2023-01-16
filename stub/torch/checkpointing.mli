val loop
  :  ~start_index:int
  -> ~end_index:int
  -> ~var_stores:list(Var_store.t)
  -> ~checkpoint_base:string
  -> ?only_keep:int
  -> ?checkpoint_every:string (* [ `iters of int | `seconds of float ] *) (* default : `second 600 *)
  -> (~index:int -> unit)
  -> unit
