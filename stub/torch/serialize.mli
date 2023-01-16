val save : tensor -> ~filename:string -> unit
val load : ~filename:string -> tensor

val load_multi_
  :  ~named_tensors:list (string * tensor)
  -> ~filename:string
  -> unit
