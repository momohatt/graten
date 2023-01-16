type t (labels)

val create : ~filename:string -> t

val char : t -> ~label:int -> char

val total_length : t -> int

val labels : d:t -> { v:int | v = d.labels }

val iter
  :  ?device:Device.t
  -> t
  -> ~seq_len:int
  -> ~batch_size:int
  -> ~f:(int -> ~xs:tensor([batch_size; seq_len]) -> ~ys:tensor([batch_size; seq_len]) -> unit)
  -> unit
