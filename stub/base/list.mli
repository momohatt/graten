val fold_i_t_t
  :  ~init:(list(int) * ({ v:tensor | 'b1#{v} } * { v:tensor | 'b2#{v} }))
  -> ~f:(list(int) * ({ v:tensor | 'b1#{v} } * { v:tensor | 'b2#{v} }) -> int -> list(int) * ({ v:tensor | 'b1#{v} } * { v:tensor | 'b2#{v} }))
  -> list(int)
  -> list(int) * ({ v:tensor | 'b1#{v} } * { v:tensor | 'b2#{v} })

val nth_exn : x:list(int) -> i:int -> { v:int | v = nth i x }
val hd_exn : x:list(int) -> { v:int | v = nth 0 x }
val hd_exn_t : x:list(tensor) -> tensor
val length : x:list(int) -> { v:int | v = len x }
val length_string : x:list(string) -> { v:int | v = len x }
val range : int -> int -> list(int)
