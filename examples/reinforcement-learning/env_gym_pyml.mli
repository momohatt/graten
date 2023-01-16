type step =
  { obs : tensor
  ; reward : float
  ; is_done : bool
  }

type t

val create : string -> ~action_repeat:int -> t
val reset : t -> tensor
val step : t -> ~action:int -> step
val actions : t -> list(string)
val lives : t -> int
