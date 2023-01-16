type step =
  { obs : tensor
  ; reward : tensor
  ; is_done : tensor
  }

type t

val create : string -> ~num_processes:int -> t
val reset : t -> tensor
val step : t -> ~actions:list(int) -> step
val action_space : t -> int
