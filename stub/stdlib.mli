val __neg__ : x:int -> { v:int | v = - x }
val ( + ) : x:int -> y:int -> { v:int | v = x + y  }
val ( - ) : x:int -> y:int -> { v:int | v = x - y  }
val ( * ) : x:int -> y:int -> { v:int | v = x * y  }
val ( / ) : x:int -> y:int -> { v:int | v = x // y }
val ( % ) : int -> int -> int
val ( ** ) : int -> int -> int

val __fneg__ : float -> float
val ( +. ) : float -> float -> float
val ( -. ) : float -> float -> float
val ( *. ) : float -> float -> float
val ( /. ) : float -> float -> float
val ( **. ) : float -> float -> float

val not    : x:bool -> { v:bool | v = !x }
val ( && ) : x:bool -> y:bool -> { v:bool | v = (x && y) }
val ( || ) : x:bool -> y:bool -> { v:bool | v = (x || y) }

val ( = )  : x:int -> y:int -> { v:bool | v = (x = y)  }
val ( <> ) : x:int -> y:int -> { v:bool | v = !(x = y) }
val ( < )  : x:int -> y:int -> { v:bool | v = (x < y)  }
val ( > )  : x:int -> y:int -> { v:bool | v = (x > y)  }
val ( <= ) : x:int -> y:int -> { v:bool | v = (x <= y) }
val ( >= ) : x:int -> y:int -> { v:bool | v = (x >= y) }

val ( ^ ) : string -> string -> string

type in_channel
type out_channel

module Unix : sig
  type process_status =
    | WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  val open_process_in   : string -> in_channel
  val open_process_out  : string -> in_channel
  val close_process_in  : in_channel -> process_status
  val close_process_out : out_channel -> process_status

  val gettimeofday : unit -> float
end
