val min : int -> int -> int
val max : int -> int -> int
val to_string : int -> string
val of_float  : float -> int
val ( + ) : x:int -> y:int -> { v:int | v = x + y  }
val ( - ) : x:int -> y:int -> { v:int | v = x - y  }
val ( * ) : x:int -> y:int -> { v:int | v = x * y  }
val ( / ) : x:int -> y:int -> { v:int | v = x // y }
val ( % ) : int -> int -> int
val ( ** ) : int -> int -> int
