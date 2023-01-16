val f1 : ?x:int = { v:int | v = 3 } -> ?y:int = { v:int | v = 5 } -> z:int -> tensor([x; y; z])
val t1 : tensor([4; 6; 7])
val t2 : tensor([3; 6; 7])
val t3 : tensor([3; 5; 7])
val t4 : tensor([4; 5; 7])
val f2 : x:int -> tensor([x])
val t5 : tensor([5])
val f3 : ?x:int -> int
val t6 : int
