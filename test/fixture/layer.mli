val l1 : x:int -> y:int -> z:{ v:tensor | last v.shape = x } -> tensor(append (init z.shape) [y])
val t1 : tensor([2; 3; 5])
val l2 : x:tensor -> tensor(x.shape)
val t2 : tensor([2; 3; 4])
val l3 : x:{ v:tensor | last v.shape = 4 } -> tensor(append (init x.shape) [5])
val l4 : x:int -> y:int -> tensor([2; 2; x // 2; y // 2])
val l5 : x:int -> y:int -> tensor([2; 2; (x + 2) // 2; (y + 2) // 2])
val l6 : x:int -> y:int -> tensor([2; 2; x - 1; y - 1])
val t3 : tensor([2; 2; 4; 4])
val l7 : int -> y:int -> tensor([32; y; 127; 127])
val l8 : x:int -> y:int -> z:{ v:tensor | len v.shape = 4 && nth 1 v.shape = x } -> tensor([nth 0 z.shape; y; nth 2 z.shape - 1; nth 3 z.shape - 1])
