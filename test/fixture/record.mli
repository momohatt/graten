val x : { v:t | v.a = 1 }
val y : { v:int | v = 1 }
val z : { v:int | v = 3 }
val f : x:t -> { v:int | v = x.a + 1 }
val g : x:t -> { v:t | v.a = x.a }
val w : { v:int | v = 2 }

val x2 : { v:t2 | v.a = 2 }
(* TODO: Pass z2. *)
(* val z2 : tensor([2; nth 1 v.shape]) *)
