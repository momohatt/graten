val model1 : s:int -> { v:tensor | len v.shape = 1 && (nth 0 v.shape - 1) // s + 1 = 10 } -> tensor([10])
val model2 : int -> { v:tensor | len v.shape = 1 } -> tensor([10])
val model3 : s:int -> { v:tensor | len v.shape = 1 && (nth 0 v.shape - 1) // s + 1 = 10 } -> tensor([10])
