val f1 : tensor([24; 24]) -> tensor([24; 24])
val f2 : tensor([32; 1; 24; 24]) -> tensor([32; 1; 24; 24])
val f3 : x:tensor -> tensor(x.shape)
val f4 : x:{ v:tensor | len v.shape = 2 } -> tensor([nth 0 x.shape; nth 1 x.shape])
