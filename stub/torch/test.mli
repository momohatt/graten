val layer1
  :  s:int
  -> x:{ v:tensor | len v.shape = 1 }
  -> tensor([(nth 0 x.shape - 1) // s + 1])
