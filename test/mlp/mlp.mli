val net
  :  int
  -> int
  -> x:{ v:tensor | len v.shape = 2 && nth 1 v.shape = 3 }
  -> tensor([nth 0 x.shape])
  -> tensor([])
