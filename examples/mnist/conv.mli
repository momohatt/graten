val model
  :  ~is_training:bool
  -> x:{ v:tensor | prod v.shape = 784 * (prod v.shape // 784) }
  -> tensor([prod x.shape // 784; 10])
