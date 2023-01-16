val model : xs:{ v:tensor | len v.shape = 2 && 784 = nth 1 v.shape } -> { v:tensor | v.shape = [nth 0 xs.shape; 10] }
