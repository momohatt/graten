val linear1 : x:{ v:tensor | last v.shape = 784 } -> tensor(append (init x.shape) [128])
val linear2 : x:{ v:tensor | last v.shape = 128 } -> tensor(append (init x.shape) [10])
val model   : x:{ v:tensor | last v.shape = 784 } -> tensor(append (init x.shape) [10])
