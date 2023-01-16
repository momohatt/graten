val sample
  :  ~dataset:Torch.Text_helper.t
  -> ~lstm:{ v:Torch.Layer.Lstm.t | v.input_size = dataset.labels && v.hidden_size = 256 }
  -> ~linear:(x:{ v:tensor | last v.shape = 256 }
  -> { v:tensor | v.shape = append (init x.shape) [labels] })
  -> ~device:Torch.Device.t -> string
