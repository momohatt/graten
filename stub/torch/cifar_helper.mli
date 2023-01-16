val read_files
  :  ?dirname:string
  -> ?with_caching:bool
  -> unit
  -> { v:Dataset_helper.t | v.ntrain = 50000 && v.ntest = 10000 && v.image = [3; 32; 32] && v.label = [] }

val image_w     : { v:int | v = 32 }
val image_h     : { v:int | v = 32 }
val image_c     : { v:int | v = 3 }
val image_dim   : { v:int | v = 32 * 32 * 3 }
val label_count : { v:int | v = 10 }
