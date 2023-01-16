val read_files
  :  ?prefix:string
  -> unit
  -> { v:Dataset_helper.t | v.ntrain = 60000 && v.ntest = 10000 && v.image = [28 * 28] && v.label = [] }

val image_w     : { v:int | v = 28 }
val image_h     : { v:int | v = 28 }
val image_dim   : { v:int | v = 28 * 28 }
val label_count : { v:int | v = 10 }
