type t (ntrain, ntest, image, label) =
  { train_images : tensor(ntrain :: image)
  ; train_labels : tensor(ntrain :: label)
  ; test_images  : tensor(ntest :: image)
  ; test_labels  : tensor(ntest :: label)
  }

val train_batch
  :  ?device:Device.t
  (* -> ?augmentation:[ `flip | `crop_with_pad of int | `cutout of int ] list *)
  -> d:t
  -> ~batch_size:int
  -> ~batch_idx:int
  -> tensor(batch_size :: d.image) * tensor(batch_size :: d.label)

val batch_accuracy
  :  ?device:Device.t
  -> ?samples:int (* ntest if `test, ntrain if `train *)
  -> d:t
  -> string (* [ `test | `train ] *)
  -> ~batch_size:int
  -> ~predict:(tensor(batch_size :: d.image) -> { v:tensor | len v.shape = 2 && nth 0 v.shape = batch_size })
  -> float

val batches_per_epoch
  :  d:t
  -> ~batch_size:int
  -> { v:int | v = d.ntrain // batch_size }

val iter
  :  ?device:Device.t
  -> ?augmentation:list(string) (* [ `flip | `crop_with_pad of int | `cutout of int ] list *)
  -> ?shuffle:bool
  -> d:t
  -> ~batch_size:int
  -> ~f:(int -> batch_images:tensor(batch_size :: d.image) -> batch_labels:tensor(batch_size :: d.label) -> unit)
  -> unit

val map
  :  forall image label.
     ?device:Device.t
  -> d:t
  -> ~batch_size:int
  -> ~f:(int -> ~batch_images:tensor(batch_size :: d.image) -> ~batch_labels:tensor(batch_size :: d.label) -> tensor(batch_size :: image) * tensor(batch_size :: label))
  -> { v:t | v.ntrain = d.ntrain && v.ntest = d.ntest && v.image = image && v.label = label }

val print_summary : t -> unit
