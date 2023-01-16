type dtype =
  | Uint8
  | Int8
  | Int16
  | Int
  | Int64
  | Half
  | Float
  | Double
  | ComplexHalf
  | ComplexFloat
  | ComplexDouble
  | Bool

type kind = T of dtype

(*** Tensor Creation ***)
val zeros
  :  ?requires_grad:bool (* default: false *)
  -> ?kind:kind
  -> ?device:Device.t
  -> ?scale:float         (* default: 1.0 *)
  -> shape:list(int)
  -> tensor(shape)

val ones
  :  ?requires_grad:bool (* default: false *)
  -> ?kind:kind
  -> ?device:Device.t
  -> ?scale:float        (* default: 1.0 *)
  -> shape:list(int)
  -> tensor(shape)

val rand
  :  ?requires_grad:bool (* default: false *)
  -> ?kind:kind
  -> ?device:Device.t
  -> ?scale:float        (* default: 1.0 *)
  -> shape:list(int)
  -> tensor(shape)

val randn
  :  ?requires_grad:bool (* default: false *)
  -> ?kind:kind
  -> ?device:Device.t
  -> ?scale:float        (* default: 1.0 *)
  -> shape:list(int)
  -> tensor(shape)

val randint
  :  ~high:int
  -> ~size:list(int)
  -> ~options:kind * Device.t
  -> tensor(size)

val float_vec : ?kind:kind -> ?device:Device.t -> list(float) -> tensor

val zeros_like : x:tensor -> tensor(x.shape)
val ones_like  : x:tensor -> tensor(x.shape)
val rand_like  : x:tensor -> tensor(x.shape)
val randn_like : x:tensor -> tensor(x.shape)

val copy  : x:tensor -> tensor(x.shape)
val copy_ : ~src:tensor -> x:{ v:tensor | broadcast v.shape src.shape = v.shape }-> unit

(*** Conversion ***)
val f : float -> tensor([])
val of_float0 : ?device:Device.t -> float -> tensor([])
val of_float1 : ?device:Device.t -> x:array (float) -> tensor([len x])
val of_float2 : ?device:Device.t -> array (array (float)) -> { v:tensor | len v.shape = 2 }
val of_float3 : ?device:Device.t -> array (array (array (float))) -> { v:tensor | len v.shape = 3 }
val of_int0   : ?device:Device.t -> int -> tensor([])
val of_int1   : ?device:Device.t -> x:array (int) -> tensor([len x])
val of_int2   : ?device:Device.t -> array (array (int)) -> { v:tensor | len v.shape = 2 }
val of_int3   : ?device:Device.t -> array (array (array (int))) -> { v:tensor | len v.shape = 3 }

val float_value : { v:tensor | prod v.shape = 1 } -> float
val int_value : { v:tensor | prod v.shape = 1 } -> int

val to_int0_exn : { v:tensor | prod v.shape = 1 } -> int
val to_int1_exn : { v:tensor | len v.shape = 1 } -> array (int)
val to_int2_exn : { v:tensor | len v.shape = 2 } -> array (array (int))
val to_int3_exn : { v:tensor | len v.shape = 3 } -> array (array (array (int)))
val to_float0_exn : { v:tensor | prod v.shape = 1 } -> float
val to_float1_exn : { v:tensor | len v.shape = 1 } -> array (float)
val to_float2_exn : { v:tensor | len v.shape = 2 } -> array (array (float))
val to_float3_exn : { v:tensor | len v.shape = 3 } -> array (array (array (float)))

val get        : x:tensor -> i:int -> tensor(tail x.shape)
val get_float2 : { v:tensor | len v.shape = 2 } -> int -> int -> float
val get_float1 : { v:tensor | len v.shape = 1 } -> int -> float
val get_int2   : { v:tensor | len v.shape = 2 } -> int -> int -> int
val get_int1   : { v:tensor | len v.shape = 1 } -> int -> int
val set_float2 : { v:tensor | len v.shape = 2 } -> int -> int -> float -> unit
val set_float1 : { v:tensor | len v.shape = 1 } -> int -> float -> unit
val set_int2   : { v:tensor | len v.shape = 2 } -> int -> int -> int -> unit
val set_int1   : { v:tensor | len v.shape = 1 } -> int -> int -> unit

val fill_float : tensor -> float -> unit
val fill_int   : tensor -> int -> unit
val masked_fill : ~mask:tensor -> ~value:tensor([]) -> x:{ v:tensor | broadcastable v.shape mask.shape } -> tensor(broadcast x.shape mask.shape)
val clamp : ~min:tensor([]) -> ~max:tensor([]) -> x:tensor -> tensor(x.shape)

val to_kind : ~kind:kind -> x:tensor -> tensor(x.shape)
val to_type : ~type_:kind -> x:tensor -> tensor(x.shape)

(*** Gradient ***)
val backward  : ?keep_graph:bool -> tensor([]) -> unit

val grad : x:tensor -> tensor(x.shape)

val no_grad_u : (unit -> unit) -> unit
val no_grad_t : (unit -> { v:tensor | 'b#{v} }) -> { v:tensor | 'b#{v} }

val zero_grad : tensor -> unit

val set_requires_grad : x:tensor -> ~r:bool -> tensor(x.shape)

(* TODO: give more precise type signature *)
val run_backward : ?keep_graph:bool -> ?create_graph:bool -> list (tensor) -> list (tensor) -> list (tensor)

val detach : x:tensor -> tensor(x.shape)

(*** Misc ***)
val shape : x:tensor -> { v:list(int) | v = x.shape }
val size  : x:tensor -> { v:list(int) | v = x.shape }
val shape2_exn : x:{ v:tensor | len v.shape = 2 } -> { v:int | v = nth 0 x.shape } * { v:int | v = nth 1 x.shape }
val shape3_exn : x:{ v:tensor | len v.shape = 3 } -> { v:int | v = nth 0 x.shape } * { v:int | v = nth 1 x.shape } * { v:int | v = nth 2 x.shape }
val shape4_exn : x:{ v:tensor | len v.shape = 4 } -> { v:int | v = nth 0 x.shape } * { v:int | v = nth 1 x.shape } * { v:int | v = nth 2 x.shape } * { v:int | v = nth 3 x.shape }

val print : tensor -> unit
val print_shape : ~name:string -> tensor -> unit

val to_device : ~device:Device.t -> x:tensor -> tensor(x.shape)

val contiguous : x:tensor -> tensor(x.shape)

(*** Basic math functions ***)
val ( + ) : x:tensor -> y:{ v:tensor | broadcastable x.shape v.shape } -> tensor(broadcast x.shape y.shape)
val ( - ) : x:tensor -> y:{ v:tensor | broadcastable x.shape v.shape } -> tensor(broadcast x.shape y.shape)
val ( * ) : x:tensor -> y:{ v:tensor | broadcastable x.shape v.shape } -> tensor(broadcast x.shape y.shape)
val ( / ) : x:tensor -> y:{ v:tensor | broadcastable x.shape v.shape } -> tensor(broadcast x.shape y.shape)
val ( += ) : x:tensor -> { v:tensor | broadcast x.shape v.shape = x.shape } -> unit
val ( -= ) : x:tensor -> { v:tensor | broadcast x.shape v.shape = x.shape } -> unit
val ( *= ) : x:tensor -> { v:tensor | broadcast x.shape v.shape = x.shape } -> unit
val ( /= ) : x:tensor -> { v:tensor | broadcast x.shape v.shape = x.shape } -> unit

val __neg__ : x:tensor -> tensor(x.shape)
val ( ~- ) : x:tensor -> tensor(x.shape)
val ( = ) : x:tensor -> tensor(x.shape) -> tensor(x.shape)
val eq_scalar : x:tensor -> tensor([]) -> tensor(x.shape)

val abs    : x:tensor -> tensor(x.shape)
val exp    : x:tensor -> tensor(x.shape)
val log    : x:tensor -> tensor(x.shape)
val square : x:tensor -> tensor(x.shape)
val sqrt   : x:tensor -> tensor(x.shape)
val tan    : x:tensor -> tensor(x.shape)
val tanh   : x:tensor -> tensor(x.shape)

val scale : x:tensor -> float -> tensor(x.shape)

val tril : ?diagonal:int -> x:{ v:tensor | len v.shape = 2 } -> tensor(x.shape)
val triu : ?diagonal:int -> x:{ v:tensor | len v.shape = 2 } -> tensor(x.shape)

val max : x:tensor -> y:{ v:tensor| broadcastable x.shape v.shape } -> tensor(broadcast x.shape y.shape)
val min : x:tensor -> y:{ v:tensor| broadcastable x.shape v.shape } -> tensor(broadcast x.shape y.shape)
val maximum : x:tensor -> tensor([])
val minimum : x:tensor -> tensor([])

val mm
  :  x:{ v:tensor | len v.shape = 2 }
  -> y:{ v:tensor([nth 1 x.shape; nth 1 v.shape]) | true }
  -> tensor([nth 0 x.shape; nth 1 y.shape])

val bmm
  :  x:{ v:tensor | len v.shape = 3 }
  -> ~mat2:{ v:tensor | len v.shape = 3 && nth 0 v.shape = nth 0 x.shape && nth 1 v.shape = nth 2 x.shape }
  -> tensor([nth 0 x.shape; nth 1 x.shape; nth 2 mat2.shape])

val matmul
  :  x:tensor
  -> y:{ v:tensor | broadcastable (init (init x.shape)) (init (init v.shape)) &&
                    (* |nth (-2) S| is the last element of |S| if |len S = 1| *)
                    last x.shape = nth (-2) v.shape }
  -> tensor(broadcast (init (init x.shape)) (init (init y.shape)) @ matmul x.shape y.shape)

(*** Tensor Manipulation ***)
val reshape
  :  ~shape:list(int)
  -> x:{ v:tensor | reshapeable v.shape shape }
  -> tensor(reshape x.shape shape)

val view
  :  ~size:list(int)
  -> x:{ v:tensor | reshapeable v.shape size }
  -> tensor(reshape x.shape size)

val flatten : x:tensor -> tensor([nth 0 x.shape; prod (tail x.shape)])

val select : ~dim:int -> ~index:int -> x:tensor -> tensor(dropAt dim x.shape)

val index_select
  :  ~dim:int
  -> ~index:{ v:tensor | len v.shape = 1 }
  -> x:tensor
  -> tensor(insertAt dim (nth 0 index.shape) (dropAt dim x.shape))

val gather
  :  x:tensor
  -> ~dim:int
  -> ~index:{ v:tensor | len v.shape = len x.shape }
  -> ~sparse_grad:bool
  -> tensor(insertAt dim (nth dim index.shape) (dropAt dim x.shape))

val stack : ~dim:int -> list(tensor) -> tensor

val transpose
  :  ~dim0:int
  -> ~dim1:int
  -> x:tensor
  -> tensor(swap dim0 dim1 x.shape)

val tr
  :  x:{ v:tensor | len v.shape = 2 }
  -> tensor([nth 1 x.shape; nth 0 x.shape])

val narrow
  :  x:tensor
  -> ~dim:int
  -> ~start:int
  -> ~length:int
  -> tensor(insertAt dim length (dropAt dim x.shape))

(* TODO: give precise type for squeeze *)
val squeeze      : tensor -> tensor
val squeeze_dim  : x:tensor -> ~dim:int -> tensor(dropAt dim x.shape)
val squeeze_last : x:{ v:tensor | last v.shape = 1 } -> tensor(dropAt (-1) x.shape)
val unsqueeze    : x:tensor -> ~dim:int -> tensor(insertAt dim 1 x.shape)


val expand
  :  tensor
  -> ~size:list(int)  (* TODO: constraints of size *)
  -> ~implicit:bool
  -> tensor(size)

val cat : ~dim:int -> list (tensor) -> tensor

(* Precise variant of Tensor.cat *)
val cat_
  :  ~dim:int
  -> x:tensor
  -> y:{ v:tensor | dropAt dim v.shape = dropAt dim x.shape }
  -> tensor(insertAt dim (nth dim x.shape + nth dim y.shape) (dropAt dim x.shape))

val scatter_ : ~dim:int -> ~src:tensor -> ~index:tensor -> x:tensor -> tensor(x.shape)

val topk
  :  x:tensor -> ~k:int -> ~dim:int -> ~largest:bool -> ~sorted:bool
  -> tensor(insertAt dim k (dropAt dim x.shape)) * tensor(insertAt dim k (dropAt dim x.shape))

(*** Reduction ***)
val sum : tensor -> tensor([])
val mean : tensor -> tensor([])

val sum_dim_intlist
  :  x:tensor
  -> ~dim:list(int)
  -> ~keepdim:bool
  -> ~dtype:kind
  -> tensor(dropAllAt dim x.shape) (* keepdim = false *)

val mean_dim
  :  x:tensor
  -> ~dim:list(int)
  -> ?keepdim:bool
  -> ?dtype:kind
  -> tensor(insertAllAt dim 1 (dropAllAt dim x.shape)) (* keepdim = true *)

val argmax
  :  ?dim:int = { v:int | v = -1 }
  -> ?keepdim:bool
  -> x:tensor
  -> tensor(dropAt dim x.shape) (* TODO: keepdim *)

(*** Loss function ***)
type reduction =
  | None
  | Elementwise_mean
  | Sum

val softmax              : x:tensor -> ~dim:int -> ~dtype:kind -> tensor(x.shape)
val log_softmax          : x:tensor -> ~dim:int -> ~dtype:kind -> tensor(x.shape)
val bce_loss             : ?reduction:reduction -> x:tensor -> ~targets:{ v:tensor | broadcastable x.shape v.shape } -> tensor([])
val bce_loss_with_logits : x:tensor -> ~targets:{ v:tensor | broadcastable x.shape v.shape } -> tensor([])
val mse_loss             : x:tensor -> { v:tensor | broadcastable x.shape v.shape } -> tensor([])

val nll_loss                 : x:{ v:tensor | len v.shape = 2 } -> ~targets:tensor([nth 0 x.shape]) -> tensor([])
val cross_entropy_for_logits : x:{ v:tensor | len v.shape = 2 } -> ~targets:tensor([nth 0 x.shape]) -> tensor([])

val multinomial : x:tensor -> ~num_samples:int -> ?replacement:bool -> tensor(init x.shape @ [num_samples])

(*** Layers ***)
val dropout
  :  ~p:float (* dropout prob *)
  -> ~is_training:bool
  -> x:tensor
  -> tensor(x.shape)

val elu   : x:tensor -> tensor(x.shape)
val elu_  : x:tensor -> tensor(x.shape)
val celu  : x:tensor -> tensor(x.shape)
val celu_ : x:tensor -> tensor(x.shape)
val gelu  : x:tensor -> tensor(x.shape)
val relu  : x:tensor -> tensor(x.shape)
val relu_ : x:tensor -> tensor(x.shape)
val selu  : x:tensor -> tensor(x.shape)
val selu_ : x:tensor -> tensor(x.shape)
val sigmoid : x:tensor -> tensor(x.shape)
val leaky_relu  : x:tensor -> tensor(x.shape)
val leaky_relu_ : x:tensor -> tensor(x.shape)

val max_pool2d
  :  ?padding:int * int = { v:int | v = 0 } * { v:int | v = 0 }
  -> ?ceil_mode:bool
  -> ~ksize:int * int
  -> ?stride:int * int = { v:int | v = ksize._1 } * { v:int | v = ksize._2 }
  -> x:{ v:tensor | len v.shape = 4 }
  -> tensor([nth 0 x.shape; nth 1 x.shape; (nth 2 x.shape + 2 * padding._1 - ksize._1) // stride._1 + 1; (nth 3 x.shape + 2 * padding._2 - ksize._2) // stride._2 + 1])

val avg_pool2d
  :  ?padding:int * int = { v:int | v = 0 } * { v:int | v = 0 }
  -> ?ceil_mode:bool
  -> ~ksize:int * int
  -> ?stride:int * int = { v:int | v = ksize._1 } * { v:int | v = ksize._2 }
  -> x:{ v:tensor | len v.shape = 4 }
  -> tensor([nth 0 x.shape; nth 1 x.shape; (nth 2 x.shape + 2 * padding._1 - ksize._1) // stride._1 + 1; (nth 3 x.shape + 2 * padding._2 - ksize._2) // stride._2 + 1])

val adaptive_avg_pool2d
  :  ~output_size:list(int)
  -> x:{ v:tensor | len v.shape = 4 }
  -> tensor([nth 0 x.shape; nth 1 x.shape] @ output_size)

val upsample_nearest2d
  :  ~output_size:list(int)
  -> ~scales_h:float
  -> ~scales_w:float
  -> x:{ v:tensor | len v.shape = 4 }
  -> tensor([nth 0 x.shape; nth 1 x.shape] @ output_size)

val const_batch_norm : ?momentum:float -> ?eps:float -> x:tensor -> tensor(x.shape)
