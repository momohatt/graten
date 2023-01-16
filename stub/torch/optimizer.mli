type t

val adam
  :  ?beta1:float        (* default:  0.99 *)
  -> ?beta2:float        (* default:  0.999 *)
  -> ?weight_decay:float (* default: 0. *)
  -> ?eps:float          (* default: 1e-8 *)
  -> Var_store.t
  -> ~learning_rate:float
  -> t

val rmsprop
  :  ?alpha:float (* default: 0.99 *)
  -> ?eps:float   (* default: 1e-8 *)
  -> ?weight_decay:float (* default: 0. *)
  -> ?momentum:float     (* default: 0. *)
  -> ?centered:bool      (* default: false *)
  -> Var_store.t
  -> ~learning_rate:float
  -> t

val sgd
  :  ?momentum:float     (* default: 0. *)
  -> ?dampening:float    (* default: 0. *)
  -> ?weight_decay:float (* default: 0. *)
  -> ?nesterov:bool      (* default: false *)
  -> Var_store.t
  -> ~learning_rate:float
  -> t

type clip_grad =
  | Norm2 of float
  | Value of float

val step
  :  ?clip_grad:clip_grad
  -> t
  -> unit

val zero_grad : t -> unit

val backward_step
  :  ?clip_grad:clip_grad
  -> t
  -> ~loss:tensor([])
  -> unit

val set_learning_rate : t -> ~learning_rate:float -> unit

module Linear_interpolation : sig
  val create : list (float * float) -> t
  val eval : t -> float -> float
end
