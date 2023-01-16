type t

val create
  :  ?frozen:bool (* default: false *)
  -> ?device:Device.t (* Device.cpu *)
  -> ~name:string
  -> unit
  -> t

val sub    : t -> string -> t
val subi   : t -> int -> t
val ( / )  : t -> string -> t
val ( // ) : t -> int -> t

val all_vars : t -> list (string * tensor)

val device : t -> Device.t

val new_var
  :  ?trainable:bool (* default: true *)
  -> t
  -> ~shape:list(int)
  -> ~init:Init.t
  -> ~name:string
  -> tensor(shape)

val freeze : t -> unit
val unfreeze : t -> unit

module Init : sig
  type t =
    | Zeros
    | Ones
    | Const of float
    (* TODO (gan/realistic_dcgan) *)
    | Normal (* of
        { mean : float
        ; stdev : float
        } *)
    | Uniform of float * float
    | Copy of tensor
end
