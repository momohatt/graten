type t

(** [parse_config str] loads a darknet config file and parses it to a darknet config. *)
val parse_config : string -> t

val build_model : Torch.Var_store.t -> t -> tensor -> ~is_training:bool -> tensor
val width : t -> int
val height : t -> int
