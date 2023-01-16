module Alexnet : sig
  (* Although num_classes does not have default values in ocaml-torch,
   * it makes no difference type-wise to define the default value as 512. *)
  val alexnet : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end

module Densenet : sig
  val densenet121 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val densenet161 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val densenet169 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val densenet201 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end

module Efficientnet : sig
  val b0 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val b1 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val b2 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val b3 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val b4 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val b5 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val b6 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val b7 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end

module Image : sig
  val resize :  x:{ v:tensor | len v.shape = 4 } -> ~width:int -> ~height:int
             -> tensor([nth 0 x.shape; nth 1 x.shape; height; width])
  val load_image : ?resize:int * int -> string -> tensor
  val write_image : tensor -> ~filename:string -> unit
end

module Imagenet : sig
  val load_dataset
    :  ~dir:string
    -> ~classes:list (string)
    -> ?with_cache:string
    -> unit
    -> Torch.Dataset_helper.t

  val load_image : ?resize:int * int -> string -> tensor

  module Classes : sig
    val count : { v:int | v = 1000 }
    val names : array (string)
    val top : { v:tensor | v.shape = [1000] || v.shape = [1; 1000] || v.shape = [1; 1; 1000] } -> ~k:int -> list (string * float)
  end
end

module Inception : sig
  val v3 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end

module Mobilenet : sig
  val v2 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end

module Resnet : sig
  val resnet18  : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val resnet34  : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val resnet50  : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val resnet101 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val resnet152 : ?num_classes:int = { v:int | v = 512 } -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end

module Squeezenet : sig
  val squeezenet1_0 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val squeezenet1_1 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end

module Vgg : sig
  val vgg11 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val vgg13 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val vgg16 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
  val vgg19 : ~num_classes:int -> Torch.Var_store.t -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = 3 } -> ~is_training:bool -> tensor([nth 0 x.shape; num_classes])
end
