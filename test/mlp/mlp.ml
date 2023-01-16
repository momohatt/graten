open Torch

let net n_units n_out =
  let device = Device.cuda_if_available () in
  let vs = Var_store.create ~device ~name:"mlp" () in
  let l1 = Layer.linear vs ~input_dim:3 n_units in
  let l2 = Layer.linear vs ~input_dim:n_units n_out in
  fun x t ->
    Layer.forward l1 x
    |> Layer.forward l2
    |> Tensor.nll_loss ~targets:t
