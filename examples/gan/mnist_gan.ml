(* Generative Adversarial Networks trained on the MNIST dataset. *)
open Base
open Torch

let image_dim = Mnist_helper.image_dim
let latent_dim = 100
let generator_hidden_nodes = 128
let discriminator_hidden_nodes = 128
let batch_size = 128
let learning_rate = 1e-4
let batches = 10 ** 8

let create_generator vs =
  let linear1 =
    Layer.linear vs generator_hidden_nodes ~activation:Leaky_relu ~input_dim:latent_dim
  in
  let linear2 =
    Layer.linear vs image_dim ~activation:Tanh ~input_dim:generator_hidden_nodes
  in
  fun rand_input -> Layer.forward linear1 rand_input |> Layer.forward linear2

let create_discriminator vs =
  let linear1 =
    Layer.linear vs discriminator_hidden_nodes ~activation:Leaky_relu ~input_dim:image_dim
  in
  let linear2 =
    Layer.linear vs 1 ~activation:Sigmoid ~input_dim:discriminator_hidden_nodes
  in
  fun xs -> Layer.forward linear1 xs |> Layer.forward linear2

let bce ?(epsilon = 1e-7) ~labels model_values =
  Tensor.(
    -((f labels * log (model_values + f epsilon))
     + (f (1. -. labels) * log (f (1. +. epsilon) - model_values))))
  |> Tensor.mean

let rand () = Tensor.((f 2. * rand [ batch_size; latent_dim ]) - f 1.)

let write_samples samples ~filename =
  Stdio.Out_channel.with_file filename ~f:(fun channel ->
      Stdio.Out_channel.output_string channel "data_ = [\n";
      for sample_index = 0 to 99 do
        List.init image_dim ~f:(fun pixel_index ->
            Tensor.get_float2 samples sample_index pixel_index |> Printf.sprintf "%.2f")
        |> String.concat ~sep:", "
        |> Printf.sprintf "  [%s],\n"
        |> Stdio.Out_channel.output_string channel
      done;
      Stdio.Out_channel.output_string channel "]\n")

let () =
  let mnist = Mnist_helper.read_files () in
  let generator_vs = Var_store.create ~name:"gen" () in
  let generator = create_generator generator_vs in
  let opt_g = Optimizer.adam generator_vs ~learning_rate in
  let discriminator_vs = Var_store.create ~name:"disc" () in
  let discriminator = create_discriminator discriminator_vs in
  let opt_d = Optimizer.adam discriminator_vs ~learning_rate in
  let fixed_noise = rand () in
  for batch_idx = 1 to batches do
    let batch_images, _ = Dataset_helper.train_batch mnist ~batch_size ~batch_idx in
    let discriminator_loss =
      Tensor.( + )
        (bce ~labels:0.9 (discriminator Tensor.((f 2. * batch_images) - f 1.)))
        (bce ~labels:0.0 (rand () |> generator |> discriminator))
    in
    Optimizer.backward_step ~loss:discriminator_loss opt_d;
    let generator_loss = bce ~labels:1. (rand () |> generator |> discriminator) in
    Optimizer.backward_step ~loss:generator_loss opt_g;
    if batch_idx % 100 = 0
    then
      Stdio.printf
        "batch %4d    d-loss: %12.6f    g-loss: %12.6f\n%!"
        batch_idx
        (Tensor.float_value discriminator_loss)
        (Tensor.float_value generator_loss);
    Caml.Gc.full_major ();
    if batch_idx % 100000 = 0 || (batch_idx < 100000 && batch_idx % 25000 = 0)
    then
      write_samples
        (generator fixed_noise)
        ~filename:(Printf.sprintf "out%d.txt" batch_idx)
  done
