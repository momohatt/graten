open Base
open Torch

let batch_size = 128

module VAE = struct
  type t =
    { fc1  : x:{ v:tensor | last v.shape = 784 } -> tensor(init x.shape @ [400]) (* ANNOT: 2 *)
    ; fc21 : x:{ v:tensor | last v.shape = 400 } -> tensor(init x.shape @ [20])  (* ANNOT: 2 *)
    ; fc22 : x:{ v:tensor | last v.shape = 400 } -> tensor(init x.shape @ [20])  (* ANNOT: 2 *)
    ; fc3  : x:{ v:tensor | last v.shape = 20  } -> tensor(init x.shape @ [400]) (* ANNOT: 2 *)
    ; fc4  : x:{ v:tensor | last v.shape = 400 } -> tensor(init x.shape @ [784]) (* ANNOT: 2 *)
    }

  let create vs =
    { fc1  = Layer.linear vs ~input_dim:784 400
    ; fc21 = Layer.linear vs ~input_dim:400 20
    ; fc22 = Layer.linear vs ~input_dim:400 20
    ; fc3  = Layer.linear vs ~input_dim:20 400
    ; fc4  = Layer.linear vs ~input_dim:400 784
    }

  let encode t xs =
    let h1 = Layer.forward t.fc1 xs |> Tensor.relu in
    Layer.forward t.fc21 h1, Layer.forward t.fc22 h1

  let decode t zs =
    Layer.forward t.fc3 zs |> Tensor.relu |> Layer.forward t.fc4 |> Tensor.sigmoid

  let forward t xs =
    let mu, logvar = encode t (Tensor.view xs ~size:[ -1; 784 ]) in
    let std_ = Tensor.(exp (logvar * f 0.5)) in
    let eps = Tensor.randn_like std_ in
    decode t Tensor.(mu + (eps * std_)), mu, logvar
end

let loss ~recon_x ~x ~mu ~logvar =
  let bce =
    Tensor.bce_loss recon_x ~targets:(Tensor.view x ~size:[ -1; 784 ]) ~reduction:Sum
  in
  let kld = Tensor.(f (-. 0.5) * (f 1.0 + logvar - (mu * mu) - exp logvar) |> sum) in
  Tensor.( + ) bce kld

let write_samples samples ~filename =
  let samples = Tensor.(samples * f 256.) in
  List.init 8 ~f:(fun i ->
      List.init 8 ~f:(fun j ->
          Tensor.narrow samples ~dim:0 ~start:((4 * i) + j) ~length:1)
      |> Tensor.cat ~dim:2)
  |> Tensor.cat ~dim:3
  |> Torch_vision.Image.write_image ~filename

let () =
  let device = Device.cuda_if_available () in
  let mnist = Mnist_helper.read_files () in
  let vs = Var_store.create ~name:"vae" ~device () in
  let vae = VAE.create vs in
  let opt = Optimizer.adam vs ~learning_rate:1e-3 in
  for epoch_idx = 1 to 20 do
    let train_loss = Tensor.f 0. in
    let samples = Tensor.f 0. in
    Dataset_helper.iter
      mnist
      ~batch_size
      ~device
      ~f:(fun _ ~batch_images ~batch_labels:_ ->
        let recon_x, mu, logvar = VAE.forward vae batch_images in
        let loss = loss ~recon_x ~x:batch_images ~mu ~logvar in
        Optimizer.backward_step ~loss opt;
        Tensor.(train_loss += loss;
        samples += (shape batch_images |> List.hd_exn |> Float.of_int |> f)));
    Stdio.printf "epoch %4d  loss: %12.6f\n%!" epoch_idx (Tensor.float_value train_loss /. Tensor.float_value samples);
    Tensor.randn ~device [ 64; 20 ]
    |> VAE.decode vae
    |> Tensor.to_device ~device:Cpu
    |> Tensor.view ~size:[ -1; 1; 28; 28 ]
    |> write_samples ~filename:(Printf.sprintf "s_%d.png" epoch_idx)
  done
