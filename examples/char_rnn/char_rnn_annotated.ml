(* This example uses the tinyshakespeare dataset which can be downloaded at:
   https://raw.githubusercontent.com/karpathy/char-rnn/master/data/tinyshakespeare/input.txt

   It has been heavily inspired by https://github.com/karpathy/char-rnn
*)
open Base
open Torch

let learning_rate = 0.01
let hidden_size = 256
let seq_len = 180
let batch_size = 256
let epochs = 100
let sampling_length = 1024

(* EDIT: Reordered arguments *)
let sample ~dataset ~lstm ~linear:(linear : x:{ v:tensor | last v.shape = hidden_size } -> tensor(init x.shape @ [dataset.labels])) ~device = (* ANNOT: 2 *)
  let labels = Text_helper.labels dataset in
  let seq, _ =
    let zero_state = Layer.Lstm.zero_state lstm ~batch_size:1 in
    let rec loop seq state i =
      if i = 0
      then (seq, state)
      else (Caml.Gc.full_major ();
            let prev_label = List.hd_exn seq in
            let prev_y = Tensor.zeros [ 1; labels ] ~device in
            Tensor.fill_float (Tensor.narrow prev_y ~dim:1 ~start:prev_label ~length:1) 1.;
            let state = Layer.Lstm.step lstm state prev_y in
            (* let (`h_c (h, _)) = state in *)
            let (h, _) = state in
            let sampled_y =
              Layer.forward linear h
              |> Tensor.softmax ~dim:(-1) ~dtype:(T Float)
              |> Tensor.multinomial ~num_samples:1 ~replacement:false
            in
            let sampled_label =
              Tensor.get (Tensor.get sampled_y 0) 0 |> Tensor.int_value
            in loop (sampled_label :: seq) state (i - 1))
    in loop [0] zero_state sampling_length
  in
  List.rev_map seq ~f:(fun label -> Text_helper.char dataset ~label)
  |> String.of_char_list

let () =
  let device = Device.cuda_if_available () in
  let dataset = Text_helper.create ~filename:"data/input.txt" in
  let vs = Var_store.create ~name:"char-rnn" ~device () in
  let labels = Text_helper.labels dataset in
  Stdio.printf
    "Dataset loaded, length: %d, labels: %d.\n%!"
    (Text_helper.total_length dataset)
    labels;
  let lstm = Layer.Lstm.create vs ~input_dim:labels ~hidden_size in
  let linear = Layer.linear vs ~input_dim:hidden_size labels in
  let adam = Optimizer.adam vs ~learning_rate in
  let batches_per_epoch = (Text_helper.total_length dataset - seq_len) / batch_size in
  Checkpointing.loop
    ~start_index:1
    ~end_index:epochs
    ~var_stores:[ vs ]
    ~checkpoint_base:"char-rnn.ot"
    ~checkpoint_every:(`iters 1)
    (fun ~index:epoch_idx ->
      Stdio.Out_channel.write_all
        (Printf.sprintf "out.txt.%d" epoch_idx)
        ~data:(sample ~lstm ~linear ~dataset ~device);
      let start_time = Unix.gettimeofday () in
      (* EDIT: Replace ref float with a 0-dimensional tensor *)
      let sum_loss = Tensor.f 0. in
      Text_helper.iter ~device dataset ~batch_size ~seq_len ~f:(fun batch_idx ~xs ~ys ->
          Optimizer.zero_grad adam;
          let onehot =
            let xs = Tensor.view xs ~size:[ batch_size; seq_len; 1 ] in
            let one = Tensor.ones (Tensor.size xs) ~device in
            Tensor.zeros [ batch_size; seq_len; labels ] ~device
            |> Tensor.scatter_ ~dim:2 ~src:one ~index:xs
          in
          let lstm_out, _ = Layer.Lstm.seq lstm onehot ~is_training:true in
          let logits = Layer.forward linear lstm_out in
          (* Compute the cross-entropy loss. *)
          let loss =
            Tensor.cross_entropy_for_logits
              (Tensor.view logits ~size:[ batch_size * seq_len; labels ])
              ~targets:(Tensor.view ys ~size:[ batch_size * seq_len ])
          in
          (let open Tensor in sum_loss += loss);
          Stdio.printf
            "%d/%d %f\r%!"
            batch_idx
            batches_per_epoch
            (Tensor.float_value sum_loss /. Float.of_int (1 + batch_idx));
          Tensor.backward loss;
          Optimizer.step ~clip_grad:(Norm2 4.) adam);
      Stdio.printf
        "%d %.0fs %f\n%!"
        epoch_idx
        (Unix.gettimeofday () -. start_time)
        (Tensor.float_value sum_loss /. Float.of_int batches_per_epoch))
