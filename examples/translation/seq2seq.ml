(* Translation with a Sequence to Sequence Model and Attention.

   This follows the line of the PyTorch tutorial:
   https://pytorch.org/tutorials/intermediate/seq2seq_translation_tutorial.html
   And trains a Sequence to Sequence (seq2seq) model using attention to
   perform translation between French and English.

   The dataset can be downloaded from the following link:
   https://download.pytorch.org/tutorial/data.zip
   The eng-fra.txt file should be moved in the data directory.
*)
open Base
open Torch

(* Texts with more than this number of words will be discarded. *)
let max_length = 10

(* The seq2seq model that we build uses an encoder based on a GRU to
   produce a vector representing the whole input text.
   This vector is the final hidden state of the GRU and is passed as
   initial state to the decoder which is also based on a GRU.
*)
module Enc = struct
  type t =
    { forward : tensor -> ~hidden:tensor -> tensor * tensor
    ; zero_state : tensor
    }

  let create vs ~input_size ~hidden_size =
    let embedding =
      Layer.embeddings vs ~num_embeddings:input_size ~embedding_dim:hidden_size
    in
    let gru = Layer.Gru.create vs ~input_dim:hidden_size ~hidden_size in
    let forward input_ ~hidden =
      let hidden =
        Layer.forward embedding input_
        |> Tensor.view ~size:[ 1; -1 ]
        |> Layer.Gru.step gru hidden
      in
      let out = hidden in
      hidden, out
    in
    { forward; zero_state = Layer.Gru.zero_state gru ~batch_size:1 }

  let to_tensor state = state
end

(* Decoder module without the attention part. *)
module Dec = struct
  (* Precise type annotations aren't needed since this module is not used. *)
  type t =
    { forward :  input_:tensor -> ~hidden:tensor -> ~enc_outputs:tensor -> ~is_training:bool -> tensor * tensor
    ; zero_state : tensor
    }

  let create vs ~hidden_size ~output_size =
    let embedding =
      Layer.embeddings vs ~num_embeddings:output_size ~embedding_dim:hidden_size
    in
    let gru = Layer.Gru.create vs ~input_dim:hidden_size ~hidden_size in
    let linear = Layer.linear vs ~input_dim:hidden_size output_size in
    let forward input_ ~hidden ~enc_outputs:_ ~is_training:_ =
      let hidden =
        Layer.forward embedding input_
        |> Tensor.view ~size:[ 1; -1 ]
        |> Tensor.relu
        |> Layer.Gru.step gru hidden
      in
      let out = hidden in
      hidden, Layer.forward linear out |> Tensor.log_softmax ~dim:(-1) ~dtype:(T Float)
    in
    { forward; zero_state = Layer.Gru.zero_state gru ~batch_size:1 }

  let of_tensor state = (* `state *) state
end

(* Decoder module with the attention part. *)
module D = struct
  type t =
    { forward :  input_:tensor -> ~hidden:tensor -> ~enc_outputs:tensor
              -> ~is_training:bool -> tensor * tensor
    ; zero_state : tensor
    }

  let create vs ~hidden_size ~output_size =
    let device = Var_store.device vs in
    let embedding =
      Layer.embeddings vs ~num_embeddings:output_size ~embedding_dim:hidden_size
    in
    let gru = Layer.Gru.create vs ~input_dim:hidden_size ~hidden_size in
    let attn = Layer.linear vs ~input_dim:(hidden_size * 2) max_length in
    let attn_combine = Layer.linear vs ~input_dim:(hidden_size * 2) hidden_size in
    let linear = Layer.linear vs ~input_dim:hidden_size output_size in
    let forward input_ ~hidden ~enc_outputs ~is_training =
      let embedded =
        Layer.forward embedding input_
        |> Tensor.dropout ~p:0.1 ~is_training
        |> Tensor.view ~size:[ 1; -1 ]
      in
      let hidden_tensor = hidden in
      let attn_weights =
        Tensor.cat_ embedded hidden_tensor ~dim:1
        |> Layer.forward attn
        |> Tensor.unsqueeze ~dim:0
      in
      let sz1, sz2, sz3 = Tensor.shape3_exn enc_outputs in
      let enc_outputs =
        if sz2 = max_length
        then enc_outputs
        else
          Tensor.cat_ ~dim:1
            enc_outputs (Tensor.zeros ~device [ sz1; max_length - sz2; sz3 ])
      in
      let attn_applied =
        Tensor.bmm attn_weights ~mat2:enc_outputs |> Tensor.squeeze_dim ~dim:1
      in
      let output =
        Tensor.cat_ embedded attn_applied ~dim:1
        |> Layer.forward attn_combine
        |> Tensor.relu
      in
      let hidden = Layer.Gru.step gru hidden output in
      let hidden_tensor = hidden in
      ( hidden
      , Layer.forward linear hidden_tensor
        |> Tensor.log_softmax ~dim:(-1) ~dtype:(T Float) )
    in
    { forward; zero_state = Layer.Gru.zero_state gru ~batch_size:1 }

  let of_tensor tensor = (* `state *) tensor
end

(* Apply the model to an input and get back the predicted word indexes. *)
let predict ~input_ ~enc ~dec ~dec_start ~dec_eos ~device =
  let enc_final, enc_outputs =
    let rec loop
    = fun state idx outs ->
      if idx = List.length input_
      then (state, outs)
      else
        let input_tensor = Tensor.of_int1 [| idx |] ~device in
        let (hidden, out) = Enc.(enc.forward) input_tensor ~hidden:state in
        (loop hidden (idx + 1) (out :: outs))
    in loop Enc.(enc.zero_state) 0 []
  in
  let enc_outputs = Tensor.stack enc_outputs ~dim:1 in
  let dec_state = Enc.to_tensor enc_final |> D.of_tensor in
  let rec loop
  = fun ~state ~prevs ~max_length ->
    (let state, output =
      match prevs with
      | prev :: _ -> D.(dec.forward) prev ~hidden:state ~enc_outputs ~is_training:false
    in
    let _, output = Tensor.topk output ~k:1 ~dim:(-1) ~largest:true ~sorted:true in
    if Tensor.to_int0_exn output = dec_eos || max_length = 0
    then List.rev prevs
    else loop ~state ~prevs:(output :: prevs) ~max_length:(max_length - 1) )
  in
  loop ~state:dec_state ~prevs:[ dec_start ] ~max_length |> List.map ~f:Tensor.to_int0_exn

(* Compute the training loss on a pair of texts. *)
let train_loss ~input_ ~target ~enc ~dec ~dec_start ~dec_eos ~device =
  let enc_final, enc_outputs =
    let rec loop
    = fun state idx outs ->
      if idx = List.length input_
      then (state, outs)
      else
        let input_tensor = Tensor.of_int1 [| idx |] ~device in
        let (hidden, out) = Enc.(enc.forward) input_tensor ~hidden:state in
        loop hidden (idx + 1) (out :: outs)
    in loop Enc.(enc.zero_state) 0 []
  in
  let enc_outputs = Tensor.stack enc_outputs ~dim:1 in
  (* When [use_teacher_forcing] is [true], use the target words as input
     for each step of the decoder rather than the decoder output for the
     previous step. *)
  let use_teacher_forcing = Float.( < ) (Random.float 1.) 0.5 in
  let dec_state = Enc.to_tensor enc_final |> D.of_tensor in
  (* Loop until the target sequence ends or the model returns EOS. *)
  let rec loop
  = fun ~loss ~state ~prev ~target ->
    match target with
    | [] -> loss
    | idx :: target ->
      let state, output =
        D.(dec.forward) prev ~hidden:state ~enc_outputs ~is_training:true
      in
      let target_tensor = Tensor.of_int1 [| idx |] ~device in
      let loss = Tensor.(loss + nll_loss output ~targets:target_tensor) in
      let _, output = Tensor.topk output ~k:1 ~dim:(-1) ~largest:true ~sorted:true in
      if Tensor.get_int2 output 0 0 = dec_eos
      then loss
      else (
        let prev = if use_teacher_forcing then target_tensor else output in
        loop ~loss ~state ~prev ~target)
  in
  loop ~loss:(Tensor.of_float0 ~device 0.) ~state:dec_state ~prev:dec_start ~target

let hidden_size = 256

module Loss_stats = struct
  (* TODO: also track time elapsed ? *)
  type t =
    { (* mutable *) total_loss : tensor
    ; (* mutable *) samples : tensor
    }

  let create () = { total_loss = Tensor.f 0.; samples = Tensor.of_int0 0 }

  let avg_and_reset t =
    let avg = Tensor.float_value t.total_loss /. Float.of_int (Tensor.int_value t.samples) in
    Tensor.fill_float t.total_loss 0.;
    Tensor.fill_int t.samples 0;
    avg

  let update t loss =
    Tensor.(t.total_loss += Tensor.f loss);
    Tensor.(t.samples += Tensor.f 1.)
end

let () =
  let dataset =
    Dataset.create ~input_lang:"eng" ~output_lang:"fra" ~max_length |> Dataset.reverse
  in
  let ilang = Dataset.input_lang dataset in
  let olang = Dataset.output_lang dataset in
  Stdio.printf "Input:  %s %d words.\n%!" (Lang.name ilang) (Lang.length ilang);
  Stdio.printf "Output: %s %d words.\n%!" (Lang.name olang) (Lang.length olang);
  let device = Device.cuda_if_available () in
  let vs = Var_store.create ~name:"seq2seq" ~device () in
  let enc = Enc.create vs ~input_size:(Lang.length ilang) ~hidden_size in
  let dec = D.create vs ~output_size:(Lang.length olang) ~hidden_size in
  let optimizer = Optimizer.adam vs ~learning_rate:0.0001 in
  let pairs = Dataset.pairs dataset in
  let loss_stats = Loss_stats.create () in
  let dec_start = Tensor.of_int1 [| Lang.sos_token olang |] ~device in
  let dec_eos = Lang.eos_token olang in
  for iter = 1 to 75_000 do
    let input_, target = pairs.(Random.int (Array.length pairs)) in
    let loss = train_loss ~input_ ~target ~enc ~dec ~dec_start ~dec_eos ~device in
    Optimizer.backward_step optimizer ~loss;
    let loss = Tensor.to_float0_exn loss /. Float.of_int (List.length target) in
    Loss_stats.update loss_stats loss;
    if iter % 1_000 = 0
    then (
      Stdio.printf "%d %f\n%!" iter (Loss_stats.avg_and_reset loss_stats);
      let to_str l lang = List.map l ~f:(Lang.get_word lang) |> String.concat ~sep:" " in
      for _pred_index = 1 to 5 do
        (* In sample testing. *)
        let input_, target = pairs.(Random.int (Array.length pairs)) in
        let predict = predict ~input_ ~enc ~dec ~dec_start ~dec_eos ~device in
        Stdio.printf "in:  %s\n%!" (to_str input_ ilang);
        Stdio.printf "tgt: %s\n%!" (to_str target olang);
        Stdio.printf "out: %s\n%!" (to_str predict olang)
      done)
  done
