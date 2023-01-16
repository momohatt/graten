val forward
  :  forall b1 b2.
     (x:{ v:tensor | 'b1#{v} } -> { v:tensor | 'b2#{v,x} })
  ->  x:{ v:tensor | 'b1#{v} } -> { v:tensor | 'b2#{v,x} }

val forward_
  :  forall b1 b2.
     (x:{ v:tensor | 'b1#{v} } -> ~is_training:bool -> { v:tensor | 'b2#{v,x} })
  ->  x:{ v:tensor | 'b1#{v} } -> ~is_training:bool -> { v:tensor | 'b2#{v,x} }

val of_fn
  :  forall b1 b2.
     (x:{ v:tensor | 'b1#{v} } -> { v:tensor | 'b2#{v,x} })
  ->  x:{ v:tensor | 'b1#{v} } -> { v:tensor | 'b2#{v,x} }

val of_fn_
  :  forall b1 b2.
     (x:{ v:tensor | 'b1#{v} } -> ~is_training:bool -> { v:tensor | 'b2#{v,x} })
  ->  x:{ v:tensor | 'b1#{v} } -> ~is_training:bool -> { v:tensor | 'b2#{v,x} }

val with_training
  :  forall b1 b2.
     (x:{ v:tensor | 'b1#{v} } -> { v:tensor | 'b2#{v,x} })
  ->  x:{ v:tensor | 'b1#{v} } -> ~is_training:bool -> { v:tensor | 'b2#{v,x} }

val id  : x:tensor -> tensor(x.shape)
val id_ : x:tensor -> ~is_training:bool -> tensor(x.shape)

type activation =
  | Relu
  | Softmax
  | Log_softmax
  | Tanh
  | Leaky_relu
  | Sigmoid

val linear
  :  Var_store.t
  -> ?activation:activation
  -> ?use_bias:bool     (* default: true *)
  -> ?w_init:Var_store.Init.t
  -> ~input_dim:int
  -> output_dim:int
  -> x:{ v:tensor | last v.shape = input_dim }
  -> tensor(init x.shape @ [output_dim])

val conv2d_
  :  Var_store.t
  -> ~ksize:int
  -> ~stride:int
  -> ?activation:activation (* default: no activation *)
  -> ?use_bias:bool     (* default: true *)
  -> ?w_init:Var_store.Init.t
  -> ?padding:int = { v:int | v = 0 }
  -> ?groups:int
  -> ~input_dim:int
  -> output_dim:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> tensor([nth 0 x.shape; output_dim; (nth 2 x.shape + 2 * padding - ksize) // stride + 1; (nth 3 x.shape + 2 * padding - ksize) // stride + 1])

val conv_transpose2d_
  :  Var_store.t
  -> ~ksize:int
  -> ~stride:int
  -> ?activation:activation (* default: no activation *)
  -> ?use_bias:bool (* default: true *)
  -> ?w_init:Var_store.Init.t
  -> ?padding:int
  -> ?output_padding:int = { v:int | v = 0 }
  -> ?groups:int
  -> ~input_dim:int
  -> output_dim:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = input_dim }
  -> tensor([nth 0 x.shape; output_dim;
             (nth 2 x.shape - 1) * stride - 2 * padding + ksize + output_padding;
             (nth 3 x.shape - 1) * stride - 2 * padding + ksize + output_padding])

val batch_norm2d
  :  Var_store.t
  -> ?w_init:Var_store.Init.t
  -> ?cudnn_enabled:bool (* default: true *)
  -> ?eps:float          (* default: 1e-5 *)
  -> ?momentum:float     (* default: 0.1 *)
  -> num_features:int
  -> x:{ v:tensor | len v.shape = 4 && nth 1 v.shape = num_features }
  -> ~is_training:bool
  -> tensor(x.shape)

val layer_norm
  :  Var_store.t
  -> ?cudnn_enable:bool
  -> ?eps:float
  -> dim:int
  -> x:{ v:tensor | last v.shape = dim }
  -> tensor(x.shape)

module Lstm : sig
  type t (input_size, hidden_size) =
    { w_ih : tensor([ 4 * hidden_size; input_size ])
    ; w_hh : tensor([ 4 * hidden_size; hidden_size ])
    ; b_ih : tensor([ 4 * hidden_size ])
    ; b_hh : tensor([ 4 * hidden_size ])
    ; device : Device.t
    }

  val create
    :  Var_store.t
    -> ~input_dim:int
    -> ~hidden_size:int
    -> { v:t | v.input_size = input_dim && v.hidden_size = hidden_size }

  val zero_state
    :  l:t
    -> ~batch_size:int
    -> tensor([ batch_size; l.hidden_size ]) * tensor([ batch_size; l.hidden_size ])

  val step
    :  forall batch_size.
       l:t
    -> tensor([ batch_size; l.hidden_size ]) * tensor([ batch_size; l.hidden_size ])
    -> tensor([ batch_size; l.input_size ])
    -> tensor([ batch_size; l.hidden_size ]) * tensor([ batch_size; l.hidden_size ])

  val seq
    :  forall batch_size seq_len.
       l:t
    -> input_:tensor([batch_size; seq_len; l.input_size])
    -> ~is_training:bool
    -> tensor([batch_size; seq_len; l.hidden_size])
     * (tensor([1; batch_size; l.hidden_size]) * tensor([1; batch_size; l.hidden_size]))
end

module Gru : sig
  type t (input_size, hidden_size) =
    { w_ih : tensor([ 3 * hidden_size; input_size ])
    ; w_hh : tensor([ 3 * hidden_size; hidden_size ])
    ; b_ih : tensor([ 3 * hidden_size ])
    ; b_hh : tensor([ 3 * hidden_size ])
    ; device : Device.t
    }

  val create
    :  Var_store.t
    -> ~input_dim:int
    -> ~hidden_size:int
    -> { v:t | v.input_dim = input_dim && v.hidden_size = hidden_size }

  val zero_state
    :  l:t
    -> ~batch_size:int
    -> tensor([ batch_size; l.hidden_size ])

  val step
    :  forall batch_size.
       l:t
    -> tensor([ batch_size; l.hidden_size ])
    -> tensor([ batch_size; l.input_size ])
    -> tensor([ batch_size; l.hidden_size ])

  val seq
    :  forall batch_size seq_len.
       l:t
    -> input_:tensor([batch_size; seq_len; l.input_size])
    -> ~is_training:bool
    -> tensor([batch_size; seq_len; l.hidden_size]) * tensor([1; batch_size; l.hidden_size])
end

val embeddings
  :  ?sparse:bool
  -> ?scale_grad_by_freq:bool
  -> Var_store.t
  -> ~num_embeddings:int
  -> ~embedding_dim:int
  -> x:tensor
  -> tensor(x.shape @ [embedding_dim])
