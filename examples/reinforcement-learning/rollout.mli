type actor_critic (num_procs) =
  { actor : tensor([num_procs; nth 1 v.shape])
  ; critic : tensor([num_procs; 1])
  }

type totals =
  { rewards : float
  ; episodes : float
  }

type rollout (num_steps, num_stack, num_procs) =
  { states : tensor([num_steps + 1; num_procs; num_stack; 84; 84])
  ; returns : tensor([num_steps + 1; num_procs])
  ; actions : tensor([num_steps; num_procs])
  ; values : tensor([num_steps; num_procs])
  }

type t (num_steps, num_stack, num_procs)

(** [create] creates a rollout environment with the specified parameters. *)
val create
  :  ~atari_game:string -> ~num_steps:int -> ~num_stack:int -> ~num_procs:int
  -> { v:t | v.num_steps = num_steps && v.num_stack = num_stack && v.num_procs = num_procs }

val set
  :  tensor:tensor -> int
  -> { v:tensor | tail tensor.shape = broadcast (tail tensor.shape) v.shape }
  -> unit

val action_space : t -> int

(** [run t ~model] performs a rollout for [t.num_steps] using the given actor-critic
    model.
    The resulting rollout combines the observed states, returns to the end of the
    episode, performed action, and critic values.
*)
val run
  :  t:t
  -> ~model:(tensor([t.num_procs; t.num_stack; 84; 84]) -> { v:actor_critic | v.num_procs = t.num_procs })
  -> { v:rollout | v.num_steps = t.num_steps && v.num_stack = t.num_stack && v.num_procs = t.num_procs }

(** [get_and_reset_totals t] returns the sum of rewards and the number of finished
    episodes performed during the previous calls to [run t] since the last call
    to [get_and_reset_totals].
*)
val get_and_reset_totals : t -> totals
