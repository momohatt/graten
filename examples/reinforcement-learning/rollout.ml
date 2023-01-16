open Base
open Torch
module E = Vec_env_gym_pyml

module Frame_stack = struct
  type t (num_procs, num_stack) =
    { data : tensor([num_procs; num_stack; 84; 84])
    ; num_procs : { v:int | v = num_procs }
    ; num_stack : { v:int | v = num_stack }
    }

  let create ~num_procs ~num_stack =
    { data = Tensor.zeros [ num_procs; num_stack; 84; 84 ] ~kind:(T Float)
    ; num_procs
    ; num_stack
    }

  (* EDIT: Make 'masks' a required argument *)
  let update t ~masks img =
    Tensor.(t.data *= view masks ~size:[ t.num_procs; 1; 1; 1 ]);
    let stack_slice i = Tensor.narrow t.data ~dim:1 ~start:i ~length:1 in
    for frame_index = 1 to t.num_stack - 1 do
      Tensor.copy_ (stack_slice (frame_index - 1)) ~src:(stack_slice frame_index)
    done;
    Tensor.copy_ (stack_slice (t.num_stack - 1)) ~src:img;
    t.data
end

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

type t (num_steps, num_stack, num_procs) =
  { envs : Vec_env_gym_pyml.t
  ; num_steps : { v:int | v = num_steps }
  ; num_procs : { v:int | v = num_procs }
  ; frame_stack : { v:Frame_stack.t | v.num_procs = num_procs && v.num_stack = num_stack }
  ; s_states : tensor([num_steps + 1; num_procs; num_stack; 84; 84])
  ; sum_rewards : tensor([num_procs])
  ; total_rewards : tensor([])
  ; total_episodes : tensor([])
  }

let create ~atari_game ~num_steps ~num_stack ~num_procs =
  let frame_stack = Frame_stack.create ~num_procs ~num_stack in
  let envs = E.create atari_game ~num_processes:num_procs in
  let obs = E.reset envs in
  Tensor.print_shape obs ~name:"obs";
  ignore (Frame_stack.update frame_stack obs ~masks:(Tensor.ones [num_procs])); (* EDIT: add 'masks' *)
  let s_states =
    Tensor.zeros [ num_steps + 1; num_procs; num_stack; 84; 84 ] ~kind:(T Float)
  in
  { envs
  ; num_steps
  ; num_procs
  ; frame_stack
  ; s_states
  ; sum_rewards = Tensor.zeros [ num_procs ]
  (* EDIT: replace mutable float with a 0-dimensional tensor *)
  ; total_rewards  = Tensor.f 0.
  ; total_episodes = Tensor.f 0.
  }

let set tensor i src = Tensor.copy_ (Tensor.get tensor i) ~src
let action_space t = E.action_space t.envs

let run t ~model =
  set t.s_states 0 (Tensor.get t.s_states (-1));
  let s_values = Tensor.zeros [ t.num_steps; t.num_procs ] in
  let s_rewards = Tensor.zeros [ t.num_steps; t.num_procs ] in
  let s_actions = Tensor.zeros [ t.num_steps; t.num_procs ] ~kind:(T Int64) in
  let s_masks = Tensor.zeros [ t.num_steps; t.num_procs ] in
  for s = 0 to t.num_steps - 1 do
    (* TODO: Successfully infer refinement for 'actor' and 'critic' here. (Also pass 'z2' of test/fixture/record.ml) *)
    let { actor; critic } = (* Tensor.no_grad *) (fun () -> model (Tensor.get t.s_states s)) () in
    let probs = Tensor.softmax actor ~dim:(-1) ~dtype:(T Float) in
    let actions =
      Tensor.multinomial probs ~num_samples:1 ~replacement:true |> Tensor.squeeze_last
    in
    let { E.obs = obs
        ; reward = reward
        ; is_done = is_done } =
      E.step t.envs ~actions:(Tensor.to_int1_exn actions |> Array.to_list)
    in
    Tensor.(t.sum_rewards += reward);
    Tensor.(t.total_rewards += sum (t.sum_rewards * is_done));
    Tensor.(t.total_episodes += Tensor.sum is_done);
    let masks = Tensor.(f 1. - is_done) in
    Tensor.(t.sum_rewards *= masks);
    let obs = Frame_stack.update t.frame_stack obs ~masks in
    set s_actions s actions;
    set s_values s (critic |> Tensor.squeeze_dim ~dim:(-1));
    set t.s_states (s + 1) obs;
    set s_rewards s reward;
    set s_masks s masks
  done;
  let s_returns =
    let r = Tensor.zeros [ t.num_steps + 1; t.num_procs ] in
    let critic = (* Tensor.no_grad *) (fun () -> (model (Tensor.get t.s_states (-1))).critic) () in
    set r (-1) (Tensor.view critic ~size:[ t.num_procs ]);
    for s = t.num_steps - 1 downto 0 do
      set r s Tensor.((get r Int.(s + 1) * f 0.99 * get s_masks s) + get s_rewards s)
    done;
    r
  in
  { states = t.s_states; returns = s_returns; actions = s_actions; values = s_values }

let get_and_reset_totals t =
  let res = { rewards = Tensor.float_value t.total_rewards; episodes = Tensor.float_value t.total_episodes } in
  Tensor.fill_float t.total_rewards  0.;
  Tensor.fill_float t.total_episodes 0.;
  res
