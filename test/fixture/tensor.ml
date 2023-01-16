open Torch

(** Tensor creation *)

let t1 = Tensor.zeros [2; 3; 4]

let t2 = Tensor.zeros ~requires_grad:true [2; 3; 4]

let t3 = Tensor.zeros ~requires_grad:true [2; 3 + 3; 4]

let t4 = Tensor.(reshape ~shape:[3; 2; 4] (zeros [2; 3; 4]))

let t5 = Tensor.(reshape ~shape:[3; -1; 4] (zeros [2; 3; 4]))


(** Broadcast *)

let t6 = Tensor.(zeros [2; 3; 4] + zeros [3; 4])

let t7 x = Tensor.(zeros [2; x; 4] + zeros [x; 4])

let t8 x y = Tensor.(zeros [y; x; 4] + zeros [x; 4])

(** Tensor Manipulation *)

let t9 = Tensor.(cat_ ~dim:1 (zeros [3; 4; 5]) (zeros [3; 6; 5]))
