type t (a) =
  { a : { v:int | v = a }
  ; b : { v:int | v = a + 1 }
  }

let x = { a = 1; b = 2 }

let y = x.a

let z =
  let { a; b } = x in
  a + b

let f x =
  let { a = a; _ } = x in
  a + 1

let g x =
  let { a = a; _ } = x in
  { a = a; b = a + 1 }

let w = f x

type t2 (a) =
  { c : tensor([a; nth 1 v.shape]) }

let x2 = { c = Torch.Tensor.zeros [2; 3] }

let z2 =
  let { c } = x2 in
  c
