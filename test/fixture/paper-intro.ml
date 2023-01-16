open Torch

let device = Device.cuda_if_available ()
let vs = Var_store.create ~device ~name:"cnn" ()

let model1 s =
  let layer1 = Test.layer1 s in
  let layer2 (x : tensor([10])) = x in
  fun x ->
    let x = layer1 x in
    let x = layer2 x in
    x

let model2 s =
  let layer1 = Test.layer1 s in
  let layer2 (x : tensor([10])) = x in
  fun x ->
    let x = if s = 1 then x else layer1 x in
    let x = layer2 x in
    x

let model3 s =
  let layer1 = Test.layer1 s in
  let layer2 (x : tensor([10])) = x in
  fun x ->
    let x = (if s = 1 then x else layer1 x : tensor([(nth 0 x.shape - 1) // s + 1])) in
    let x = layer2 x in
    x
