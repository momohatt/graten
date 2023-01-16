module Foo = struct
  type t = { a : int }

  let create () = { a = 1 }

  let x = 1
  let x = 2
end

let x = Foo.create()

(* This should be 2 *)
let y = Foo.x
