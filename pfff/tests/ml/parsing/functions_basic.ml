
let foo x =
  raise Todo

let foo = fun x ->
  raise Todo

let foo = function x ->
  raise Todo


let (foo: int -> int) = fun x ->
  raise Todo
