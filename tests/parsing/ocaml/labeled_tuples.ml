let make x y = (~x, ~y:2, 3)
let typed x = (~(x : int), 2)
let (~x, ~y:renamed, z) = make 1 2
let read = function (~x, ~y:renamed, z) -> x + renamed + z
let typed_read = function (~(x : int), z) -> x + z
type labeled = x:int * y:string * bool
type mixed = int * name:string * bool
