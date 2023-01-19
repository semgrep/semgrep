(* ERROR: *)
let foo = foo 1 2

(* ERROR: *)
let foo1 = foo 1
               2

(* ERROR: *)
let foo2 = foo 1 (* comment *)
              2

let foo3 = foo 2 1
