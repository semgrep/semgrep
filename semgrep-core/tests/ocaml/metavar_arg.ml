(* ERROR: *)
let foo = foo 1 2

(* ERROR: *)
let foo1 = foo a_very_long_constant_name 2

(* ERROR: *)
let foo2 = foo unsafe (* indeed *) 2

(* ERROR: *)
let foo3 = foo (bar 1 3) 2

let foo4 = foo 2 1
