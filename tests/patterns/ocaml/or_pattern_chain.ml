(* ERROR: match *)
let choose x = match x with A | B | C -> 1
let different x = match x with A | B | D -> 1
let shorter x = match x with A | B -> 1
