(* Compatibility with OCaml code written before effect became a keyword. *)
let effect x = x
let use = effect 42
let apply effect = effect 1
