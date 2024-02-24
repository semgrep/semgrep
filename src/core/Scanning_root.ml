(*
   A dedicated type for scanning roots so as to clarify the code.
*)

type t = Fpath.t [@@deriving show]

let of_fpath x = x
let to_fpath x = x
let of_string = Fpath.v
let to_string = Fpath.to_string
