(* alt: we could move also this file under src/rule/ now *)

type t = Rule_options_t.t

let default = Rule_options_j.t_of_string "{}"

let pp fmt t =
  let s = Rule_options_j.string_of_t t in
  Format.fprintf fmt "%s" s
