type t = Semver.t

(* we don't have access to Semver code so can't add the deriving there
 * so we need to roll our own boilerplate imitating what
 * a simple [@@deriving show, ord] would do above.
 *)
let pp fmt x = Format.pp_print_string fmt (Semver.to_string x)
let compare = Semver.compare
let show = Semver.to_string
