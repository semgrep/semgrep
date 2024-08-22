(* Just a small wrapper around the 'semver' OPAM library
 * to offer also the deriving functions.
 *)

type t = Semver.t [@@deriving show, ord]
