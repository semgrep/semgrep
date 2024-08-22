(* Note that since OCaml 4.12.0, the standard library has an Either module
 * but it is not recognized by default by ppx_deriving so it's simpler
 * for now to define our own alias with the right deriving.
 *)

(* Haskell-inspired either type *)
type ('a, 'b) t = ('a, 'b) Either.t = Left of 'a | Right of 'b
[@@deriving eq, show, sexp]

val partition : ('a -> ('b, 'c) t) -> 'a list -> 'b list * 'c list

type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
[@@deriving eq, show]

val partition_either3 :
  ('a -> ('b, 'c, 'd) either3) -> 'a list -> 'b list * 'c list * 'd list
