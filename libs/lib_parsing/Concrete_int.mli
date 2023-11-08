(* Why concrete integers?
 * This is a library for "concrete ints", which are a type that correspond to
 * literal integers that have been parsed by the Semgrep engine.

 * In several places, we have integers that we use for obvious purposes,
 * such as counters, indices, etc. These can be conflated with the OCaml
 * representation of integers in the target, which are often not meant to be
 * manipulated, but are merely symbols.

 * As such, it is somewhat useful to have a separate notion of integers which are
 * concrete in the target text, and integers which we make up for our internal
 * purposes.
 *)

(* Why virtual?
 * Concrete integers are typically the ordinary OCaml ints, which have favorable
 * performance characteristics.

 * When not running Semgrep natively, however, (as we do when running JSCaml, by
 * transpiling OCaml code to Javascript), this representation of integers will
 * not line up with the Javascript native number representation.

 * On those systems, we would prefer 64-bit ints.
 * So this module is virtual, because then we can swap these implementations out
 * at compile-time, without needing to functorize everything, or incur a runtime
 * cost.

 * I LOVE ZERO-COST ABSTRACTIONS
 *)
type t [@@deriving hash, show, ord, eq, sexp]

(* creators *)
val of_int : int -> t
val of_int64 : int64 -> t
val of_string_opt : string -> t option
val of_float : float -> t

(* like int_of_string_opt, but also converts C octals like 0400 in
 * the right value. *)
val of_string_c_octal_opt : string -> t option
val zero : t
val neg : t -> t

(* destructors *)
val to_int64 : t -> int64
val to_int : t -> int
val to_v : t -> OCaml.v
val to_string : t -> string
val to_float : t -> float

(* misc *)
val eq_const : t -> int -> bool
