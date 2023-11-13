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

type t [@@deriving hash, show, ord, eq, sexp]

(* creators *)
val of_int : int -> t
val of_int64 : int64 -> t
val parse : string * Tok.t -> t

(* like int_of_string_opt, but also converts C octals like 0400 in
 * the right value. *)
val parse_c_octal : string * Tok.t -> t
val of_string_opt : string -> t option
val of_float : float -> t
val zero : t
val neg : t -> t
val map_tok : (Tok.t -> Tok.t) -> t -> t
val bind : (int64 * Tok.t -> 'a option) -> t -> 'a option
val out : t -> int64 option * Tok.t

(* destructors *)
val to_int64_opt : t -> int64 option
val to_int_opt : t -> int option
val to_float_opt : t -> float option
val to_string_opt : t -> string option
val get_tok : t -> Tok.t
val has_val : t -> bool
val visit : v_tok:(Tok.t -> Tok.t) -> t -> t

(* misc *)
val eq_const : t -> int -> bool
