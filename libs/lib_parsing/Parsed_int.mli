(* Why parsed integers?
 * This is a library for "parsed ints", which are a type that correspond to
 * literal integers that have been parsed from source text.

 * Integers have many obvious uses, such as counters, indices, etc. These can
 * be conflated with the OCaml representation of integers in a target program,
 * which are often not meant to be manipulated, but are merely symbols.

 * As such, it is somewhat useful to have a separate notion of integers which are
 * concrete in the target text, and integers which we make up for our internal
 * purposes.
 *)

type t = int64 option * Tok.t [@@deriving hash, show, eq, ord, sexp]

(* These are the main functions you need to create a Parsed_int.t.
   The other ones are less important.
*)
val parse : string * Tok.t -> t
val parse_c_octal : string * Tok.t -> t

(* creators *)
val of_int : int -> t
val of_int64 : int64 -> t

(* like int_of_string_opt, but also converts C octals like 0400 in
 * the right value. *)
val of_string_opt : string -> t option
val of_float : float -> t
val fake_zero : t
val neg : t -> t
val map_tok : (Tok.t -> Tok.t) -> t -> t

(* destructors *)
val to_int_opt : t -> int option
val to_float_opt : t -> float option
val to_string_opt : t -> string option
val visit : v_tok:(Tok.t -> Tok.t) -> t -> t

(* misc *)
val eq_const : t -> int -> bool
val eq_value : t -> t -> bool
