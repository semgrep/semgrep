(*
   Representation of the location of a piece of code by a pair of tokens.

   This is used so far in intermediate, language-specific ASTs.
*)

type tok = Parse_info.t

(* Tuples are convenient, let's not make this type abstract. *)
type t = tok * tok

(*
   'create tok1 tok2' is essentially '(tok1, tok2)' except if one of the
   of tokens is fake. For example, if 'tok1' is fake, the result is
   '(tok2, tok2)'. In general, it's ok to not use this function and use
   the tuple syntax directly.
*)
val create : tok -> tok -> t

(*
   Identify the leftmost and rightmost tokens from a list and return them
   as the list's location.
*)
val of_toks : tok list -> t

(*
   Identify the leftmost and rightmost tokens from a list of locations
   and return them as the list's location.
*)
val of_list : t list -> t

(*
   If one and only one of the tokens is fake, return a location that
   contains no fake token.
*)
val fix : t -> t

(*
   A location is fake if at least one of its tokens is fake.
   If only one token is fake, it can be fixed with 'normalize'.
*)
val is_fake : t -> bool

(*
   Take the first token of first location and the last token of the second
   location to form a new location. Tries to eliminate fake tokens in the
   process.
*)
val range : t -> t -> t

(* Replace the start of the location, unconditionally. *)
val update_start : tok -> t -> t

(* Replace the end of the location, unconditionally. *)
val update_end : t -> tok -> t

(*
   Extend a location to the left or to right using the new token
   if it falls outside of the current location.
   Results are unspecified if the tokens overlap or come from different files.
*)
val extend : t -> tok -> t

(* A pair of fake tokens. Better avoided. *)
val unsafe_fake_loc : t
