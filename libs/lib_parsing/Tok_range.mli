(*
   Representation of the location of a piece of code by the first and
   the last token of the region. The token type is the one used in the
   generic AST.

   This is used so far in intermediate, language-specific ASTs. A location is
   stored with each node of the AST, similarly to how it's done in Camlp4's
   OCaml AST where it used to work well. I (Martin) encourage others to use
   the same approach in newer code. I also think it would be an improvement
   to use this in the generic AST, but that would need a lot of refactoring.

   What it simplifies:
   - The location of a node is directly available (O(1)).
     No need to recompute it each time it's needed.
   - It's a uniform representation that's independent from the syntax.
     Compare to the generic AST which has the 'bracket' and 'wrap' constructs.
     A 'bracket' is not always a pair of brackets. This is the case for example
     for the arguments function calls with a syntax like 'f x y' (OCaml,
     shell, ...). Instead of calling everything a bracket and using the first
     and last token as the "brackets", we call this a loc and it's not
     weird to have every node associated with a loc.
   - Combining locations is simple:
     the sequence (loc1, loc2) = ((first, _), (_, last)) is (first, last).
*)

(* A location or 'loc' for short.

   A location is a pair (first_token, last_token) representing a region of
   code from a given source. It doesn't make sense for the tokens to come from
   different source files. The position of the first token should not come
   after the position of the last token.

   Tuples are convenient, let's not make this type abstract.
*)
type t = Tok.t * Tok.t [@@deriving show]

(*
   'create tok1 tok2' is essentially '(tok1, tok2)' except if one of the
   of tokens is fake. For example, if 'tok1' is fake, the result is
   '(tok2, tok2)'. In general, it's ok to not use this function and use
   the tuple syntax directly.
*)
val create : Tok.t -> Tok.t -> t

(*
   Identify the leftmost and rightmost tokens from a list and return them
   as the list's location.
*)
val of_toks : ('a -> Tok.t) -> 'a list -> t

(*
   Identify the leftmost and rightmost tokens from a list of locations
   and return them as the list's location.
*)
val of_locs : t list -> t

(*
   Same as 'of_locs' but operates on two arguments. This is independent
   of argument order, unlike 'range'.
   'union <2,4> <1,3>' will return '<1,4>', whereas 'range <2,4> <1,3>'
   will return '<2,3>'.
*)
val union : t -> t -> t

(*
   Extract location from each element of the list and construct a location
   spanning all those locations.
*)
val of_list : ('a -> t) -> 'a list -> t

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
   process. See also 'union'.
*)
val range : t -> t -> t

(* Replace the start of the location, unconditionally. *)
val update_start : Tok.t -> t -> t

(* Replace the end of the location, unconditionally. *)
val update_end : t -> Tok.t -> t

(*
   Extend a location to the left or to right using the new token
   if it falls outside of the current location.
   Results are unspecified if the tokens overlap or come from different files.
*)
val extend : t -> Tok.t -> t

(* A pair of fake tokens. Better avoided. *)
val unsafe_fake_loc : t

(* TODO? merge with of_toks? *)
val min_max_toks_by_pos : Tok.t list -> t
