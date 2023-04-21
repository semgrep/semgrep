(* TODO: remove at some point *)
type t = Tok.t [@@deriving eq, show]

(*****************************************************************************)
(* Fake tokens: safe vs unsafe *)
(*****************************************************************************)

val is_fake : t -> bool
val is_origintok : t -> bool

(* NOTE: These functions introduce unsafe fake tokens, prefer safe functions
 * below, use these only as a last resort! *)
val unsafe_fake_info : string -> t
val unsafe_fake_bracket : 'a -> t * 'a * t

(* "safe" fake versions *)

val fake_info_loc : Tok.location -> string -> t
val fake_info : t -> string -> t
val fake_bracket_loc : Tok.location -> 'a -> t * 'a * t
val fake_bracket : t -> 'a -> t * 'a * t
val sc_loc : Tok.location -> t

(* accessor *)
val unbracket : t * 'a * t -> 'a

(*****************************************************************************)
(* Info accessors *)
(*****************************************************************************)

(* Format the location file/line/column into a string *)
val string_of_info : t -> string

(* comparison (TODO? should use deriving ord?) *)
val compare_pos : t -> t -> int
val min_max_ii_by_pos : t list -> t * t
