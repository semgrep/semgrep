(** Locations, or "real tokens". *)

type t = {
  str : string; (* the content of the token starting at pos (e.g., "if") *)
  pos : Pos.t;
}
[@@deriving show, eq, ord, sexp]

val end_pos : t -> int * int * int (* line x col x charpos *)
(** Token positions in loc.pos denote the beginning of a token.
   Suppose we are interested in having instead the line, column, and charpos
   of the end of a token.
   This is something we can do at relatively low cost by going through and
   inspecting the content of the location, plus the start information.
   alt: return a Pos.t instead
*)

val first_loc_of_file : Fpath.t -> t
(** the location will be empty, but its pos will be the beginning of the file *)

val adjust_loc_wrt_base : t -> t -> t
(** See [Tok.adjust_tok_wrt_base]. *)

val fix_pos : (Pos.t -> Pos.t) -> t -> t
(** adjust the position in a location *)
