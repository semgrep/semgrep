(* File position.
 *
 * See also Loc.ml for file location (file region/range).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  bytepos : int; (* 0-based *)
  line : int; (* 1-based *)
  column : int; (* 0-based *)
  file : string;
}
[@@deriving show, eq, ord, sexp]

val make : ?line:int -> ?column:int -> ?file:string -> int -> t

(* basic file position (used to be Common2.filepos) (used in codemap) *)
type linecol = { l : int; c : int } [@@deriving show, eq]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val fake_pos : t
val first_pos_of_file : string (* filename *) -> t

(* for error reporting *)
val string_of_pos : t -> string

(*****************************************************************************)
(* Adjust line x col in a position *)
(*****************************************************************************)

(*
   Return (line, column) from a byte position.

   If the byte position is out of range, the functions of this type return
   the nearest valid position which is either the first or the last position
   in the range.
   Empty files admit at least one valid byte position.
*)
type bytepos_to_linecol_fun = int -> int * int

(* Can we deprecate those full_charpos_xxx? use
 * Parsing_helpers.tokenize_all_and_adjust_pos()?
 * Parse_ruby is still using those functions though :(
 *)

(* f(i) will contain the (line x col) of the i char position *)
val full_charpos_to_pos_large : string (* filename *) -> bytepos_to_linecol_fun
val full_charpos_to_pos_str : string -> bytepos_to_linecol_fun

(* fill in the line and column field of a position that were not set
 * during lexing because of limitations of ocamllex and Lexing.position.
 *)
val complete_position :
  string (* filename *) -> bytepos_to_linecol_fun -> t -> t
