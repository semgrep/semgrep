(* File position.
 *
 * See also Loc.ml for file location (file region/range).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  charpos : int; (* byte position, 0-based *)
  (* line x column can be filled later based on charpos.
   * See complete_position() *)
  line : int; (* 1-based *)
  column : int; (* 0-based *)
  file : Common.filename;
}
[@@deriving show, eq, compare]

(* basic file position (used to be Common2.filepos) (used in codemap) *)
type linecol = { l : int; c : int } [@@deriving show, eq]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val fake_pos : t
val first_pos_of_file : Common.filename -> t

(* for error reporting *)
val string_of_pos : t -> string

(*****************************************************************************)
(* Adjust line x col in a position *)
(*****************************************************************************)

type bytepos_to_linecol_fun = int -> int * int

(* Can we deprecate those full_charpos_xxx? use
 * Parsing_helpers.tokenize_all_and_adjust_pos()?
 * Parse_ruby is still using those functions though :(
 *)

(* f(i) will contain the (line x col) of the i char position *)
val full_charpos_to_pos_large : Common.filename -> bytepos_to_linecol_fun
val full_charpos_to_pos_str : string -> bytepos_to_linecol_fun

(* fill in the line and column field of a position that were not set
 * during lexing because of limitations of ocamllex and Lexing.position.
 *)
val complete_position : Common.filename -> bytepos_to_linecol_fun -> t -> t
