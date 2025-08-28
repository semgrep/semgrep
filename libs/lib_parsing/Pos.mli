(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* File position.
 *
 * See also Loc.ml for file location (file region/range).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  file : Fpath.t;
  bytepos : int; (* 0-based *)
  line : int; (* 1-based *)
  column : int; (* 0-based *)
}
[@@deriving show, eq, ord, sexp]
(** The derived [compare] function allows sorting by location *)

val make : ?line:int -> ?column:int -> Fpath.t -> int -> t

val of_lexing_position : Lexing.position -> t
(** Convert from the standard type Lexing.position used by ocamllex.
    The line count should be set during the lexing phase using
    [Lexing.new_line lexbuf]. Extracting the pair of positions for
    a token matched by ocamllex is done with
    [(Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)].
*)

(* basic file position (used to be Common2.filepos) (used in codemap) *)
type linecol = { l : int; c : int } [@@deriving show, eq]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val first_pos_of_file : Fpath.t -> t

(* for error reporting *)
val string_of_pos : t -> string
val to_linecol : t -> linecol

(*****************************************************************************)
(* Adjust line x col in a position *)
(*****************************************************************************)

(*
   Return (line, column) from a byte position.
   Also return byte position from a (line, column).

   If the byte position is out of range, the functions of this type return
   the nearest valid position which is either the first or the last position
   in the range.
   Empty files admit at least one valid byte position.

   If the (line, column) is out of range, a Not_found exception will be raised.
*)
type bytepos_linecol_converters = {
  bytepos_to_linecol_fun : int -> int * int;
  linecol_to_bytepos_fun : int * int -> int;
}

(* Can we deprecate those full_charpos_xxx? use
 * Parsing_helpers.tokenize_all_and_adjust_pos()?
 * Parse_ruby is still using those functions though :(
 *)

(* f(i) will contain the (line x col) of the i char position *)
val full_converters_large : Fpath.t -> bytepos_linecol_converters
val full_converters_str : string -> bytepos_linecol_converters

(* fill in the line and column field of a position that were not set
 * during lexing because of limitations of ocamllex and Lexing.position.
 *)
val complete_position : Fpath.t -> bytepos_linecol_converters -> t -> t
