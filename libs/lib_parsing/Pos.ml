(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2023 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* File position.
 *
 * See also Loc.t for file location (file region/range).
 *
 * similar code:
 *  - Lexing.position (also used for Spacegrep.Loc.Pos), but no convenient
 *    line x col
 *  - Semgrep_output_v1.position (but no filename)
 *  - Tree_sitter_run.Loc.pos (but no filename, no charpos, just line x col),
 *    itself derived from Tree_sitter_bindings.Tree_sitter_output_t.position
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  charpos : int; (* byte position, 0-based *)
  (* line x column can be filled later based on charpos.
   * See Parsing_helpers.complete_token_location() *)
  line : int; (* 1-based *)
  column : int; (* 0-based *)
  (* TODO: use Fpath.t *)
  file : Common.filename;
}
[@@deriving show, eq]

(* basic file position (used to be Common2.filepos) (unused in Semgrep) *)
type linecol = { l : int; c : int }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fake_pos =
  { charpos = -1; line = -1; column = -1; file = "FAKE TOKEN LOCATION" }

let first_pos_of_file file = { charpos = 0; line = 1; column = 0; file }

(* for error reporting *)
let string_of_pos x = spf "%s:%d:%d" x.file x.line x.column
