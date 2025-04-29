(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2023 Semgrep Inc.
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
open Sexplib.Std

(*
   There are (too) many places in Semgrep where we define a "location"
   and "position".
   A position is usually a place in a file, like a cursor.
   A location is usually a region in this file.

   Here are example of places where we define a loc (and often pos):
    - Tok.location (a Pos.t and a string)
    - Tok_range.t (a pair of Tok.t)
    - semgrep_output_v1.location (a file and start/end positions)
    - Spacegrep.Loc.t (using a range of Lexing.position)
    - Aliengrep.Match.loc (no filename, just start x length)
    - Tree_sitter_run.Loc.t (no filename either, just start/end positions)

   TODO: we should factorize some of those locations
*)

(* alias to the most common location type used in Semgrep *)
type t = {
  (* the content of the "token" *)
  str : string;
  (* TODO? the content of Pos.t used to be inlined in this location type.
   * It is cleaner to factorize things in Pos.t, but this introduces
   * an extra pointer which actually can have real performance implication
   * in Semgrep on huge monorepos. It might be worth inlining it back
   * (and also reduce its number of fields).
   *)
  pos : Pos.t;
}
[@@deriving show { with_path = false }, eq, ord, sexp]

(* Token locations are supposed to denote the beginning of a token.
   Suppose we are interested in instead having line, column, and bytepos of
   the end of a token instead.
   This is something we can do at relatively low cost by going through and
   inspecting the contents of the token, plus the start information.
*)
let end_pos loc =
  let line, col, trailing_nl =
    String.fold_left
      (fun (line, col, after_nl) c ->
        match c with
        | '\n' when after_nl -> (line + 1, 0, true)
        | '\n' -> (line, col, true)
        | _ when after_nl -> (line + 1, 1, false)
        | _ -> (line, col + 1, false))
      (loc.pos.line, loc.pos.column, false)
      loc.str
  in
  let col =
    (* THINK: We count a trailing newline as an extra character in the last line,
     * is that the standard ? *)
    if trailing_nl then col + 1 else col
  in
  (line, col, loc.pos.bytepos + String.length loc.str)

let first_loc_of_file file = { str = ""; pos = Pos.first_pos_of_file file }

(* TODO? move to Pos.ml and use Pos.t instead *)
let adjust_loc_wrt_base base_loc loc =
  (* Note that bytepos and columns are 0-based, whereas lines are 1-based. *)
  let base_pos = base_loc.pos in
  let pos = loc.pos in
  {
    loc with
    pos =
      {
        bytepos = base_pos.bytepos + pos.bytepos;
        line = base_pos.line + pos.line - 1;
        column =
          (if pos.line =|= 1 then base_pos.column + pos.column else pos.column);
        file = base_pos.file;
      };
  }

let fix_pos fix loc = { loc with pos = fix loc.pos }
