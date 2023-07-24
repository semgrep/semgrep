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
  (* TODO? rename bytepos? handle UTF-8? *)
  charpos : int; (* byte position, 0-based *)
  (* line x column can be filled later based on charpos.
   * See complete_position() *)
  line : int; (* 1-based *)
  column : int; (* 0-based *)
  (* TODO: use Fpath.t *)
  file : Common.filename;
}
[@@deriving show, eq, ord]

(* basic file position (used to be Common2.filepos) (used in codemap) *)
type linecol = { l : int; c : int } [@@deriving show, eq]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fake_pos =
  { charpos = -1; line = -1; column = -1; file = "FAKE TOKEN LOCATION" }

let first_pos_of_file file = { charpos = 0; line = 1; column = 0; file }

(* for error reporting *)
let string_of_pos x = spf "%s:%d:%d" x.file x.line x.column

(*****************************************************************************)
(* Adjust line x col in a position *)
(*****************************************************************************)

(* conversion table, in the shape of a function *)
type bytepos_to_linecol_fun = int -> int * int

(* Lexing.ml in the standard OCaml libray does not handle
 * the line number position.
 * Even if there are certain fields in the Lexing.position structure, they are
 * not maintained by the lexing engine so the following code does not work:
 *
 *   let pos = Lexing.lexeme_end_p lexbuf in
 *   sprintf "at file %s, line %d, char %d" pos.pos_fname pos.pos_lnum
 *      (pos.pos_cnum - pos.pos_bol) in
 *
 * Hence the function below to overcome the previous limitation,
 * alt:
 *   - in each lexer you need to take care of newlines and update manually
 *     the field.
 *)
let complete_position filename table (x : t) =
  {
    x with
    file = filename;
    line = fst (table x.charpos);
    column = snd (table x.charpos);
  }

let full_charpos_to_pos_large (file : Common.filename) : bytepos_to_linecol_fun
    =
  let chan = open_in_bin file in
  let size = Common2.filesize file + 2 in

  (* old: let arr = Array.create size  (0,0) in *)
  let arr1 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  let arr2 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  Bigarray.Array1.fill arr1 0;
  Bigarray.Array1.fill arr2 0;

  let charpos = ref 0 in
  let line = ref 0 in

  let full_charpos_to_pos_aux () =
    try
      while true do
        let s = input_line chan in
        incr line;
        let len = String.length s in

        (* '... +1 do'  cos input_line does not return the trailing \n *)
        let col = ref 0 in
        for i = 0 to len - 1 + 1 do
          (* old: arr.(!charpos + i) <- (!line, i); *)
          arr1.{!charpos + i} <- !line;
          arr2.{!charpos + i} <- !col;
          (* ugly: hack for weird windows files containing a single
           * carriage return (\r) instead of a carriage return + newline
           * (\r\n) to delimit newlines. Not recognizing those single
           * \r as a newline marker prevents Javascript ASI to correctly
           * insert semicolons.
           * note: we could fix info_from_charpos() too, but it's not
           * used for ASI so simpler to leave it as is.
           *)
          if i < len - 1 && String.get s i =$= '\r' then (
            incr line;
            col := -1);
          incr col
        done;
        charpos := !charpos + len + 1
      done
    with
    | End_of_file ->
        for
          i = !charpos
          to (* old: Array.length arr *)
             Bigarray.Array1.dim arr1 - 1
        do
          (* old: arr.(i) <- (!line, 0); *)
          arr1.{i} <- !line;
          arr2.{i} <- 0
        done;
        ()
  in
  full_charpos_to_pos_aux ();
  close_in chan;
  fun i -> (arr1.{i}, arr2.{i})
  [@@profiling]

(* This is mostly a copy-paste of full_charpos_to_pos_large,
   but using a string for a target instead of a file. *)
let full_charpos_to_pos_str (s : string) : bytepos_to_linecol_fun =
  let size = String.length s + 2 in

  (* old: let arr = Array.create size  (0,0) in *)
  let arr1 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  let arr2 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  Bigarray.Array1.fill arr1 0;
  Bigarray.Array1.fill arr2 0;

  let charpos = ref 0 in
  let line = ref 0 in
  let str_lines = String.split_on_char '\n' s in

  let full_charpos_to_pos_aux () =
    List.iter
      (fun s ->
        incr line;
        let len = String.length s in

        (* '... +1 do'  cos input_line does not return the trailing \n *)
        let col = ref 0 in
        for i = 0 to len - 1 + 1 do
          (* old: arr.(!charpos + i) <- (!line, i); *)
          arr1.{!charpos + i} <- !line;
          arr2.{!charpos + i} <- !col;
          (* ugly: hack for weird windows files containing a single
           * carriage return (\r) instead of a carriage return + newline
           * (\r\n) to delimit newlines. Not recognizing those single
           * \r as a newline marker prevents Javascript ASI to correctly
           * insert semicolons.
           * note: we could fix info_from_charpos() too, but it's not
           * used for ASI so simpler to leave it as is.
           *)
          if i < len - 1 && String.get s i =$= '\r' then (
            incr line;
            col := -1);
          incr col
        done;
        charpos := !charpos + len + 1)
      str_lines
  in
  full_charpos_to_pos_aux ();
  fun i -> (arr1.{i}, arr2.{i})
  [@@profiling]
