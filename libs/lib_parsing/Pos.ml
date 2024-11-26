(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2023-2024 Semgrep Inc.
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
open Fpath_.Operators

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
 *  - Semgrep_output_v1.position, but no filename
 *  - Tree_sitter_run.Loc.pos (derived itself from Tree_sitter_bindings.Tree_sitter_output_t.position),
 *    but no filename, no bytepos, just line x col.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Pos.t is used in many places in Semgrep, either directly
 * or indirectly via Tok.t.
 *
 * In theory, we should just have 'type t = int'. This would be nice
 * because Pos.t, which is used in Tok.t, is used to store the position of
 * every token in the generic AST (see AST_generic.ml) so keeping its type
 * small would help reduce the memory footprint of an AST.
 * However this would require a big refactoring effort. Indeed,
 * even though the current type is a bit "fat", it is also convenient
 * because you can easily get line x col or filename information.
 * Moving the filename out of Pos.t would require to pass it around
 * in parsers, evaluators, static analyzers, etc.
 * With the current design, once you have a Pos.t (or Tok.t), you
 * can easily issue an error message with a precise location.
 *
 * TODO: we could probably remove the line x column and compute them
 * on demand.
 *)
type t = {
  (* Does it handle UTF-8? This is a byte position, not a character
   * position, so in theory we should not have to care about UTF-8.
   *)
  bytepos : int; (* 0-based *)
  (* Those two fields can be derived from bytepos (See complete_position() *)
  line : int; (* 1-based *)
  column : int; (* 0-based *)
  (* TODO: use an Src.t/Origin.t instead? (see spacegrep Src_file.source *)
  file : Fpath_.t;
}
[@@deriving show, eq, ord, sexp]

(* basic file position (used to be Common2.filepos) (used in codemap) *)
type linecol = { l : int; c : int } [@@deriving show, eq]

(* alt: could use @@deriving make.
 * TODO? should we use 0 instead? -1 clearly mark the field has not been set
 *)
let make ?(line = -1) ?(column = -1) file bytepos =
  { bytepos; line; column; file }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let first_pos_of_file file = make ~line:1 ~column:0 file 0

(* for error reporting *)
let string_of_pos { file; line; column; _ } = spf "%s:%d:%d" !!file line column
let to_linecol { line; column; _ } = { l = line; c = column }

(*****************************************************************************)
(* Adjust line x col in a position *)
(*****************************************************************************)

(* conversion table, in the shape of a function *)
type bytepos_linecol_converters = {
  bytepos_to_linecol_fun : int -> int * int;
  linecol_to_bytepos_fun : int * int -> int;
}

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
let complete_position (file : Fpath.t) converters (x : t) =
  {
    x with
    file;
    line = fst (converters.bytepos_to_linecol_fun x.bytepos);
    column = snd (converters.bytepos_to_linecol_fun x.bytepos);
  }

(*
   line_arr maps byte position to line.
   col_arr maps byte position to column.
*)
let converters_of_arrays line_arr col_arr : bytepos_linecol_converters =
  let len1 = Bigarray.Array1.dim line_arr in
  let len2 = Bigarray.Array1.dim col_arr in
  (* len1 and len2 should be equal but we're playing it safe *)
  let len = min len1 len2 in
  match len with
  | 2 ->
      {
        bytepos_to_linecol_fun = (fun _i -> (1, 0));
        linecol_to_bytepos_fun = (fun _ -> 0);
      }
  | _ ->
      {
        bytepos_to_linecol_fun =
          (fun i ->
            let i = max 0 (min i (len - 1)) in
            (line_arr.{i}, col_arr.{i}));
        linecol_to_bytepos_fun =
          (let cmp = Ord.to_comparison Int.compare in
           (* This is the line/col we're trying to find the pos of.
           *)
           fun (line, col) ->
             let res =
               line_arr
               |> Ord.binary_search_bigarr1 ~f:(fun bytepos line' ->
                      let col' = col_arr.{bytepos} in
                      (* We want the relationship of the varying line' with
                         respect to the line we are trying to search for.
                         For instance, if we want to find line 5, but are given
                         line 3, we should want to say Greater, because we want
                         to go greater.
                      *)
                      match cmp line line' with
                      | Ord.Equal -> cmp col col'
                      | Ord.Less -> Ord.Less
                      | Ord.Greater -> Ord.Greater)
             in
             match res with
             | Error _idx -> raise Not_found
             | Ok (bytepos, _) -> bytepos);
      }

(* coupling: see also Parse_tree_sitter_helpers.line_col_to_pos *)
let full_converters_large (file : Fpath.t) : bytepos_linecol_converters =
  let size = UFile.filesize file + 2 in

  (* old: let arr = Array.create size  (0,0) in *)
  let arr1 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  let arr2 = Bigarray.Array1.create Bigarray.int Bigarray.c_layout size in
  Bigarray.Array1.fill arr1 0;
  Bigarray.Array1.fill arr2 0;

  let charpos = ref 0 in
  let line = ref 0 in

  UFile.with_open_in file (fun chan ->
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
              (* old: hack for weird Windows files containing a single
               * carriage return (CR) (\r) instead of a carriage return +
               * newline feed (LF) (\r\n) to delimit newlines.
               *   if i < len - 1 && String.get s i =$= '\r' then (
               *      incr line;
               *      col := -1);
               * Not recognizing those single \r as a newline marker prevents
               * Javascript ASI to correctly insert semicolons.
               * However, this hack is commented because having one part
               * of the program recognizing those single CR as newlines
               * (e.g., the Javascript parser), and other parts not
               * (e.g., any function using Stdlib.input_line such as
               * UFile.lines_of_file_exn) can cause a mismatch such as
               * array out of bound exceptions in some functions.
               * Simpler to be consistent. Note that tools such
               * as `wc -l` do not recognize either those single CR as newlines.
               * Same for the tree-sitter libraries.
               *)
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
      full_charpos_to_pos_aux ());
  converters_of_arrays arr1 arr2
[@@profiling]

(* This is mostly a copy-paste of full_charpos_to_pos_large,
   but using a string for a target instead of a file. *)
let full_converters_str (s : string) : bytepos_linecol_converters =
  let size = String.length s + 2 in

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
        let col = ref 0 in
        for i = 0 to len - 1 + 1 do
          arr1.{!charpos + i} <- !line;
          arr2.{!charpos + i} <- !col;
          incr col
        done;
        charpos := !charpos + len + 1)
      str_lines
  in
  full_charpos_to_pos_aux ();
  converters_of_arrays arr1 arr2
[@@profiling]

(*****************************************************************************)
(* unit tests *)
(*****************************************************************************)

let s = "a\nhi\n"
let converters = full_converters_str s

let equate_positions bytepos linecol =
  converters.bytepos_to_linecol_fun bytepos =*= linecol
  && converters.linecol_to_bytepos_fun linecol =*= bytepos

let%test _ = equate_positions 0 (1, 0)

(* newline character is counted as being on the same line *)
let%test _ = equate_positions 1 (1, 1)
let%test _ = equate_positions 2 (2, 0)
let%test _ = equate_positions 3 (2, 1)
let%test _ = equate_positions 4 (2, 2)
