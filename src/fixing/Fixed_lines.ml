(* Brandon Wu
 *
 * Copyright (C) 2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* We need this module because ???
 *
 * pad: I think this helped in moving autofix management from pysemgrep
 * to partially semgrep-core.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Mapping of file path to a list of ranges affected by previous autofixes. *)
type env = (Fpath.t, (int * int) list) Hashtbl.t

let mk_env () = Hashtbl.create 13

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let make_fixed_lines_of_string env file_contents (edit : Textedit.t) =
  let previous_edits =
    match Hashtbl.find_opt env edit.path with
    | Some xs -> xs
    | None -> []
  in
  let fix_overlaps =
    (* O(n). But realistically the list will probably be short. We're reading
     * the whole file contents in anyway each time, too. *)
    List.exists
      (fun (st, en) -> st < edit.end_ && en > edit.start)
      previous_edits
  in
  if fix_overlaps then None
  else
    (* First, apply the edit *)
    let updated_contents = Textedit.apply_edit_to_text file_contents edit in
    (* Now, compute the range in the updated string that was affected by the
     * edit. *)
    let start = edit.start in
    let end_ = start + String.length edit.replacement_text in
    (* Then, get the lines in the updated string that were affected by the edit.
     * *)
    let lines = String_.lines_of_range (start, end_) updated_contents in
    (* Record that we did this edit, so that subsequent overlapping edits can be
     * omitted. *)
    Hashtbl.replace env edit.path ((edit.start, edit.end_) :: previous_edits);
    match lines with
    (* If we are deleting whole line(s) only, we omit fixed_lines. This is odd
     * behavior, but it matches pysemgrep and is exercised by e2e tests. *)
    | [ "" ] -> None
    | _ -> Some lines

let make_fixed_lines env (edit : Textedit.t) =
  let file_contents = UFile.read_file edit.path in
  make_fixed_lines_of_string env file_contents edit
