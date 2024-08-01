(* Nat Mote
 *
 * Copyright (C) 2019-2022 Semgrep Inc.
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
open Fpath_.Operators

(* alt: use a common Log_commons.ml src? *)
let src = Logs.Src.create "commons.textedit"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module represents and applies edits to text
 *
 * Related work:
 *  - 'ed', the old Unix command-line tool
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  path : Fpath.t;
  (* 0-based byte index, inclusive *)
  start : int;
  (* 0-based byte index, exclusive *)
  end_ : int;
  replacement_text : string;
}
[@@deriving show]

type edit_application_result =
  | Success of string
  | Overlap of {
      partial_result : string;
      (* nonempty *)
      conflicting_edits : t list;
    }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let overlaps (e1 : t) (e2 : t) =
  (* overlap <=> not (no overlap)
             <=> not (e1 ends before e2 or e2 ends before e1) *)
  not (e1.end_ <= e2.start || e2.end_ <= e1.start)

let overlaps_with_an_interval (intervals : t list) (e1 : t) =
  List.exists (fun e2 -> overlaps e1 e2) intervals

(*
   Remove overlapping edits by proceeding from left to right in the order
   the edits are given to us.

   This is an O(N^2) algorithm where N is the number of edits. It should be
   small enough in practice since N is the number of edits to make in one
   file. If it turns out this isn't good enough, we'll have to use a more
   elaborate algorithm. The problem is that we have to apply the fixes
   in the order in which they're discovered by semgrep rules rather than
   than in the order in which they appear in the file.
*)
let remove_overlapping_edits edits =
  let already_applied_edits = Hashtbl.create 100 in
  let accepted_edits, redundant_edits, conflicting_edits =
    List.fold_left
      (fun (accepted_edits, redundant_edits, conflicting_edits) edit ->
        let key : t = edit in
        if Hashtbl.mem already_applied_edits key then
          (accepted_edits, edit :: redundant_edits, conflicting_edits)
        else (
          Hashtbl.add already_applied_edits key ();
          if overlaps_with_an_interval accepted_edits edit then
            (accepted_edits, redundant_edits, edit :: conflicting_edits)
          else (edit :: accepted_edits, redundant_edits, conflicting_edits)))
      ([], [], []) edits
  in
  (List.rev accepted_edits, List.rev redundant_edits, List.rev conflicting_edits)

(* To align the autofix logic with the Python CLI, remove the newline
     character when replacing an entire line with an empty string.*)
let remove_newline_for_empty_replacement text edit =
  let text_len = String.length text in
  let line_start = edit.start = 0 || String.get text (edit.start - 1) = '\n' in
  let is_empty_replacement = String.length edit.replacement_text = 0 in
  let skip_char c i =
    if i + 1 <= text_len && String.get text i = c then i + 1 else i
  in
  if line_start && is_empty_replacement then
    { edit with end_ = edit.end_ |> skip_char '\r' |> skip_char '\n' }
  else edit

let partition_edits_by_file edits =
  (* TODO Consider using Common.group_by if we update it to return edits in
   * order and add a comment specifying that changes to it must maintain that
   * behavior. *)
  let edits_by_file = Hashtbl.create 8 in
  List.iter
    (fun edit ->
      let prev =
        match Hashtbl.find_opt edits_by_file edit.path with
        | Some lst -> lst
        | None -> []
      in
      Hashtbl.replace edits_by_file edit.path (edit :: prev))
    edits;
  (* Restore the original order of the edits as they appeared in the input list.
   * This is important so that we consistently choose to apply the first edit in
   * the original input when there are edits for an identical span. *)
  Hashtbl.filter_map_inplace
    (fun _file edits -> Some (List.rev edits))
    edits_by_file;
  edits_by_file

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let apply_edit_to_text text ({ start; end_; replacement_text; _ } as edit) =
  Log.debug (fun m -> m "Apply edit %s" (show edit));
  let before = Str.string_before text start in
  let after = Str.string_after text end_ in
  before ^ replacement_text ^ after

let apply_edits_to_text path text edits =
  (*
     Don't sort the edits. They must be applied in the order in which they
     were detected. If two rules report a problem at the same location,
     the first rule that reports the problem has precedence.
  *)
  Log.info (fun m ->
      m "Applying %i edits to file %s" (List.length edits) !!path);
  let applicable_edits, redundant_edits, conflicting_edits =
    remove_overlapping_edits edits
  in
  (* Switch to bottom to top order so that we don't need to track offsets as
   * we apply multiple patches *)
  let applicable_edits =
    List.sort (fun a b -> Int.compare b.start a.start) applicable_edits
  in
  let fixed_text =
    (* Apply the fixes. These string operations are inefficient but should
     * be fine. The Python CLI version of this code is even more inefficent. *)
    List.fold_left
      (fun file_text edit -> apply_edit_to_text file_text edit)
      text applicable_edits
  in
  Log.info (fun m ->
      let successful_edits = applicable_edits @ redundant_edits in
      m "file %s: %i/%i edits were applied successfully" !!path
        (List.length successful_edits)
        (List.length edits));
  if conflicting_edits = [] then Success fixed_text
  else (
    Log.warn (fun m -> m "file %s has overapping textedits" !!path);
    Overlap { partial_result = fixed_text; conflicting_edits })

let apply_edits ~dryrun edits =
  let edits_by_file = partition_edits_by_file edits in
  let all_conflicting_edits = ref [] in
  Hashtbl.iter
    (fun (path : Fpath.t) file_edits ->
      let file_text = UFile.read_file path in
      let file_edits =
        List_.map (remove_newline_for_empty_replacement file_text) file_edits
      in
      let new_text =
        match apply_edits_to_text path file_text file_edits with
        | Success x -> x
        | Overlap { partial_result; conflicting_edits } ->
            Stack_.push conflicting_edits all_conflicting_edits;
            partial_result
      in
      (* TOPORT: when dryrun, report fixed lines *)
      if not dryrun then UFile.write_file ~file:path new_text)
    edits_by_file;
  let modified_files = Hashtbl.to_seq_keys edits_by_file |> List.of_seq in
  let conflicting_edits = List_.flatten !all_conflicting_edits in
  (modified_files, conflicting_edits)
