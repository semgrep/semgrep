(* Nat Mote
 *
 * Copyright (C) 2019-2022 r2c
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

(* This module represents and applies edits to text *)

type t = {
  path : Common.filename;
  (* 0-based byte index, inclusive *)
  start : int;
  (* 0-based byte index, exclusive *)
  end_ : int;
  replacement_text : string;
}

type edit_application_result =
  | Success of string
  | Overlap of {
      partial_result : string;
      (* nonempty *)
      conflicting_edits : t list;
    }

let remove_overlapping_edits edits =
  let rec f edits conflicting_edits = function
    | e1 :: e2 :: tl ->
        if e1.end_ > e2.start then
          let conflicting_edits =
            (* If the edits are identical, they are not conflicting. We can
             * apply just one. *)
            if e1 = e2 then conflicting_edits else e2 :: conflicting_edits
          in
          f edits conflicting_edits (e1 :: tl)
        else
          let edits = e1 :: edits in
          f edits conflicting_edits (e2 :: tl)
    | [ edit ] ->
        let edits = edit :: edits in
        (List.rev edits, List.rev conflicting_edits)
    | [] -> (List.rev edits, List.rev conflicting_edits)
  in
  f [] [] edits

let apply_edit_to_text text { start; end_; replacement_text; _ } =
  let before = Str.string_before text start in
  let after = Str.string_after text end_ in
  before ^ replacement_text ^ after

let apply_edits_to_text text edits =
  let edits = List.sort (fun e1 e2 -> e1.start - e2.start) edits in
  let edits, conflicting_edits = remove_overlapping_edits edits in
  (* Switch to bottom to top order so that we don't need to track offsets as
   * we apply multiple patches *)
  let edits = List.rev edits in
  let fixed_text =
    (* Apply the fixes. These string operations are inefficient but should
     * be fine. The Python CLI version of this code is even more inefficent. *)
    List.fold_left
      (fun file_text edit -> apply_edit_to_text file_text edit)
      text edits
  in
  if conflicting_edits = [] then Success fixed_text
  else Overlap { partial_result = fixed_text; conflicting_edits }

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

let apply_edits ~dryrun edits =
  let edits_by_file = partition_edits_by_file edits in
  let all_conflicting_edits = ref [] in
  Hashtbl.iter
    (fun file file_edits ->
      let file_text = Common.read_file file in
      let file_edits =
        Common.map (remove_newline_for_empty_replacement file_text) file_edits
      in
      let new_text =
        match apply_edits_to_text file_text file_edits with
        | Success x -> x
        | Overlap { partial_result; conflicting_edits } ->
            Common.push conflicting_edits all_conflicting_edits;
            partial_result
      in
      (* TOPORT: when dryrun, report fixed lines *)
      if not dryrun then Common.write_file ~file new_text)
    edits_by_file;
  let modified_files = Hashtbl.to_seq_keys edits_by_file |> List.of_seq in
  let conflicting_edits = List.concat !all_conflicting_edits in
  (modified_files, conflicting_edits)
