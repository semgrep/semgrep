(* Cooper Pierce, Yoann Padioleau
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
(* Gather types and helper functions to deal with extract rules *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* ex: foo.html *)
type original_target = Original of Fpath.t

(* ex: /tmp/extracted-foo-42.js *)
type extracted_target = Extracted of Fpath.t

let debug_extract_mode = ref false

(* A function which maps a match result from the *extracted* target
 * (e.g., '/tmp/extract-foo.rb') to a match result to the
 * *original* target (e.g., 'src/foo.erb').
 *
 * We could use instead:
 *
 *        (a -> a) -> a Report.match_result -> a Report.match_result
 *
 * although this is a bit less ergonomic for the caller.
 *)
type match_result_location_adjuster =
  Core_result.matches_single_file -> Core_result.matches_single_file

(* Intermediate type used by Match_extract_mode.ml which ultimately
 * gets collated with other to build a final [adjusters] below.
 *
 * old: was Input_to_core_t.target when this type was in Match_extract_mode
 * but in core/ we don't want to depend on targeting/ (which contains the
 * Input_to_core.atd symlink)
 *)
type extracted_target_and_adjuster = {
  extracted : extracted_target;
  original : original_target;
  adjuster : match_result_location_adjuster;
  (* useful to build in Input_to_core_t.target *)
  analyzer : Xlang.t;
}

(* TODO: use a single hashtbl from extracted_target to the type above *)
type adjusters = {
  loc_adjuster : (extracted_target, match_result_location_adjuster) Hashtbl.t;
  original_target : (extracted_target, original_target) Hashtbl.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let is_extract_rule (r : Rule.t) : bool =
  match r.mode with
  | `Extract _ -> true
  | `Search _
  | `Taint _
  | `Steps _ ->
      false

let partition_rules (rules : Rule.t list) : Rule.t list * Rule.extract_rule list
    =
  Either_.partition_either
    (fun r ->
      match r.Rule.mode with
      | `Extract _ as e -> Right { r with mode = e }
      | mode -> Left { r with mode })
    rules

(* this does not just filter, this also returns a better type *)
let filter_extract_rules (rules : Rule.t list) : Rule.extract_rule list =
  rules
  |> List_.map_filter (fun (r : Rule.t) ->
         match r.mode with
         | `Extract _ as e -> Some ({ r with mode = e } : Rule.extract_rule)
         | `Search _
         | `Taint _
         | `Steps _ ->
             None)

let adjusters_of_extracted_targets
    (extracted_targets : extracted_target_and_adjuster list) : adjusters =
  (* Build the hashtables for mapping back the ranges *)
  (* TODO these would be better as Maps *)
  let fn_tbl = Hashtbl.create 101 in
  let file_tbl = Hashtbl.create 101 in
  extracted_targets
  |> List.iter (fun { extracted; adjuster; original; analyzer = _ } ->
         Hashtbl.add fn_tbl extracted adjuster;
         Hashtbl.add file_tbl extracted original);
  { loc_adjuster = fn_tbl; original_target = file_tbl }

(* adjust the match location for extracted targets *)
let adjust_location_extracted_targets_if_needed (adjusters : adjusters)
    (file : Fpath.t) (matches : Core_result.matches_single_file) :
    Core_result.matches_single_file =
  match Hashtbl.find_opt adjusters.loc_adjuster (Extracted file) with
  | Some match_result_loc_adjuster -> match_result_loc_adjuster matches
  | None -> matches
