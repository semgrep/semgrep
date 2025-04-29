(* Heejong Lee
 *
 * Copyright (C) 2024 Semgrep Inc.
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
open Common
open Fpath_.Operators
module SS = Set.Make (String)
module Fpaths = Set.Make (Fpath)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A differential scan is when we run Semgrep two times:
 *  - once on a "baseline" commit (e.g., develop)
 *  - once on a "current" commit (e.g., HEAD)
 * Semgrep then reports only the new findings, that is findings that occur
 * in the current commit but not in the baseline.
 *
 * This helps a lot to migrate gradually to semgrep and to new rules by
 * not having to deal with all the findings that occur in a baseline.
 *
 * history: similar to my 'cmf --only-new-errors' at Facebook :)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type diff_scan_func = Fpath.t list -> Rule.rules -> Core_result.result_or_exn

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* TODO: explain what this achieves.
   It's used by remove_matches_in_baseline below. *)
let extract_sig renamed (m : Core_match.t) =
  let rule_id = m.rule_id in
  let path =
    m.path.internal_path_to_content |> fun p ->
    Option.bind renamed
      (List_.find_some_opt (fun (before, after) ->
           if Fpath.equal after p then Some before else None))
    |> Option.value ~default:p
  in
  let start_range, end_range = m.range_loc in
  let path' = m.path.internal_path_to_content in
  let syntactic_ctx : string list =
    match
      UFile.lines_of_file (start_range.pos.line, end_range.pos.line) path'
    with
    | Ok xs -> xs
    | Error err ->
        Logs.warn (fun m ->
            m
              "error on accessing lines of %s; skipping syntactic_ctx (error \
               was %s)"
              !!path' err);
        []
  in
  (rule_id, path, syntactic_ctx)

(* This function removes duplicated matches from the results of the
   head commit scan if they are also present in the results of the
   baseline commit scan. Matches are considered identical if the
   tuples containing the rule ID, file path, and matched code snippet
   are equal. *)
let remove_matches_in_baseline caps (commit : string) (baseline : Core_result.t)
    (head : Core_result.t) (renamed : (Fpath.t * Fpath.t) list) =
  let sigs = Hashtbl.create 10 in
  Git_wrapper.run_with_worktree_exn caps ~commit (fun () ->
      List.iter
        (fun ({ pm; _ } : Core_result.processed_match) ->
          pm |> extract_sig None |> fun x -> Hashtbl.add sigs x true)
        baseline.processed_matches);
  let removed = ref 0 in
  let processed_matches =
    List_.filter_map
      (fun (pm : Core_result.processed_match) ->
        let s = extract_sig (Some renamed) pm.pm in
        if Hashtbl.mem sigs s then (
          Hashtbl.remove sigs s;
          incr removed;
          None)
        else Some pm)
      (head.processed_matches
       (* Sort the matches in ascending order according to their byte positions.
          This ensures that duplicated matches are not removed arbitrarily;
          rather, priority is given to removing matches positioned closer to the
          beginning of the file. *)
      |> List.sort
           (fun ({ pm = x; _ } : Core_result.processed_match) { pm = y; _ } ->
             let x_start_range, x_end_range = x.range_loc in
             let y_start_range, y_end_range = y.range_loc in
             let start_compare =
               x_start_range.pos.bytepos - y_start_range.pos.bytepos
             in
             if start_compare <> 0 then start_compare
             else x_end_range.pos.bytepos - y_end_range.pos.bytepos))
  in
  Logs.app (fun m ->
      m "Removed %s that were in baseline scan"
        (String_.unit_str !removed "finding"));
  { head with processed_matches }

(* Execute the engine again on the baseline checkout, utilizing only
   the files and rules linked with matches from the head checkout
   scan. Subsequently, eliminate any previously identified matches
   from the results of the head checkout scan. *)
let scan_baseline_and_remove_duplicates (caps : < Cap.chdir ; Cap.tmp ; .. >)
    (profiler : Profiler.t) (result_or_exn : Core_result.result_or_exn)
    (rules : Rule.rules) (commit : string) (status : Git_wrapper.status)
    (diff_scan_func : diff_scan_func) : Core_result.result_or_exn =
  let/ r = result_or_exn in
  if r.processed_matches <> [] then
    let add_renamed paths =
      List.fold_left (fun x (y, _) -> Fpaths.add y x) paths status.renamed
    in
    let remove_added paths =
      List.fold_left (Fun.flip Fpaths.remove) paths status.added
    in
    let rules_in_match =
      r.processed_matches
      |> List_.map (fun ({ pm; _ } : Core_result.processed_match) ->
             pm.rule_id.id |> Rule_ID.to_string)
      |> SS.of_list
    in
    (* only use the rules that have been identified within the existing
       matches. *)
    let baseline_rules =
      rules
      |> List.filter (fun x ->
             SS.mem (x.Rule.id |> fst |> Rule_ID.to_string) rules_in_match)
    in
    let baseline_result =
      Profiler.record profiler ~name:"baseline_core_time" (fun () ->
          (* TODO explain this code, break it down into functions and test
             them.
             Or delete this code. *)
          Git_wrapper.run_with_worktree_exn caps ~commit (fun () ->
              let prepare_targets paths =
                (* TODO: what's happening here? *)
                paths |> Fpaths.of_list |> add_renamed |> remove_added
                |> Fpaths.to_seq
                |> Seq.filter_map (fun path ->
                       if
                         Sys_.file_exists !!path
                         &&
                         match (Unix.lstat !!path).st_kind with
                         | S_LNK -> false
                         | _ -> true
                       then Some path
                       else None)
                |> List.of_seq
              in
              let paths_in_match =
                r.processed_matches
                |> List_.map (fun ({ pm; _ } : Core_result.processed_match) ->
                       pm.path.internal_path_to_content)
                |> prepare_targets
              in
              let baseline_targets = paths_in_match in
              (* running on baseline (e.g., develop) *)
              diff_scan_func baseline_targets baseline_rules))
    in
    match baseline_result with
    | Error _exn -> (* TODO: don't ignore exceptions *) baseline_result
    | Ok baseline_r ->
        Ok (remove_matches_in_baseline caps commit baseline_r r status.renamed)
  else Ok r

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let scan_baseline (caps : < Cap.chdir ; Cap.tmp ; .. >) (profiler : Profiler.t)
    (baseline_commit : string) (rules : Rule.rules)
    (diff_scan_func : diff_scan_func) : Core_result.result_or_exn =
  Logs.info (fun m ->
      m "running differential scan on base commit %s" baseline_commit);
  Metrics_.g.payload.environment.isDiffScan <- true;
  let commit = Git_wrapper.merge_base_exn baseline_commit in
  let status = Git_wrapper.status_exn ~cwd:(Fpath.v ".") ~commit () in
  let added_or_modified = status.added @ status.modified in
  let (head_scan_result : Core_result.result_or_exn) =
    Profiler.record profiler ~name:"head_core_time" (fun () ->
        (* running on HEAD *)
        diff_scan_func added_or_modified rules)
  in
  scan_baseline_and_remove_duplicates caps profiler head_scan_result rules
    commit status diff_scan_func
