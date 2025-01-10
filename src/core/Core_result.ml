(* Yoann Padioleau
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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
module E = Core_error
module Log = Log_semgrep.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* (Core) Scan result information.
 *
 * In addition to the results (matches + errors), we report extra
 * information such as the skipped targets or optional profiling times.
 *
 * Yet another way to store matches/findings.
 * Just like for Core_error.ml, "core" results are translated at some point in
 * Semgrep_output_v1.core_output, then processed in pysemgrep (or osemgrep)
 * and translated again in Semgrep_output_v1.cli_output.
 * There's also Core_runner.result in osemgrep.
 *
 * From the simplest matches to the most complex we have:
 * Core_match.t
 * -> Core_result.processed_match (in this file)
 * -> Core_result.matches_single_file (this file)
 * -> Core_result.t (this file)
 * -> Core_result.result_or_exn (this file)
 * -> Semgrep_output_v1.core_output
 *  -> Core_runner.result
 *  -> Semgrep_output_v1.cli_output
 *  -> Semgrep_output_v1.findings
 * LATER: it would be good to remove some intermediate types.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO: move in Core_error.ml *)
let fmt_errors fmt errors =
  Format.fprintf fmt "{ ";
  E.ErrorSet.iter
    (fun error -> Format.fprintf fmt "%s, " (Core_error.show error))
    errors;
  Format.fprintf fmt "}"

(* For each file, substitute in the profiling type we have *)
type 'a match_result = {
  matches : Core_match.t list;
  errors : E.ErrorSet.t; [@printer fmt_errors]
  profiling : 'a option;
  explanations : Matching_explanation.t list;
}
[@@deriving show]

(* shortcut *)
type matches_single_file = Core_profiling.partial_profiling match_result
[@@deriving show]

(* What is a processsed match?
   These are just match-specific information that we add "after" a scan has
   occurred. This information is initially set to a nullary value, but will be
   filled in by Pre_post_core_scan.

   When we bridge the gap from `Core_result.t` to `Out.core_output`, we have
   to associate each match to a (potential) edit or ignored status.
   We will choose to embed the fix information in `Core_result.t`, as
   autofixing and nosemgrep are now valid functions of the core engine, and
   thus the produced fixes are related to its produced results.
   These edits start as all None, but will be filled in by
   `Autofix.produce_autofixes`, and the associated Autofix_processor step.

   alt: we could have added this to `Core_match.t`, but felt a bit early.
   alt: we could have produced this information when going from Core_result.t
   to Out.core_output, but this would require us to do autofixing and ignoring
   at the same time as output, which conflates process of producing output and
   side-effectively applying autofixes / filtering. In addition, the `Autofix`
   and `Nosemgrep` modules are not available from that directory.
*)
type processed_match = {
  pm : Core_match.t;
  is_ignored : bool;
  autofix_edit : Textedit.t option;
}
[@@deriving show]

type t = {
  (* old: matches : Core_match.t list *)
  processed_matches : processed_match list;
  errors : Core_error.t list;
  (* A target is scanned when semgrep found some applicable rules.
   * The information is useful to return to pysemgrep/osemgrep to
   * display statistics.
   * old: was simply [Fpath.t list] before, but for inter-file diff scan
   * metrics we also need to know the analyzer used on the target,
   * hence the use of [Target.t list] now
   *)
  scanned : Target.t list;
  skipped_targets : Semgrep_output_v1_t.skipped_target list;
  skipped_rules : Rule_error.invalid_rule list;
  valid_rules : Rule.rule list;
  rules_with_targets : Rule.rule list;
  profiling : Core_profiling.t option;
  explanations : Matching_explanation.t list option;
  rules_by_engine : (Rule_ID.t * Engine_kind.t) list;
  interfile_languages_used : Analyzer.t list;
}
[@@deriving show]

type result_or_exn = (t, Exception.t) result

(*****************************************************************************)
(* Builders *)
(*****************************************************************************)

let mk_processed_match pm = { pm; is_ignored = false; autofix_edit = None }

let empty_match_result : Core_profiling.times match_result =
  {
    matches = [];
    errors = E.ErrorSet.empty;
    profiling = None;
    explanations = [];
  }

let mk_result_with_just_errors (errors : Core_error.t list) : t =
  {
    errors;
    (* default values *)
    processed_matches = [];
    valid_rules = [];
    rules_with_targets = [];
    skipped_targets = [];
    scanned = [];
    skipped_rules = [];
    profiling = None;
    explanations = None;
    rules_by_engine = [];
    interfile_languages_used = [];
  }

(* Create a match result *)
let mk_match_result matches errors profiling =
  {
    matches;
    errors;
    profiling = Core_profiling.profiling_opt profiling;
    explanations = [];
  }

(*****************************************************************************)
(* Augment reported information with profiling info *)
(*****************************************************************************)

let map_profiling (f : 'a -> 'b) (x : 'a match_result) : 'b match_result =
  { x with profiling = Option.map f x.profiling }

let add_run_time (run_time : float)
    (match_result : Core_profiling.partial_profiling match_result) :
    Core_profiling.file_profiling match_result =
  match_result
  |> map_profiling (fun { Core_profiling.p_file; p_rule_times } ->
         { Core_profiling.file = p_file; rule_times = p_rule_times; run_time })

let add_rule (rule : Rule.rule)
    (match_result : Core_profiling.times match_result) :
    Core_profiling.rule_profiling match_result =
  match_result
  |> map_profiling (fun { Core_profiling.parse_time; match_time } ->
         {
           Core_profiling.rule_id = fst rule.Rule.id;
           rule_parse_time = parse_time;
           rule_match_time = match_time;
         })

(*****************************************************************************)
(* Aggregate *)
(*****************************************************************************)

(* Helper to aggregate the shared parts of results (rougly equivalent
 * to a fold).
 *)
let collate_results (init : 'c) (combine : 'b option -> 'c -> 'c)
    (final : 'c -> 'a option) (results : 'b match_result list) : 'a match_result
    =
  let unzip_results l =
    let rec unzip all_matches all_errors all_profiling all_explanations
        (l : _ match_result list) =
      match l with
      | { matches; errors; profiling; explanations } :: l ->
          unzip (matches :: all_matches) (errors :: all_errors)
            (combine profiling all_profiling)
            (explanations :: all_explanations)
            l
      | [] ->
          ( List.rev all_matches,
            List.rev all_errors,
            all_profiling,
            List.rev all_explanations )
    in
    unzip [] [] init [] l
  in
  let matches, errors, profiling, explanations = unzip_results results in
  {
    matches = List_.flatten matches;
    (* We deduplicate errors here to avoid repeat PartialParsing errors
       which can arise when multiple rules generate the same error. This is
       done for consistency with other parsing errors, like ParseError or
       LexicalError, which are only reported once per file, not rule.

       See also the note in semgrep_output_v1.atd.
    *)
    errors = List.fold_left E.ErrorSet.union E.ErrorSet.empty errors;
    profiling = final profiling;
    explanations = List_.flatten explanations;
  }

(* Aggregate a list of pattern results into one result *)
let collate_pattern_results (results : Core_profiling.times match_result list) :
    Core_profiling.times match_result =
  let init : Core_profiling.times = { parse_time = 0.0; match_time = 0.0 } in
  let combine extra all_profiling =
    match extra with
    | None -> all_profiling
    | Some profiling -> Core_profiling.add_times profiling all_profiling
  in
  let final = Core_profiling.profiling_opt in
  collate_results init combine final results

(* Aggregate a list of rule results into one result for the target *)
let collate_rule_results (file : Fpath.t)
    (results : Core_profiling.rule_profiling match_result list) :
    Core_profiling.partial_profiling match_result =
  let init = [] in
  let combine extra all_profiling =
    match extra with
    | None -> all_profiling
    | Some profiling -> profiling :: all_profiling
  in
  let final profiling =
    let (p : Core_profiling.partial_profiling) =
      { p_file = file; p_rule_times = profiling }
    in
    Core_profiling.profiling_opt p
  in
  collate_results init combine final results

(*****************************************************************************)
(* Final result *)
(*****************************************************************************)

(* Aggregate a list of target results into one final result *)
let mk_result (results : Core_profiling.file_profiling match_result list)
    (rules_with_engine : (Rule.t * Engine_kind.t) list)
    (skipped_rules : Rule_error.invalid_rule list) (scanned : Target.t list)
    (interfile_languages_used : Analyzer.t list) ~rules_parse_time : t =
  (* concatenating information from the match_result list *)
  let unprocessed_matches =
    results
    |> List.concat_map (fun (x : _ match_result) -> x.matches)
    (* These fixes and ignores are initially all unset, and will be populated
       after we run our Pre_post_core_scan
    *)
    |> List_.map mk_processed_match
  in
  let explanations =
    results |> List.concat_map (fun (x : _ match_result) -> x.explanations)
  in
  let errors =
    results
    |> List.concat_map (fun (x : _ match_result) ->
           x.errors |> E.ErrorSet.elements)
  in

  let (prof : Core_profiling.t) =
    let file_times =
      results
      |> List_.filter_map
           (fun (result : Core_profiling.file_profiling match_result) ->
             result.profiling)
    in
    {
      rules = List_.map fst rules_with_engine;
      rules_parse_time;
      file_times;
      (* Notably, using the `top_heap_words` does not measure cumulative
         memory usage across concurrent processes, meaning that if the most
         amount of memory is consumed by forked processes, we would need to
         multiply by the numbrer of processes to estimate the true maximum.
      *)
      max_memory_bytes = (Gc.quick_stat ()).top_heap_words * Sys.word_size;
    }
  in
  let profiling = Core_profiling.profiling_opt prof in
  {
    processed_matches = unprocessed_matches;
    errors;
    profiling;
    scanned;
    skipped_targets = [];
    skipped_rules;
    valid_rules = [];
    rules_with_targets = [];
    explanations = (if explanations =*= [] then None else Some explanations);
    rules_by_engine =
      rules_with_engine |> List_.map (fun (r, ek) -> (fst r.Rule.id, ek));
    interfile_languages_used;
  }
