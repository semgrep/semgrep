module Out = Semgrep_output_v1_t
module RP = Report

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Translated from core_runner.py and core_output.py

   LATER: we should remove this file and call directly Run_semgrep
   and not go through the intermediate semgrep-core JSON output.
*)

(* python: Don't translate this:

   class StreamingSemgrepCore:
       """
       Handles running semgrep-core in a streaming fashion

       This behavior is assumed to be that semgrep-core:
       - prints a "." on a newline for every file it finishes scanning
       - prints a number on a newline for any extra targets produced during a scan
       - prints a single json blob of all results

       Exposes the subprocess.CompletedProcess properties for
       expediency in integrating
       """
*)

(*
   python: implement this but skip parsing the semgrep-core output,
   getting it directly as OCaml objects.
   Big class (500 lines of python):

class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(* input *)
type conf = {
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int;
}
[@@deriving show]

(* output *)
(* LATER: ideally we should just return what Run_semgrep returns,
   without the need for the intermediate Out.core_match_results.
   LATER: get rid of Output_from_core_util.ml
*)
type result = {
  (* ocaml: not in original python implem, but just enough to get
   * Semgrep_scan.cli_output_of_core_results to work
   *)
  core : Out.core_match_results;
  hrules : Rule.hrules;
  scanned : Common.filename Set_.t;
      (* in python implem *)
      (* TODO: original intermediate data structures in python *)
      (*
     findings_by_rule : (Rule.t, Rule_match.t list) Map_.t;
     errors : Error.t list;
     all_targets: path Set_.t;
     (*profiling_data: profiling_data; TOPORT: do we need to translate this? *)
     parsing_data : Parsing_data.t;
     explanations : Out.matching_explanation list option;
  *)
}

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(* TODO: should return also exn opt,
 * like the return type of semgrep_with_raw_results_and_exn_handler.
 * TODO: we should not even need this function because
 * Run_semgrep.semgrep_with_raw_results_and_exn_handler can already
 * handle a list of targets in different language and so no need to
 * merge results in the first place.
 *
 * TODO? do we have utility functions like that already in Report.mli?
 * should move it there? or should not need it at all, see TODO above?
 *)
let merge_results
    (xresults : (Report.final_result * Common.filename Set_.t) list) :
    Report.final_result * Common.filename Set_.t =
  let results = xresults |> Common.map fst in
  let files =
    xresults |> Common.map snd |> List.fold_left Set_.union Set_.empty
  in
  let final_result =
    {
      RP.matches = List.concat_map (fun x -> x.RP.matches) results;
      errors = List.concat_map (fun x -> x.RP.errors) results;
      skipped_rules = List.concat_map (fun x -> x.RP.skipped_rules) results;
      extra = No_info;
      explanations = List.concat_map (fun x -> x.RP.explanations) results;
    }
  in
  (final_result, files)

(* The same rule may appear under multiple target languages because
   some patterns can be interpreted in multiple languages. *)
let group_rules_by_target_language rules : (Xlang.t * Rule.t list) list =
  (* target language -> rules *)
  let tbl = Hashtbl.create 100 in
  List.iter
    (fun (rule : Rule.t) ->
      let pattern_lang = rule.languages in
      let target_langs = Xlang.flatten pattern_lang in
      List.iter
        (fun lang ->
          let rules =
            match Hashtbl.find_opt tbl lang with
            | None -> []
            | Some rules -> rules
          in
          Hashtbl.replace tbl lang (rule :: rules))
        target_langs)
    rules;
  Hashtbl.fold (fun lang rules acc -> (lang, rules) :: acc) tbl []

let split_jobs_by_language all_rules all_targets : Runner_config.lang_job list =
  let grouped_rules = group_rules_by_target_language all_rules in
  let cache = Find_target.create_cache () in
  Common.map
    (fun (lang, rules) ->
      let targets =
        List.filter
          (fun target ->
            List.exists
              (fun (rule : Rule.t) ->
                let required_path_patterns, excluded_path_patterns =
                  match rule.paths with
                  | Some { include_; exclude } -> (include_, exclude)
                  | None -> ([], [])
                in
                Find_target.filter_target_for_lang ~cache ~lang
                  ~required_path_patterns ~excluded_path_patterns target)
              rules)
          all_targets
      in
      ({ lang; targets; rules } : Runner_config.lang_job))
    grouped_rules

let runner_config_of_conf (conf : conf) : Runner_config.t =
  match conf with
  | { num_jobs; timeout; timeout_threshold; max_memory_mb; optimizations }
  (* TODO: time_flag = _;
  *) ->
      (* We should default to Json because we do not want the same text
       * displayed in osemgrep than in semgrep-core.
       * We also don't want the current incremental matches output.
       * LATER: probably provide a mechanism to display match as
       * we process files, but probably need different text output
       * of what semgrep-core match_hook currently provides.
       *)
      let output_format = Runner_config.Json false (* no dots *) in
      let filter_irrelevant_rules = optimizations in
      {
        Runner_config.default with
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        filter_irrelevant_rules;
        version = Version.version;
      }

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(*
   Take in rules and targets and return object with findings.
*)
let invoke_semgrep_core (conf : conf) (all_rules : Rule.t list)
    (rule_errors : Rule.invalid_rule_error list)
    (all_targets : Common.filename list) : result =
  let config : Runner_config.t = runner_config_of_conf conf in

  match rule_errors with
  (* with semgrep-python, semgrep-core is passed all the rules unparsed,
   * and as soon as semgrep-core detects an error in a rule, it raises
   * InvalidRule which is caught and translated to JSON before exiting.
   * Here we emulate the same behavior, even though the rules
   * were parsed in the caller already.
   *)
  | err :: _ ->
      (* like in Run_semgrep.sanity_check_rules_and_invalid_rules *)
      let exn = Rule.Err (Rule.InvalidRule err) in
      (* like in Run_semgrep.semgrep_with_raw_results_and_exn_handler *)
      let e = Exception.catch exn in
      let err = Semgrep_error_code.exn_to_error "" e in
      (* like in semgrep_with_rules_and_formatted_output *)
      let error = JSON_report.error_to_error err in
      let core =
        {
          Out.matches = [];
          errors = [ error ];
          skipped_targets = None;
          skipped_rules = None;
          explanations = None;
          time = None;
          stats = { okfiles = 0; errorfiles = 0 };
        }
      in
      { core; hrules = Rule.hrules_of_rules all_rules; scanned = Set_.empty }
  | [] ->
      (* TODO: we should not need to use Common.map below, because
       * Run_semgrep.semgrep_with_raw_results_and_exn_handler can accept
       * a list of targets with different languages! We just
       * need to pass the right target object (and not a lang_job)
       * TODO: Martin said the issue was that Run_semgrep.targets_of_config
       * requires the xlang object to contain a single language.
       *)
      let lang_jobs = split_jobs_by_language all_rules all_targets in
      let results_by_language =
        lang_jobs
        |> Common.map (fun lang_job ->
               let _exn_optTODO, report, files =
                 (* !!!!Finally! this is where we branch to semgrep-core!!! *)
                 Run_semgrep.semgrep_with_prepared_rules_and_targets config
                   lang_job
               in
               (report, Set_.of_list files))
      in
      let res, scanned = merge_results results_by_language in
      (* TODO: should get this from Run_semgrep *)
      let _exnTODO = None in
      (* similar to Run_semgrep.semgrep_with_rules_and_formatted_output *)
      (* LATER: we want to avoid this intermediate data structure but
       * for now that's what semgrep-python used to get so simpler to
       * return it.
       *)
      let match_results =
        JSON_report.match_results_of_matches_and_errors
          (Some Autofix.render_fix) (Set_.cardinal scanned) res
      in

      (* TOPORT? or move in semgrep-core so get info ASAP
         if match_results.skipped_targets:
             for skip in match_results.skipped_targets:
                 if skip.rule_id:
                     rule_info = f"rule {skip.rule_id}"
                 else:
                     rule_info = "all rules"
                 logger.verbose(
                     f"skipped '{skip.path}' [{rule_info}]: {skip.reason}: {skip.details}"
                 )
      *)

      (* TOADAPT:
          match exn with
          | Some e -> Runner_exit.exit_semgrep (Unknown_exception e)
          | None -> ())
      *)
      { core = match_results; hrules = Rule.hrules_of_rules all_rules; scanned }
