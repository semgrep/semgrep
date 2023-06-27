open File.Operators
module Out = Semgrep_output_v1_t
module RP = Report
module Env = Semgrep_envvars

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Translated from core_runner.py and core_output.py

   LATER: we should remove this file and call directly Run_semgrep
   and not go through the intermediate semgrep-core JSON output.
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
  (* osemgrep-only: *)
  ast_caching : bool;
}
[@@deriving show]

(* output *)
(* LATER: ideally we should just return what Run_semgrep returns,
   without the need for the intermediate Out.core_match_results.
   LATER: get rid also of Output_from_core_util.ml
*)
type result = {
  (* ocaml: not in original python implem, but just enough to get
   * Semgrep_scan.cli_output_of_core_results to work
   *)
  core : Out.core_match_results;
  hrules : Rule.hrules;
  scanned : Fpath.t Set_.t;
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
let merge_results (xresults : (Report.final_result * Fpath.t Set_.t) list) :
    Report.final_result * Fpath.t Set_.t =
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
      rules_by_engine =
        List.concat_map (fun x -> x.Report.rules_by_engine) results;
    }
  in
  (final_result, files)

(* The same rule may appear under multiple target languages because
   some patterns can be interpreted in multiple languages.
   TODO? could use Common.group_by
*)
let group_rules_by_target_language rules : (Xlang.t * Rule.t list) list =
  (* target language -> rules *)
  let tbl = Hashtbl.create 100 in
  rules
  |> List.iter (fun (rule : Rule.t) ->
         let pattern_lang = rule.languages.target_analyzer in
         let target_langs = Xlang.flatten pattern_lang in
         target_langs
         |> List.iter (fun lang ->
                let rules =
                  match Hashtbl.find_opt tbl lang with
                  | None -> []
                  | Some rules -> rules
                in
                Hashtbl.replace tbl lang (rule :: rules)));
  Hashtbl.fold (fun lang rules acc -> (lang, rules) :: acc) tbl []

let split_jobs_by_language all_rules all_targets : Lang_job.t list =
  all_rules |> group_rules_by_target_language
  |> Common.map_filter (fun (xlang, rules) ->
         let targets =
           all_targets
           |> List.filter (Filter_target.filter_target_for_xlang xlang)
         in
         if Common.null targets then None
         else Some ({ xlang; targets; rules } : Lang_job.t))

let runner_config_of_conf (conf : conf) : Runner_config.t =
  match conf with
  | {
   num_jobs;
   timeout;
   timeout_threshold;
   max_memory_mb;
   optimizations;
   ast_caching;
  }
  (* TODO: time_flag = _;
  *) ->
      (* We default to Json because we do not want the current text
       * displayed in semgrep-core, and we don't want either the
       * current semgrep-core incremental matches text output.
       *)
      let output_format = Runner_config.Json false (* no dots *) in
      let filter_irrelevant_rules = optimizations in
      let parsing_cache_dir =
        if ast_caching then Some (Env.v.user_dot_semgrep_dir / "cache" / "asts")
        else None
      in
      {
        Runner_config.default with
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        filter_irrelevant_rules;
        parsing_cache_dir;
        version = Version.version;
      }

(* Similar to semgrep_with_raw_results_and_exn_handler but takes rules
   and targets already filtered for a specific language.
   All other options are read from the runner_config object.
   TODO: we should not need this function because
   semgrep_with_raw_results_and_exn_handler can take a list of targets
   in different language (via config.targets set via --targets)
   This is ugly, with potentially some filtering operations being done twice.
   It should get simplified when we get rid of the pysemgrep completely.
*)
let semgrep_with_prepared_rules_and_targets (config : Runner_config.t)
    (x : Lang_job.t) : Exception.t option * Report.final_result * Fpath.t list =
  (* compute the rule idx and rule_nums for target_mappings
   * (see Input_to_core.atd)
   *)
  let rule_ids =
    x.rules
    |> Common.map (fun (x : Rule.t) ->
           let id, _tok = x.id in
           (id :> string))
  in
  let rule_nums = rule_ids |> Common.mapi (fun i _ -> i) in
  let target_mappings =
    x.targets
    |> Common.map (fun (path : Fpath.t) : Input_to_core_t.target ->
           { path = !!path; language = x.xlang; rule_nums })
  in
  let wrapped_targets : Input_to_core_t.targets =
    { target_mappings; rule_ids }
  in
  let config =
    {
      config with
      target_source = Some (Targets wrapped_targets);
      rule_source = Some (Rules x.rules);
    }
  in
  (* !!!!Finally! this is where we branch to semgrep-core!!! *)
  Run_semgrep.semgrep_with_raw_results_and_exn_handler config

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(*
   Take in rules and targets and return object with findings.
*)
let invoke_semgrep_core ?(respect_git_ignore = true)
    ?(file_match_results_hook = None) ~matching_explanations (conf : conf)
    (all_rules : Rule.t list) (rule_errors : Rule.invalid_rule_error list)
    (all_targets : Fpath.t list) : result =
  let config : Runner_config.t = runner_config_of_conf conf in
  let config = { config with file_match_results_hook; matching_explanations } in

  match rule_errors with
  (* with pysemgrep, semgrep-core is passed all the rules unparsed,
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
          rules_by_engine = [];
          engine_requested = `OSS;
        }
      in
      { core; hrules = Rule.hrules_of_rules all_rules; scanned = Set_.empty }
  | [] ->
      (* TODO: we should not need to use Common.map below, because
         Run_semgrep.semgrep_with_raw_results_and_exn_handler can accept
         a list of targets with different languages! We just
         need to pass the right target object (and not a lang_job)
         TODO: Martin said the issue was that Run_semgrep.targets_of_config
         requires the xlang object to contain a single language.
         TODO: Martin says there's no fundamental reason to split
         a scanning job by programming language. Several optimizations
         are possible based on target project structure, number and diversity
         of rules, presence of rule-specific include/exclude patterns etc.
         Right now we're constrained by the pysemgrep/semgrep-core interface
         that requires a split by "language". While this interface is still
         in use, bypassing it without removing it seems complicated.
         See https://www.notion.so/r2cdev/Osemgrep-scanning-algorithm-5962232bfd74433ba50f97c86bd1a0f3
      *)
      let lang_jobs = split_jobs_by_language all_rules all_targets in
      Logs.app (fun m ->
          m "%a"
            (fun ppf () ->
              Status_report.pp_status ~num_rules:(List.length all_rules)
                ~num_targets:(List.length all_targets) ~respect_git_ignore
                lang_jobs ppf)
            ());
      (* TODO progress bar *)
      let results_by_language =
        lang_jobs
        |> Common.map (fun lang_job ->
               let _exn_optTODO, report, files =
                 semgrep_with_prepared_rules_and_targets config lang_job
               in
               (report, Set_.of_list files))
      in
      let res, scanned = merge_results results_by_language in
      (* TODO: should get this from Run_semgrep *)
      let _exnTODO = None in
      (* similar to Run_semgrep.semgrep_with_rules_and_formatted_output *)
      (* LATER: we want to avoid this intermediate data structure but
       * for now that's what pysemgrep used to get so simpler to return it.
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
  [@@profiling]
