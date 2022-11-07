module Out = Semgrep_output_v1_t
module RP = Report

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Translated from core_runner.py and core_output.py
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

type path = string

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
  scanned : path Set_.t;
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

let runner_config_of_conf (conf : Scan_CLI.conf) : Runner_config.t =
  match conf with
  | {
   num_jobs;
   timeout;
   timeout_threshold;
   max_memory_mb;
   output_format;
   optimizations;
   (* no need to handle, are used before *)
   show_supported_languages = _;
   version = _;
   version_check = _;
   logging_level = _;
   strict = _;
   (* TOPORT: not handled yet *)
   autofix = _;
   dryrun = _;
   baseline_commit = _;
   exclude_rule_ids = _;
   exclude = _;
   include_ = _;
   config = _;
   lang = _;
   pattern = _;
   target_roots = _;
   max_target_bytes = _;
   metrics = _;
   respect_git_ignore = _;
   rewrite_rule_ids = _;
   scan_unknown_extensions = _;
   time_flag = _;
  } ->
      let output_format =
        match output_format with
        | Json -> Runner_config.Json false (* no dots *)
        (* TOPORT: I think also in Text mode we should default
         * to Json because we do not want the same text displayed in
         * osemgrep than in semgrep-core.
         *)
        | Text -> Runner_config.Text
        (* defaulting to Json, which really mean just no incremental
         * display of match in the match_hook in semgrep-core
         *)
        | _else_ -> Runner_config.Json false
      in
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
let invoke_semgrep_core (conf : Scan_CLI.conf) (all_rules : Rule.t list)
    (all_targets : path list) : result =
  let config : Runner_config.t = runner_config_of_conf conf in

  (* TOADAPT
     (* this used to be in Core_CLI.ml but we get a config object
      * later in osemgrep
      * TODO: Just pass directly a Runner_config.t to invoke_semgrep_core
      * so can build and adjust the config in the caller
      *)
     let config =
       if config.profile then (
         logger#info "Profile mode On";
         logger#info "disabling -j when in profiling mode";
         { config with ncores = 1 })
       else config
     in
  *)
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
             Run_semgrep.semgrep_with_prepared_rules_and_targets config lang_job
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
    JSON_report.match_results_of_matches_and_errors (Some Autofix.render_fix)
      (Set_.cardinal scanned) res
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
