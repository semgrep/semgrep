open File.Operators
module Out = Semgrep_output_v1_t
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
  core : Out.core_output;
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

(* Type for the core runner function, which can either be invoked by
   invoke_semgrep_core or invoke_semgrep_core_proprietary *)

type semgrep_core_runner =
  ?respect_git_ignore:bool ->
  ?file_match_results_hook:
    (Fpath.t ->
    Core_profiling.partial_profiling Core_result.match_result ->
    unit)
    option ->
  conf ->
  (* LATER? use Config_resolve.rules_and_origin instead? *)
  Rule.rules ->
  Rule.invalid_rule_error list ->
  Fpath.t list ->
  Exception.t option * Core_result.t * Fpath.t Set_.t

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

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

let core_scan_config_of_conf (conf : conf) : Core_scan_config.t =
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
      let output_format = Core_scan_config.Json false (* no dots *) in
      let filter_irrelevant_rules = optimizations in
      let parsing_cache_dir =
        if ast_caching then Some (!Env.v.user_dot_semgrep_dir / "cache" / "asts")
        else None
      in
      {
        Core_scan_config.default with
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        filter_irrelevant_rules;
        parsing_cache_dir;
        version = Version.version;
      }

let prepare_config_for_semgrep_core (config : Core_scan_config.t)
    (lang_jobs : Lang_job.t list) =
  let target_mappings_of_lang_job (x : Lang_job.t) prev_rule_count :
      int * Input_to_core_t.target list * Rule.rules =
    let rule_ids =
      x.rules
      |> Common.map (fun (x : Rule.t) ->
             let id, _tok = x.id in
             (id :> string))
    in
    let rule_nums = rule_ids |> Common.mapi (fun i _ -> i + prev_rule_count) in
    let target_mappings =
      x.targets
      |> Common.map (fun (path : Fpath.t) : Input_to_core_t.target ->
             { path = !!path; language = x.xlang; rule_nums })
    in
    (List.length rule_ids, target_mappings, x.rules)
  in
  (* The targets are mapped to rule_nums rather than rule_ids to improve the memory usage.
     A list of rule_ids is passed with the targets to map back from num -> id. This means
     that when creating the targets structure, the rules need to be numbered against the
     final rule_ids list.

     The rules need to be reversed to number them correctly because of how :: behaves

     TODO after we delete pysemgrep, we can simplify this interface, which will also
     improve memory usage again *)
  let _, target_mappings, rules =
    lang_jobs
    |> List.fold_left
         (fun (n, acc_mappings, acc_rules) lang_job ->
           let num_rules, mappings, rules =
             target_mappings_of_lang_job lang_job n
           in
           (n + num_rules, mappings :: acc_mappings, List.rev rules :: acc_rules))
         (0, [], [])
  in
  let target_mappings = List.concat target_mappings in
  let rules = rules |> List.rev |> List.concat in
  let rule_ids =
    Common.map (fun r -> fst r.Rule.id |> Rule_ID.to_string) rules
  in
  let targets : Input_to_core_t.targets = { target_mappings; rule_ids } in
  {
    config with
    target_source = Some (Targets targets);
    rule_source = Some (Rules rules);
  }

(* Create the core result structure from the results *)
(* LATER: we want to avoid this intermediate data structure but
 * for now that's what pysemgrep used to get so simpler to return it.
 *)
let create_core_result all_rules (_exns, (res : Core_result.t), scanned) =
  let match_results =
    Core_json_output.core_output_of_matches_and_errors (Some Autofix.render_fix)
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

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(*
   Take in rules and targets and return object with findings.
*)
let invoke_semgrep_core
    ?(engine = Core_scan.semgrep_with_raw_results_and_exn_handler)
    ?(respect_git_ignore = true) ?(file_match_results_hook = None) (conf : conf)
    (all_rules : Rule.t list) (invalid_rules : Rule.invalid_rule_error list)
    (all_targets : Fpath.t list) =
  let rule_errors = Core_scan.errors_of_invalid_rule_errors invalid_rules in
  let config : Core_scan_config.t = core_scan_config_of_conf conf in
  let config = { config with file_match_results_hook } in
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
            ~num_targets:(List.length all_targets) ~respect_git_ignore lang_jobs
            ppf)
        ());
  List.iter
    (fun { Lang_job.xlang; _ } ->
      Metrics_.add_feature "language" (Xlang.to_string xlang))
    lang_jobs;
  let config = prepare_config_for_semgrep_core config lang_jobs in

  (* !!!!Finally! this is where we branch to semgrep-core!!! *)
  let exn, res = engine config in

  (* Reinject rule errors *)
  let res =
    {
      res with
      errors = rule_errors @ res.errors;
      skipped_rules = invalid_rules @ res.skipped_rules;
    }
  in

  let scanned = Set_.of_list res.scanned in

  (* TODO(dinosaure): currently, we don't collect metrics when we invoke
     semgrep-core but we should. However, if we implement a way to collect
     metrics, we will just need to set [final_result.extra] to
     [Core_result.Debug]/[Core_result.Time] and this line of code will not change. *)
  Metrics_.add_max_memory_bytes (Core_profiling.debug_info_to_option res.extra);
  Metrics_.add_targets_stats scanned
    (Core_profiling.debug_info_to_option res.extra);

  (exn, res, scanned)
  [@@profiling]
