open Common
module Env = Semgrep_envvars
module Out = Semgrep_output_v1_t

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Translated from core_runner.py and core_output.py

   LATER: we should remove this file and call directly Core_scan
   and not go through the intermediate semgrep-core JSON output.
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(* input
 * LATER: ideally we can cleanup Core_scan_config.ml enough that we don't
 * need this extra type can can just reuse Core_scan_config.t as is.
 * TODO: At least factorize some of it like the opti/limits
 *)
type conf = {
  (* opti and limits *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int; (* output flags *)
  (* features *)
  (* TODO: move nosem in Scan_CLI.conf and handled it Scan_subcommand.ml.
   * Core_scan does not use nosem anymore, or more precisely it always
   * have nosem=true and return findings with ignore=false and assume
   * the caller will handle the ignored findings.
   *)
  nosem : bool;
  strict : bool;
  (* useful for debugging rules *)
  time_flag : bool;
  matching_explanations : bool;
  (* TODO: actually seems like semgrep-core always return them,
   * even if it was not requested by the CLI
   *)
  dataflow_traces : bool;
  (* set by the scan config from the app *)
  symbol_analysis : bool;
}
[@@deriving show]

type pro_conf = {
  (* TODO: change to root: Fpath.t, like in Deep_scan_config.interfile_config *)
  roots : Scanning_root.t list;
  engine_type : Engine_type.t;
}

(* output *)
type result = Core_runner_result.t

(* Type for the scan function, which can either be built by
   mk_core_run_for_osemgrep() or set in Scan_subcommand.hook_pro_scan_func

   This is a record in an attempt to produce clearer error messages than
   with a type alias.

   It doesn't scan the filesystem since it takes a list of target files,
   not scanning roots.
*)
type func = {
  run :
    ?file_match_hook:(Fpath.t -> Core_result.matches_single_file -> unit) ->
    conf ->
    (* TODO alt: pass a bool alongside each target path that indicates whether
       the target is explicit i.e. occurs directly on the command line *)
    Find_targets.conf ->
    (* LATER? alt: use Config_resolve.rules_and_origin instead? *)
    Rule_error.rules_and_invalid ->
    (* Takes a list of target files, not scanning roots. *)
    Fpath.t list ->
    Core_result.result_or_exn;
}

let default_conf : conf =
  {
    (* Maxing out number of cores used to 16 if more not requested to
     * not overload on large machines.
     * Also, hardcode num_jobs to 1 for non-unix (i.e. Windows) because
     * we don't believe that Parmap works in those environments
     * TODO: figure out a solution for Windows multi-processing (OCaml 5 in
     * the worst case)
     *)
    num_jobs = min 16 (if Sys.unix then Parmap_.get_cpu_count () else 1);
    timeout = 5.0;
    (* ^ seconds, keep up-to-date with User_settings.ml and constants.py *)
    timeout_threshold = 3;
    max_memory_mb = 0;
    optimizations = true;
    dataflow_traces = false;
    matching_explanations = false;
    time_flag = false;
    nosem = true;
    strict = false;
    symbol_analysis = false;
  }

(*****************************************************************************)
(* To run a Pro scan (Deep scan and multistep scan) *)
(*****************************************************************************)

(* Semgrep Pro hook. Note that this is useful only for osemgrep. Indeed,
 * for pysemgrep the code path is instead to fork the
 * semgrep-core-proprietary program, which executes Pro_CLI_main.ml
 * which then calls Run.ml code which is mostly a copy-paste of Core_scan.ml
 * with the Pro scan specifities hard-coded (no need for hooks).
 * We could do the same for osemgrep, but that would require to copy-paste
 * lots of code, so simpler to use a hook instead.
 *
 * Note that Scan_subcommand.ml itself is linked in (o)semgrep-pro,
 * and executed by osemgrep-pro. When linked from osemgrep-pro, this
 * hook below will be set.
 *)
let hook_mk_pro_core_run_for_osemgrep : (pro_conf -> func) option Hook.t =
  Hook.create None

(* This hooks into the proprietary part of Semgrep, in order to access a
 * function that helps us quickly checkout and scan a remote git repo.
 * If a repo is checked out sparsely, this will only checkout the files
 * that are needed for the scan.
 *)
let hook_pro_git_remote_scan_setup : (func -> func) option Hook.t =
  Hook.create None

(*************************************************************************)
(* Metrics and reporting *)
(*************************************************************************)
let report_status_and_add_metrics_languages ~respect_gitignore
    (lang_jobs : Lang_job.t list) (rules : Rule.t list) (targets : Fpath.t list)
    =
  Logs.app (fun m ->
      m "%s"
        (* TODO: validate if target is actually within a git repo and
           perhaps set respect_git_ignore to false otherwise *)
        (Text_reports.scan_status ~num_rules:(List.length rules)
           ~num_targets:(List.length targets) ~respect_gitignore lang_jobs));
  lang_jobs
  |> List.iter (fun { Lang_job.analyzer; _ } ->
         Metrics_.add_feature "language" (Analyzer.to_string analyzer));
  ()

(*************************************************************************)
(* Input/output adapters to Core_scan input/output *)
(*************************************************************************)

(* input adapter to Core_scan.scan *)
let core_scan_config_of_conf (conf : conf) : Core_scan_config.t =
  match conf with
  | {
   num_jobs;
   timeout;
   timeout_threshold;
   max_memory_mb;
   optimizations;
   matching_explanations;
   nosem = _TODO;
   strict;
   time_flag;
   (* TODO *)
   dataflow_traces = _;
   symbol_analysis;
  } ->
      (* We do our own output in osemgrep, no need for Core_scan.scan() output *)
      let output_format = Core_scan_config.NoOutput in
      let filter_irrelevant_rules = optimizations in
      {
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        filter_irrelevant_rules;
        matching_explanations;
        strict;
        report_time = time_flag;
        (* set later in mk_core_run_for_osemgrep *)
        target_source = Targets [];
        rule_source = Rules [];
        file_match_hook = None;
        (* same than in Core_scan_config.default
         * alt: we could use a 'Core_scan_config.default with ...' but better
         * to list all the fields.
         *)
        equivalences_file = None;
        respect_rule_paths = true;
        max_match_per_file = Core_scan_config.default.max_match_per_file;
        tracing = None;
        symbol_analysis;
        use_eio = false;
      }

(* output adapter to Core_scan.scan.
 * LATER: we want to avoid this intermediate data structure but
 * for now that's what pysemgrep used to get so simpler to return it.
 *)
let mk_result (all_rules : Rule.rule list) (res : Core_result.t) : result =
  (* similar to Core_command.output_core_results code *)
  let scanned = res.scanned |> List_.map Target.internal_path |> Set_.of_list in
  let match_results = Core_json_output.core_output_of_matches_and_errors res in
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
  { core = match_results; hrules = Rule.hrules_of_rules all_rules; scanned }

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* Core_scan.core_scan_func adapter for osemgrep *)
let mk_core_run_for_osemgrep (core_scan_func : Core_scan.func) : func =
  let run ?file_match_hook (conf : conf) (targeting_conf : Find_targets.conf)
      (rules_and_invalid : Rule_error.rules_and_invalid)
      (target_paths : Fpath.t list) : Core_result.result_or_exn =
    (*
       At this point, we already have the full list of targets. These targets
       will populate the 'target_source' field of the config object
       (after splitting into "languages").
       This mode doesn't tolerate scanning roots. This is checked in
       Core_scan.ml.
    *)
    let valid_rules, invalid_rules = rules_and_invalid in
    (* Deduplicate rules for a correct rule count and avoid redundant
       work or findings.
       TODO: do this even earlier? *)
    let valid_rules =
      valid_rules
      |> List_.deduplicate_gen ~get_key:(fun r ->
             Rule_ID.to_string (fst r.Rule.id))
    in
    let rule_errors : Core_error.t list =
      invalid_rules |> List_.map Core_error.error_of_invalid_rule
    in
    let config : Core_scan_config.t = core_scan_config_of_conf conf in
    let config = { config with file_match_hook } in
    (* LATER: Martin says there's no fundamental reason to split
       a scanning job by programming language. Several optimizations
       are possible based on target project structure, number and diversity
       of rules, presence of rule-specific include/exclude patterns etc.
       Right now we're constrained by the pysemgrep/semgrep-core interface
       that requires a split by "language". While this interface is still
       in use, bypassing it without removing it seems complicated.
       See https://www.notion.so/r2cdev/Osemgrep-scanning-algorithm-5962232bfd74433ba50f97c86bd1a0f3
    *)
    let lang_jobs =
      Core_targeting.split_jobs_by_language targeting_conf valid_rules
        target_paths
    in
    report_status_and_add_metrics_languages
      ~respect_gitignore:targeting_conf.respect_gitignore lang_jobs valid_rules
      target_paths;
    let targets, applicable_rules =
      Core_targeting.targets_and_rules_of_lang_jobs lang_jobs
    in
    Logs.debug (fun m ->
        m "core runner: %i applicable rules of %i valid rules, %i invalid rules"
          (List.length applicable_rules)
          (List.length valid_rules)
          (List.length invalid_rules));
    let config =
      {
        config with
        target_source = Targets targets;
        rule_source = Rules applicable_rules;
      }
    in

    (* !!!!Finally! this is where we branch to semgrep-core core scan fun!!! *)
    let/ res = core_scan_func config in
    let rules_with_targets =
      lang_jobs |> List.concat_map (fun { Lang_job.rules; _ } -> rules)
    in
    (* Reinject rule errors *)
    let res =
      {
        res with
        errors = rule_errors @ res.errors;
        skipped_rules = invalid_rules @ res.skipped_rules;
        valid_rules;
        rules_with_targets;
      }
    in
    let scanned =
      res.scanned |> List_.map Target.internal_path |> Set_.of_list
    in

    Metrics_.add_max_memory_bytes res.profiling;
    Metrics_.add_targets_stats scanned res.profiling;
    Ok res
  in
  { run }
[@@profiling]
