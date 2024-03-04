module OutJ = Semgrep_output_v1_t

(* input *)
type core_runner_conf = {
  (* opti and limits *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int; (* output flags *)
  nosem : bool;
  strict : bool;
  time_flag : bool;
  matching_explanations : bool;
  (* TODO: actually seems like semgrep-core always return them,
   * even if it was not requested by the CLI
   *)
  dataflow_traces : bool;
}
[@@deriving show]

(* output *)
(* LATER: ideally we should just return Core_result.t
   without the need for the intermediate Out.core_output.
*)
type core_runner_result = {
  (* ocaml: not in original python implem, but just enough to get
   * Semgrep_scan.cli_output_of_core_results to work
   *)
  core : OutJ.core_output;
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

(* Create the core result structure from the results *)
(* LATER: we want to avoid this intermediate data structure but
 * for now that's what pysemgrep used to get so simpler to return it.
 *)
let create_core_result (all_rules : Rule.rule list)
    (result_or_exn : Core_result.result_or_exn) : core_runner_result =
  (* similar to Core_command.output_core_results code *)
  let res =
    match result_or_exn with
    | Ok r -> r
    | Error (exn, _core_error_opt) ->
        (* TODO: use _core_error_opt instead? reraise the exn instead?
         * TOADAPT? Runner_exit.exit_semgrep (Unknown_exception e) instead.
         *)
        let err = Core_error.exn_to_error None "" exn in
        Core_result.mk_final_result_with_just_errors [ err ]
  in
  let scanned = Set_.of_list res.scanned in
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

type output_conf = {
  nosem : bool;
  autofix : bool;
  dryrun : bool;
  strict : bool;
  (* maybe should define an Output_option.t, or add a record to
   * Output_format.Text *)
  force_color : bool;
  logging_level : Logs.level option;
  (* Display options *)
  (* mix of --json, --emacs, --vim, etc. *)
  output_format : Output_format.t;
  max_chars_per_line : int;
  max_lines_per_finding : int;
}
[@@deriving show]

let default_output_conf : output_conf =
  {
    nosem = true;
    autofix = false;
    dryrun = false;
    strict = false;
    logging_level = Some Logs.Warning;
    output_format = Text;
    force_color = false;
    max_chars_per_line = 160;
    max_lines_per_finding = 10;
  }

(* This function takes a core runner output and makes it suitable for the user,
 * by filtering out nosem, setting messages, adding fingerprinting etc.
 *)
let preprocess_core_runner_result (conf : output_conf)
    (res : core_runner_result) : OutJ.cli_output =
  let cli_output : OutJ.cli_output =
    Cli_json_output.cli_output_of_core_results ~dryrun:conf.dryrun
      ~logging_level:conf.logging_level res.core res.hrules res.scanned
  in
  cli_output |> fun results ->
  {
    results with
    results = Cli_json_output.index_match_based_ids results.results;
  }
