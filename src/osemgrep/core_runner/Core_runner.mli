(* input *)
type conf = {
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
  dataflow_traces : bool;
  (* osemgrep-only: *)
  ast_caching : bool;
}
[@@deriving show]

(* output *)
type result = {
  core : Semgrep_output_v1_t.core_output;
  hrules : Rule.hrules;
  scanned : Fpath.t Set_.t;
}

(* similar to Core_scan.core_scan_func *)
type scan_func_for_osemgrep = {
  run :
    ?file_match_results_hook:
      (Fpath.t -> Core_result.matches_single_file -> unit) option ->
    conf ->
    Find_targets.conf ->
    (* LATER? use Config_resolve.rules_and_origin instead? *)
    Rule.rules ->
    Rule.invalid_rule_error list ->
    Fpath.t list ->
    Core_result.result_or_exn;
}

(* Semgrep Pro hook for osemgrep *)
val hook_pro_scan_func_for_osemgrep :
  (Fpath.t list ->
  ?diff_config:Differential_scan_config.t ->
  Engine_type.t ->
  scan_func_for_osemgrep)
  option
  ref

val hook_pro_git_remote_scan_setup :
  (Find_targets.git_remote -> scan_func_for_osemgrep -> scan_func_for_osemgrep)
  option
  ref

val create_core_result : Rule.rule list -> Core_result.result_or_exn -> result

(* Core_scan_func adapter to be used in osemgrep.

   This will eventually call a core scan like pysemgrep but without
   creating a subprocess.

   The first argument is usually Core_scan.scan_with_exn_handler,
   but it can also be Run.deep_with_raw_results_and_exn_handler
   when running in Pro Interfile mode and when called from
   the Steps_runner in Semgrep Pro.

   LATER: This function should go away.
*)
val mk_scan_func_for_osemgrep :
  Core_scan.core_scan_func -> scan_func_for_osemgrep

(* Helper used in Semgrep_scan.ml to setup logging *)
val core_scan_config_of_conf : conf -> Core_scan_config.t

(* reused in semgrep-server *)
val split_jobs_by_language :
  Find_targets.conf -> Rule.t list -> Fpath.t list -> Lang_job.t list
