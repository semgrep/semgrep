(* This module is essentially an adapter around Core_scan.scan()
 * (and Deep_scan.scan()) so that it can be used from osemgrep. We try to
 * imitate what is done in pysemgrep which calls semgrep-core
 * (or semgrep-core-proprietary) and its underlying Core_scan (or Deep_scan).
 *)

(* input *)
type conf = {
  (* opti and limits *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int; (* output flags *)
  (* features *)
  nosem : bool;
  strict : bool;
  (* useful for debugging rules *)
  time_flag : bool;
  matching_explanations : bool;
  dataflow_traces : bool;
}
[@@deriving show]

(* output *)
type result = Core_runner_result.t

(* This type is similar to Core_scan.func, but taking a list of
 * rules and targets and a simpler conf type instead of the very
 * large Core_scan_config.t
 *
 * The list of targets below are final targets (not scanning roots).
 * no further scanning of the filesystem shall be performed.
 * The Find_targets.conf argument is for explicit target management.
 *)
type func = {
  run :
    ?file_match_hook:(Fpath.t -> Core_result.matches_single_file -> unit) ->
    conf ->
    Find_targets.conf ->
    Rule_error.rules_and_invalid ->
    Fpath.t list ->
    Core_result.result_or_exn;
}

type pro_conf = {
  diff_config : Differential_scan_config.t;
  roots : Scanning_root.t list;
  engine_type : Engine_type.t;
}

val default_conf : conf

(* Semgrep Pro hook for osemgrep *)
val hook_mk_pro_core_run_for_osemgrep : (pro_conf -> func) option ref
val hook_pro_git_remote_scan_setup : (func -> func) option ref

(* builder *)
val mk_result : Rule.rule list -> Core_result.t -> result

(* Core_scan.func adapter to be used in osemgrep.

   This will eventually call a core scan like pysemgrep but without
   creating a subprocess.

   The first argument is usually Core_scan.scan, but it can also be
   Deep_scan.scan when running in Pro Interfile mode and when called from
   Steps_scan.scan in Semgrep Pro.

   LATER: This function should go away.
*)
val mk_core_run_for_osemgrep : Core_scan.func -> func

(* Helper used also in Steps_scan.ml *)
val core_scan_config_of_conf : conf -> Core_scan_config.t

(* reused in semgrep-server in pro and for Git_remote.ml in pro *)
val split_jobs_by_language :
  Find_targets.conf -> Rule.t list -> Fpath.t list -> Lang_job.t list

(* Helper used in Test_subcommand.ml *)
val targets_for_files_and_rules : Fpath.t list -> Rule.t list -> Target.t list
