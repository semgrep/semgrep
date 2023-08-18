(* in JSON mode, we might need to display intermediate '.' in the
 * output for pysemgrep to track progress as well as extra targets
 * found by extract rules.
 * LATER: osemgrep: not needed after osemgrep migration done
 *)
type output_format = Text | Json of bool (* dots *) [@@deriving show]

(*
   'Rule_file' is for the semgrep-core CLI.
   'Rules' is for osemgrep or when for some reason the rules had to be
    preparsed.
*)
type rule_source =
  | Rule_file of Fpath.t
  | Embedded_rules of Fpath.t
  | Rules of Rule.t list
[@@deriving show]

(*
   'Target_file' is for the semgrep-core CLI which gets a list of
   paths as an explicit list rather than by discovering files by scanning
   folders recursively.
   'Targets' is used by osemgrep, which also takes care of identifying
   targets but doesn't have to put them in a file since we stay in the
   same process and we bypass the semgrep-core CLI.
*)
type target_source =
  | Target_file of Fpath.t
  | Targets of Input_to_core_t.targets
[@@deriving show]

(* TODO: similar to osemgrep Scan_CLI.conf; should be merged with it *)
type t = {
  (* Debugging/profiling/logging flags *)
  log_config_file : Fpath.t;
  log_to_file : Fpath.t option;
  test : bool;
  debug : bool;
  profile : bool;
  report_time : bool;
  error_recovery : bool;
  profile_start : float;
  matching_explanations : bool;
  (* Main flags *)
  pattern_string : string option;
  pattern_file : Fpath.t option;
  rule_source : rule_source option;
  equivalences_file : Fpath.t option;
  lang : Xlang.t option;
  roots : Fpath.t list;
  output_format : output_format;
  match_format : Matching_report.match_format;
  mvars : Metavariable.mvar list;
  (* Limits *)
  (* maximum time to spend running a rule on a single file *)
  timeout : float;
  (* maximum number of rules that can timeout on a file *)
  timeout_threshold : int;
  max_memory_mb : int;
  max_match_per_file : int;
  ncores : int;
  parsing_cache_dir : Fpath.t option;
  filter_irrelevant_rules : bool;
  (* Hook to display match results incrementally, after a file has been fully
   * processed. Note that this hook run in a child process of Parmap
   * in Run_semgrep, so the hook should not rely on shared memory!
   *)
  file_match_results_hook :
    (Fpath.t -> Report.partial_profiling Report.match_result -> unit) option;
  (* Flag used by pysemgrep *)
  target_source : target_source option;
  (* Common.ml action for the -dump_xxx *)
  action : string;
  (* Other *)
  version : string;
}
[@@deriving show]

(* The type of the semgrep core runner. We define it here so that
   semgrep and semgrep-proprietary use the same definition *)
type semgrep_engine =
  t ->
  (* Exceptions raised *)
  Exception.t option
  * (* Result *)
    Report.final_result
  * (* The processed targets *)
  Fpath.t list

(*
   Default values for all the semgrep-core command-line arguments and options.

   Its values can be inherited using the 'with' syntax:

    let my_config = {
      Runner_config.default with
      debug = true;
      ncores = 3;
    }
*)
let default =
  {
    (* Debugging/profiling/logging flags *)
    log_config_file = Fpath.v "log_config.json";
    log_to_file = None;
    test = false;
    debug = false;
    profile = false;
    report_time = false;
    error_recovery = false;
    profile_start = 0.;
    matching_explanations = false;
    (* Main flags *)
    pattern_string = None;
    pattern_file = None;
    rule_source = None;
    equivalences_file = None;
    lang = None;
    roots = [];
    output_format = Text;
    match_format = Matching_report.Normal;
    mvars = [];
    (* Limits *)
    (* maximum time to spend running a rule on a single file *)
    timeout = 0.;
    (* maximum number of rules that can timeout on a file *)
    timeout_threshold = 0;
    max_memory_mb = 0;
    max_match_per_file = 10_000;
    ncores = 1;
    parsing_cache_dir = None;
    (* a.k.a -fast, on by default *)
    filter_irrelevant_rules = true;
    file_match_results_hook = None;
    (* Flag used by the semgrep-python wrapper *)
    target_source = None;
    (* Common.ml action for the -dump_xxx *)
    action = "";
    (* Other *)
    version = "";
  }
