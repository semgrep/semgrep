open Common

(*
   Type definitions, mostly.
*)

(* in JSON mode, we might need to display intermediate '.' in the
 * output for semgrep to track progress as well as extra targets
 * found by extract rules.
 *)
type output_format = Text | Json of bool (* dots *) [@@deriving show]

(*
   'Rule_file' is for the semgrep-core CLI.
   The 'Rules' case is for an invocation that bypasses the semgrep-core
   command (osemgrep) or when for some reason the rules had to be preparsed.
*)
type rule_source = Rule_file of Fpath.t | Rules of Rule.t list

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

(* All rules and targets applicable to a specific language.
   This is passed directly by the new osemgrep implementation, not
   from the semgrep-core command line. *)
type lang_job = { lang : Xlang.t; targets : Fpath.t list; rules : Rule.t list }

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
  pattern_string : string;
  pattern_file : filename; (* TODO: use Fpath.t option *)
  rule_source : rule_source option;
  lang_job : lang_job option;
  equivalences_file : string; (* TODO: use Fpath.t option *)
  lang : Xlang.t option;
  roots : Fpath.t list;
  output_format : output_format;
  match_format : Matching_report.match_format;
  mvars : Metavariable.mvar list;
  ls : bool;
  (* Limits *)
  (* maximum time to spend running a rule on a single file *)
  timeout : float;
  (* maximum number of rules that can timeout on a file *)
  timeout_threshold : int;
  max_memory_mb : int;
  max_match_per_file : int;
  ncores : int;
  (* TODO: use Fpath.t option instead of Common.dirname *)
  parsing_cache_dir : Common.dirname; (* "" means no cache *)
  filter_irrelevant_rules : bool;
  (* Flag used by the semgrep-python wrapper *)
  target_source : target_source option;
  (* Common.ml action for the -dump_xxx *)
  action : string;
  (* Other *)
  version : string;
}

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
    pattern_string = "";
    pattern_file = "" (* invalid path! *);
    rule_source = None;
    lang_job = None;
    equivalences_file = "" (* invalid path! *);
    lang = None;
    roots = [];
    output_format = Text;
    match_format = Matching_report.Normal;
    mvars = [];
    ls = false;
    (* Limits *)
    (* maximum time to spend running a rule on a single file *)
    timeout = 0.;
    (* maximum number of rules that can timeout on a file *)
    timeout_threshold = 0;
    max_memory_mb = 0;
    max_match_per_file = 10_000;
    ncores = 1;
    parsing_cache_dir = "" (* invalid path! *);
    filter_irrelevant_rules = true;
    (* -fast by default *)
    (* "" means no cache *)
    (* Flag used by the semgrep-python wrapper *)
    target_source = None;
    (* Common.ml action for the -dump_xxx *)
    action = "";
    (* Other *)
    version = "";
  }
