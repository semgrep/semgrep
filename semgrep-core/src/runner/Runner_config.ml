open Common

(*
   Type definitions, mostly.
*)

(* in JSON mode, we might need to display intermediate '.' in the
 * output for semgrep to track progress as well as extra targets
 * found by extract rules.
 *)
type output_format = Text | Json of bool (* dots *) [@@deriving show]
type rule_source = Rule_file of filename | Rules of Rule.t list

type target_source =
  | Target_file of filename
  | Targets of Input_to_core_t.targets

(* All rules and targets applicable to a specific language.
   This is passed directly by the new osemgrep implementation, not
   from the semgrep-core command line. *)
type lang_job = { lang : Xlang.t; targets : filename list; rules : Rule.t list }

type t = {
  (* Debugging/profiling/logging flags *)
  log_config_file : filename;
  log_to_file : filename option;
  test : bool;
  debug : bool;
  profile : bool;
  report_time : bool;
  error_recovery : bool;
  profile_start : float;
  matching_explanations : bool;
  (* Main flags *)
  pattern_string : string;
  pattern_file : filename;
  rule_source : rule_source option;
  lang_job : lang_job option;
  equivalences_file : string;
  lang : Xlang.t option;
  roots : Common.path list;
  output_format : output_format;
  match_format : Matching_report.match_format;
  mvars : Metavariable.mvar list;
  lsp : bool;
  (* Limits *)
  (* maximum time to spend running a rule on a single file *)
  timeout : float;
  (* maximum number of rules that can timeout on a file *)
  timeout_threshold : int;
  max_memory_mb : int;
  max_match_per_file : int;
  ncores : int;
  parsing_cache_dir : Common.dirname; (* "" means no cache *)
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
    log_config_file = "log_config.json";
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
    pattern_file = "";
    rule_source = None;
    lang_job = None;
    equivalences_file = "";
    lang = None;
    roots = [];
    output_format = Text;
    match_format = Matching_report.Normal;
    mvars = [];
    lsp = false;
    (* Limits *)
    (* maximum time to spend running a rule on a single file *)
    timeout = 0.;
    (* maximum number of rules that can timeout on a file *)
    timeout_threshold = 0;
    max_memory_mb = 0;
    max_match_per_file = 10_000;
    ncores = 1;
    parsing_cache_dir = "";
    (* "" means no cache *)
    (* Flag used by the semgrep-python wrapper *)
    target_source = None;
    (* Common.ml action for the -dump_xxx *)
    action = "";
    (* Other *)
    version = "";
  }
