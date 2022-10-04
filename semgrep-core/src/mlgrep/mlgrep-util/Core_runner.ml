(*
   Translated from core_runner.py
*)

(*
   Don't translate this:

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
   Implement this but skip parsing the semgrep-core output, getting it directly
   as OCaml objects.

   Big class (500 lines of python):

class CoreRunner:
    """
    Handles interactions between semgrep and semgrep-core

    This includes properly invoking semgrep-core and parsing the output
    """
*)

module C = Semgrep_output_v0_t

type path = string

type result = {
  (* TODO: key by rule ID rather than whole rule? *)
  findings_by_rule : (Rule.t, Rule_match.t list) Map_.t;
  errors : Error.t list;
  all_targets : path Set_.t;
  (*profiling_data: profiling_data; TODO: do we need to translate this? *)
  parsing_data : Parsing_data.t;
  explanations : C.matching_explanation list option;
}

(*
   Take in rules and targets and return object with findings.
*)
let invoke_semgrep ~(jobs : int) ~(timeout : int) ~(max_memory : int)
    ~(timeout_threshold : int) ~(optimizations : int)
    ?(core_opts_str :
       string option (* string to be split according to shell parsing rules *))
    ~(target_manager : Target_manager.t) ~(rules : Rule.t list)
    ~(dump_command_for_core : bool) ~(deep : bool) () : result =
  ignore jobs;
  ignore timeout;
  ignore max_memory;
  ignore timeout_threshold;
  ignore optimizations;
  ignore core_opts_str;
  ignore target_manager;
  ignore rules;
  ignore dump_command_for_core;
  ignore deep;
  {
    findings_by_rule = Map_.empty;
    errors = [];
    all_targets = Set_.empty;
    parsing_data = Parsing_data.TODO;
    explanations = None;
  }
