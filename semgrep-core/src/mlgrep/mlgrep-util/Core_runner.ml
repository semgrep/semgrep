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

type path = string

type result = {
  (* TODO: key by rule ID rather than whole rule? *)
  findings_by_rule : (Rule.t, Rule_match.t list) Map_.t;
  errors : Error.semgrep_error list;
  all_targets : path Set_.t;
  (*profiling_data: profiling_data; TODO: do we need to translate this? *)
  parsing_date : parsing_data;
  explanations : matching_explanation list option;
}

(*
   Take in rules and targets and return object with findings.
*)
let invoke_semgrep ~jobs ~(* int *) timeout ~(* int *) max_memory (* int *)
    ~timeout_threshold ~(* int *) optimizations (* int *)
    ?core_opts_str (* string to be split according to shell parsing rules *)
    ~target_manager ~(* TargetManager *) rules (* rule list *)
    ~(*~dump_command_for_core (* bool *)*)
     deep (* bool *) () : result =
  {
    findings_by_rule = Map_.empty;
    errors = [];
    all_targets;
    profiling_data;
    parsing_date;
    explanations;
  }
