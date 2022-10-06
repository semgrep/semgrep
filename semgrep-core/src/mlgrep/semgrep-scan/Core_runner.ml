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

let call_semgrep_core ~num_jobs ~timeout ~timeout_threshold ~max_memory
    ~use_optimizations ~debug ~rules ~targets =
  let runner_config : Runner_config.t =
    {
      Runner_config.default with
      rule_source = Some (Rules rules);
      ncores = num_jobs;
      target_source = Some (Targets targets);
      timeout;
      timeout_threshold;
      max_memory_mb = max_memory;
      debug;
      version = Version.version;
    }
  in
  (* TODO: This is the -fast or -filter_irrelevant_rules flag of semgrep-core.
     It's currently a global, mutable variable. Move it the config object
     so we can use it easily. *)
  ignore use_optimizations;
  Run_semgrep.semgrep_with_raw_results_and_exn_handler runner_config

(*
   Take in rules and targets and return object with findings.

   ?core_opts_str: extra arguments passed to semgrep-core that need splitting
   on whitespace. We will no longer be able to support this since
   the semgrep-core CLI itself is going away.
*)
let invoke_semgrep ~num_jobs ~timeout ~timeout_threshold ~max_memory
    ~use_optimizations ~debug ~rules ~targets : result =
  let _res =
    call_semgrep_core ~num_jobs ~timeout ~timeout_threshold ~max_memory
      ~use_optimizations ~debug ~rules ~targets
  in
  {
    findings_by_rule = Map_.empty;
    errors = [];
    all_targets = Set_.empty;
    parsing_data = Parsing_data.TODO;
    explanations = None;
  }
