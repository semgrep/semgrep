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

let merge_lists get_list xs = Common.map get_list xs |> List.flatten

let merge_results result_list : Report.final_result =
  let open Report in
  (* TODO: do something with the exceptions if they're not already in the
     'errors' field. *)
  let _exceptions = List.filter_map (fun (x, _) -> x) result_list in
  let results = Common.map snd result_list in
  {
    matches = merge_lists (fun x -> x.matches) results;
    errors = merge_lists (fun x -> x.errors) results;
    skipped_rules = merge_lists (fun x -> x.skipped_rules) results;
    extra = No_info;
    explanations = merge_lists (fun x -> x.explanations) results;
  }

let call_semgrep_core ~num_jobs ~timeout ~timeout_threshold ~max_memory_mb
    ~use_optimizations ~debug ~rules ~targets =
  let runner_config : Runner_config.t =
    {
      Runner_config.default with
      ncores = num_jobs;
      timeout;
      timeout_threshold;
      max_memory_mb;
      debug;
      version = Version.version;
    }
  in
  let lang_jobs =
    (* TODO: split and filter input by language *)
    []
  in
  (* TODO: This is the -fast or -filter_irrelevant_rules flag of semgrep-core.
     It's currently a global, mutable variable. Move it the config object
     so we can use it easily. *)
  ignore use_optimizations;
  let results_by_language =
    Common.map
      (Run_semgrep.semgrep_with_prepared_rules_and_targets runner_config)
      lang_jobs
  in
  merge_results results_by_language

(*
   Take in rules and targets and return object with findings.

   ?core_opts_str: extra arguments passed to semgrep-core that need splitting
   on whitespace. We will no longer be able to support this since
   the semgrep-core CLI itself is going away.
*)
let invoke_semgrep (conf : Scan_CLI.conf) : result =
  let rules, errors =
    (* TODO: resolve rule file URLs; for now we assume it's a file. *)
    Parse_rule.parse_and_filter_invalid_rules conf.config
  in
  (* TODO: don't ignore errors *)
  ignore errors;
  let targets, skipped_targets =
    Find_target.select_global_targets ~includes:conf.include_
      ~excludes:conf.exclude ~max_target_bytes:conf.max_target_bytes
      ~respect_git_ignore:conf.respect_git_ignore conf.target_roots
  in
  let _res =
    call_semgrep_core ~num_jobs:conf.num_jobs ~timeout:conf.timeout
      ~timeout_threshold:conf.timeout_threshold
      ~max_memory_mb:conf.max_memory_mb ~use_optimizations:conf.optimizations
      ~debug:conf.debug ~rules ~targets
  in
  (* TODO: figure out the best type for this result. Ideally, this is
     the type corresponding to the JSON output. *)
  ignore skipped_targets;
  {
    findings_by_rule = Map_.empty;
    errors = [];
    all_targets = Set_.empty;
    parsing_data = Parsing_data.TODO;
    explanations = None;
  }
