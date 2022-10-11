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

(* The same rule may appear under multiple target languages because
   some patterns can be interpreted in multiple languages. *)
let group_rules_by_target_language rules : (Xlang.t * Rule.t list) list =
  (* target language -> rules *)
  let tbl = Hashtbl.create 100 in
  List.iter
    (fun (rule : Rule.t) ->
      let pattern_lang = rule.languages in
      let target_langs = Xlang.flatten pattern_lang in
      List.iter
        (fun lang ->
          let rules =
            match Hashtbl.find_opt tbl lang with
            | None -> []
            | Some rules -> rules
          in
          Hashtbl.replace tbl lang (rule :: rules))
        target_langs)
    rules;
  Hashtbl.fold (fun lang rules acc -> (lang, rules) :: acc) tbl []

let split_jobs_by_language all_rules all_targets : Runner_config.lang_job list =
  let grouped_rules = group_rules_by_target_language all_rules in
  let cache = Find_target.create_cache () in
  Common.map
    (fun (lang, rules) ->
      let targets =
        List.filter
          (fun target ->
            List.exists
              (fun (rule : Rule.t) ->
                let required_path_patterns, excluded_path_patterns =
                  match rule.paths with
                  | Some { include_; exclude } -> (include_, exclude)
                  | None -> ([], [])
                in
                Find_target.filter_target_for_lang ~cache ~lang
                  ~required_path_patterns ~excluded_path_patterns target)
              rules)
          all_targets
      in
      ({ lang; targets; rules } : Runner_config.lang_job))
    grouped_rules

let call_semgrep_core ~num_jobs ~timeout ~timeout_threshold ~max_memory_mb
    ~use_optimizations ~debug ~all_rules ~all_targets =
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
  let lang_jobs = split_jobs_by_language all_rules all_targets in
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
      ~debug:conf.debug ~all_rules:rules ~all_targets:targets
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
