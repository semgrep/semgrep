module C = Semgrep_output_v0_t

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
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

(*************************************************************************)
(* Types *)
(*************************************************************************)

type path = string

type result = {
  (* TODO: key by rule ID rather than whole rule? *)
  findings_by_rule : (Rule.t, Rule_match.t list) Map_.t;
  errors : Error.t list;
  all_targets : path Set_.t;
  (*profiling_data: profiling_data; TOPORT: do we need to translate this? *)
  parsing_data : Parsing_data.t;
  explanations : C.matching_explanation list option;
}

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

let merge_lists get_list xs = List.concat_map get_list xs

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

let runner_config_of_conf (conf : Scan_CLI.conf) : Runner_config.t =
  (* TODO: This is the -fast or -filter_irrelevant_rules flag of semgrep-core.
      It's currently a global, mutable variable. Move it the config object
      so we can use it easily.
     conf.use_optimizations;
  *)
  match conf with
  | {
   num_jobs;
   timeout;
   timeout_threshold;
   max_memory_mb;
   debug;
   output_format;
   optimizations;
   (* TOPORT: not handled yet *)
   autofix = _;
   baseline_commit = _;
   config = _;
   exclude = _;
   include_ = _;
   lang = _;
   max_target_bytes = _;
   metrics = _;
   pattern = _;
   quiet = _;
   respect_git_ignore = _;
   target_roots = _;
   verbose = _;
  } ->
      let output_format =
        match output_format with
        | Json -> Runner_config.Json false (* no dots *)
        (* TOPORT: I think also in Text mode we should default
         * to Json because we do not want the same text displayed in
         * osemgrep than in semgrep-core
         *)
        | Text -> Runner_config.Text
        (* defaulting to Json, which really mean just no incremental
         * display of match in the match_hook in semgrep-core
         *)
        | _else_ -> Runner_config.Json false
      in
      let filter_irrelevant_rules = optimizations in
      {
        Runner_config.default with
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        debug;
        filter_irrelevant_rules;
        version = Version.version;
      }

let call_semgrep_core conf all_rules all_targets =
  let config : Runner_config.t = runner_config_of_conf conf in

  (* TOADAPT
     (* this used to be in Core_CLI.ml but we get a config object
      * later in osemgrep
      *)
     let config =
       if config.profile then (
         logger#info "Profile mode On";
         logger#info "disabling -j when in profiling mode";
         { config with ncores = 1 })
       else config
     in
  *)
  let lang_jobs = split_jobs_by_language all_rules all_targets in
  let results_by_language =
    Common.map
      (* !!!!Finally! this is where we branch to semgrep-core!!! *)
      (Run_semgrep.semgrep_with_prepared_rules_and_targets config)
      lang_jobs
  in
  merge_results results_by_language

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(*
   Take in rules and targets and return object with findings.

   ?core_opts_str: extra arguments passed to semgrep-core that need splitting
   on whitespace. We will no longer be able to support this since
   the semgrep-core CLI itself is going away.
*)
let invoke_semgrep (conf : Scan_CLI.conf) : result =
  let rules, errors =
    (* TOPORT: resolve rule file URLs; for now we assume it's a file. *)
    Parse_rule.parse_and_filter_invalid_rules conf.config
  in
  (* TOPORT: don't ignore errors *)
  ignore errors;
  let targets, skipped_targets =
    Find_target.select_global_targets ~includes:conf.include_
      ~excludes:conf.exclude ~max_target_bytes:conf.max_target_bytes
      ~respect_git_ignore:conf.respect_git_ignore conf.target_roots
  in
  (* !!!TODO!!! use the result!! *)
  let _res = call_semgrep_core conf rules targets in
  (* TOPORT: figure out the best type for this result. Ideally, this is
     the type corresponding to the JSON output. *)
  ignore skipped_targets;
  {
    findings_by_rule = Map_.empty;
    errors = [];
    all_targets = Set_.empty;
    parsing_data = Parsing_data.TODO;
    explanations = None;
  }
