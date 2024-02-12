open Common
open Fpath_.Operators
module OutJ = Semgrep_output_v1_t
module Env = Semgrep_envvars

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Translated from core_runner.py and core_output.py

   LATER: we should remove this file and call directly Core_scan
   and not go through the intermediate semgrep-core JSON output.
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(* input *)
type conf = {
  (* opti and limits *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int; (* output flags *)
  nosem : bool;
  strict : bool;
  time_flag : bool;
  matching_explanations : bool;
  (* TODO: actually seems like semgrep-core always return them,
   * even if it was not requested by the CLI
   *)
  dataflow_traces : bool;
  (* osemgrep-only: *)
  ast_caching : bool;
}
[@@deriving show]

(* output *)
(* LATER: ideally we should just return Core_result.t
   without the need for the intermediate Out.core_output.
*)
type result = {
  (* ocaml: not in original python implem, but just enough to get
   * Semgrep_scan.cli_output_of_core_results to work
   *)
  core : OutJ.core_output;
  hrules : Rule.hrules;
  scanned : Fpath.t Set_.t;
      (* in python implem *)
      (* TODO: original intermediate data structures in python *)
      (*
     findings_by_rule : (Rule.t, Rule_match.t list) Map_.t;
     errors : Error.t list;
     all_targets: path Set_.t;
     (*profiling_data: profiling_data; TOPORT: do we need to translate this? *)
     parsing_data : Parsing_data.t;
     explanations : Out.matching_explanation list option;
  *)
}

(* Type for the scan function, which can either be built by
   mk_scan_func_for_osemgrep() or set in Scan_subcommand.hook_pro_scan_func *)

type scan_func_for_osemgrep =
  ?respect_git_ignore:bool ->
  ?file_match_results_hook:
    (Fpath.t -> Core_result.matches_single_file -> unit) option ->
  conf ->
  (* LATER? use Config_resolve.rules_and_origin instead? *)
  Rule.rules ->
  Rule.invalid_rule_error list ->
  Fpath.t list ->
  Core_result.result_or_exn

(*****************************************************************************)
(* To run a Pro scan (Deep scan and multistep scan) *)
(*****************************************************************************)

(* Semgrep Pro hook. Note that this is useful only for osemgrep. Indeed,
 * for pysemgrep the code path is instead to fork the
 * semgrep-core-proprietary program, which executes Pro_CLI_main.ml
 * which then calls Run.ml code which is mostly a copy-paste of Core_scan.ml
 * with the Pro scan specifities hard-coded (no need for hooks).
 * We could do the same for osemgrep, but that would require to copy-paste
 * lots of code, so simpler to use a hook instead.
 *
 * Note that Scan_subcommand.ml itself is linked in (o)semgrep-pro,
 * and executed by osemgrep-pro. When linked from osemgrep-pro, this
 * hook below will be set.
 *)
let (hook_pro_scan_func_for_osemgrep :
      (Fpath.t list ->
      ?diff_config:Differential_scan_config.t ->
      Engine_type.t ->
      scan_func_for_osemgrep)
      option
      ref) =
  ref None

(* This hooks into the proprietary part of Semgrep, in order to access a
 * function that helps us quickly checkout and scan a remote git repo.
 * If a repo is checked out sparsely, this will only checkout the files
 * that are needed for the scan.
 *)
let (hook_pro_git_remote_scan_setup :
      (Find_targets.git_remote ->
      scan_func_for_osemgrep ->
      scan_func_for_osemgrep)
      option
      ref) =
  ref None

(*************************************************************************)
(* Extract mode *)
(*************************************************************************)

(* TODO? move to Xlang.ml? *)
module XlangSet = Set.Make (struct
  let compare
      (* This only compares the first language in the case of `L (lang :: _)`.
         That should be fine because for the use of Xlang in this file,
         `L _` should always be flattened out *)
        a b =
    String.compare (Xlang.to_string a) (Xlang.to_string b)

  type t = Xlang.t
end)

(* Extract mode: we need to make sure to include rules that will apply
   to targets that have been extracted. To do that, we'll detect the languages
   that targets might be extracted to and include them in the jobs *)
(* TODO it would be nicer to just extract the targets before splitting jobs,
   but that would cause us to change things for the Core_scan shared path *)
let detect_extract_languages all_rules =
  all_rules
  |> List.fold_left
       (fun acc { Rule.mode; _ } ->
         match mode with
         | `Extract { Rule.dst_lang; _ } -> XlangSet.add dst_lang acc
         | _ -> acc)
       XlangSet.empty

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(* The same rule may appear under multiple target languages because
   some patterns can be interpreted in multiple languages.
   TODO? could use Common.group_by
*)
let group_rules_by_target_language rules : (Xlang.t * Rule.t list) list =
  (* target language -> rules *)
  let tbl = Hashtbl.create 100 in
  rules
  |> List.iter (fun (rule : Rule.t) ->
         let pattern_lang = rule.target_analyzer in
         let target_langs = Xlang.flatten pattern_lang in
         target_langs
         |> List.iter (fun lang ->
                let rules =
                  match Hashtbl.find_opt tbl lang with
                  | None -> []
                  | Some rules -> rules
                in
                Hashtbl.replace tbl lang (rule :: rules)));
  Hashtbl.fold (fun lang rules acc -> (lang, rules) :: acc) tbl []

let add_typescript_to_javascript_rules_hack (all_rules : Rule.t list) :
    Rule.t list =
  (* If Javascript is one of the rule languages, we should also run on
     Typescript files. This implementation mimics the hack in `rule.py`.
     We could alternatively set this by changing lang.json, but we should
     be careful to do that without affecting other things like the docs *)
  all_rules
  |> List_.map (fun r ->
         match r.Rule.target_analyzer with
         | LRegex
         | LSpacegrep
         | LAliengrep ->
             r
         | L (l, ls) ->
             let lset = Set_.of_list ls in
             let lset =
               if l =*= Language.Js || Set_.mem Language.Js lset then
                 Set_.add Language.Ts lset
               else lset
             in
             { r with Rule.target_analyzer = L (l, lset |> Set_.elements) })

let split_jobs_by_language all_rules all_targets : Lang_job.t list =
  let all_rules = add_typescript_to_javascript_rules_hack all_rules in
  let extract_languages = detect_extract_languages all_rules in
  all_rules |> group_rules_by_target_language
  |> List_.map_filter (fun (xlang, rules) ->
         let targets =
           all_targets
           |> List.filter (Filter_target.filter_target_for_xlang xlang)
         in
         if List_.null targets && not (XlangSet.mem xlang extract_languages)
         then None
         else Some ({ xlang; targets; rules } : Lang_job.t))

let core_scan_config_of_conf (conf : conf) : Core_scan_config.t =
  match conf with
  | {
   num_jobs;
   timeout;
   timeout_threshold;
   max_memory_mb;
   optimizations;
   ast_caching;
   matching_explanations;
   nosem;
   strict;
   (* TODO *)
   time_flag = _;
   dataflow_traces = _;
  } ->
      (* We default to Json because we do not want the current text
       * displayed in semgrep-core, and we don't want either the
       * current semgrep-core incremental matches text output.
       *)
      let output_format = Core_scan_config.Json false (* no dots *) in
      let filter_irrelevant_rules = optimizations in
      let parsing_cache_dir =
        if ast_caching then Some (!Env.v.user_dot_semgrep_dir / "cache" / "asts")
        else None
      in
      {
        Core_scan_config.default with
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        filter_irrelevant_rules;
        parsing_cache_dir;
        matching_explanations;
        nosem;
        strict;
        version = Version.version;
      }

let prepare_config_for_core_scan (config : Core_scan_config.t)
    (lang_jobs : Lang_job.t list) =
  let target_mappings_of_lang_job (x : Lang_job.t) : Target.t list * Rule.rules
      =
    let target_mappings =
      x.targets
      |> List_.map (fun (path : Fpath.t) : Target.t ->
             Code (Target.code_of_source x.xlang Product.all (File path)))
    in
    (target_mappings, x.rules)
  in
  let target_mappings, rules =
    lang_jobs
    |> List.fold_left
         (fun (acc_mappings, acc_rules) lang_job ->
           let mappings, rules = target_mappings_of_lang_job lang_job in
           (mappings :: acc_mappings, List.rev rules :: acc_rules))
         ([], [])
  in
  let target_mappings = List.concat target_mappings in
  let rules = rules |> List.rev |> List.concat in
  let targets : Target.t list = target_mappings in
  {
    config with
    target_source = Some (Targets targets);
    rule_source = Some (Rules rules);
  }

(* Create the core result structure from the results *)
(* LATER: we want to avoid this intermediate data structure but
 * for now that's what pysemgrep used to get so simpler to return it.
 *)
let create_core_result (all_rules : Rule.rule list)
    (result_or_exn : Core_result.result_or_exn) =
  (* similar to Core_command.output_core_results code *)
  let res =
    match result_or_exn with
    | Ok r -> r
    | Error (exn, _core_error_opt) ->
        (* TODO: use _core_error_opt instead? reraise the exn instead?
         * TOADAPT? Runner_exit.exit_semgrep (Unknown_exception e) instead.
         *)
        let err = Core_error.exn_to_error None "" exn in
        Core_result.mk_final_result_with_just_errors [ err ]
  in
  let scanned = Set_.of_list res.scanned in
  let match_results = Core_json_output.core_output_of_matches_and_errors res in
  (* TOPORT? or move in semgrep-core so get info ASAP
     if match_results.skipped_targets:
         for skip in match_results.skipped_targets:
             if skip.rule_id:
                 rule_info = f"rule {skip.rule_id}"
             else:
                 rule_info = "all rules"
             logger.verbose(
                 f"skipped '{skip.path}' [{rule_info}]: {skip.reason}: {skip.details}"
             )
  *)
  { core = match_results; hrules = Rule.hrules_of_rules all_rules; scanned }

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(*
   Take in rules and targets and return object with findings.
*)
let mk_scan_func_for_osemgrep (core_scan_func : Core_scan.core_scan_func) :
    scan_func_for_osemgrep =
 fun ?(respect_git_ignore = true) ?(file_match_results_hook = None)
     (conf : conf) (all_rules : Rule.t list)
     (invalid_rules : Rule.invalid_rule_error list) (all_targets : Fpath.t list)
     : Core_result.result_or_exn ->
  let rule_errors = Core_scan.errors_of_invalid_rule_errors invalid_rules in
  let config : Core_scan_config.t = core_scan_config_of_conf conf in
  let config = { config with file_match_results_hook } in
  (* TODO: we should not need to use List_.map below, because
     Run_semgrep.semgrep_with_raw_results_and_exn_handler can accept
     a list of targets with different languages! We just
     need to pass the right target object (and not a lang_job)
     TODO: Martin said the issue was that Run_semgrep.targets_of_config
     requires the xlang object to contain a single language.
     TODO: Martin says there's no fundamental reason to split
     a scanning job by programming language. Several optimizations
     are possible based on target project structure, number and diversity
     of rules, presence of rule-specific include/exclude patterns etc.
     Right now we're constrained by the pysemgrep/semgrep-core interface
     that requires a split by "language". While this interface is still
     in use, bypassing it without removing it seems complicated.
     See https://www.notion.so/r2cdev/Osemgrep-scanning-algorithm-5962232bfd74433ba50f97c86bd1a0f3
  *)
  let lang_jobs = split_jobs_by_language all_rules all_targets in
  let rules_with_targets =
    List.concat_map (fun { Lang_job.rules; _ } -> rules) lang_jobs
    |> List_.uniq_by Stdlib.( == )
  in
  Logs.app (fun m ->
      m "%a"
        (fun ppf () ->
          (* TODO: validate if target is actually within a git repo and perhaps set respect_git_ignore to false otherwise *)
          Status_report.pp_status ~num_rules:(List.length all_rules)
            ~num_targets:(List.length all_targets) ~respect_git_ignore lang_jobs
            ppf)
        ());
  List.iter
    (fun { Lang_job.xlang; _ } ->
      Metrics_.add_feature "language" (Xlang.to_string xlang))
    lang_jobs;
  let config = prepare_config_for_core_scan config lang_jobs in

  (* !!!!Finally! this is where we branch to semgrep-core core scan fun!!! *)
  let result_or_exn = core_scan_func config in
  match result_or_exn with
  | Error _ -> result_or_exn
  | Ok res ->
      (* Reinject rule errors *)
      let res =
        {
          res with
          errors = rule_errors @ res.errors;
          skipped_rules = invalid_rules @ res.skipped_rules;
          rules_with_targets;
        }
      in

      let scanned = Set_.of_list res.scanned in

      (* TODO(dinosaure): currently, we don't collect metrics when we invoke
         semgrep-core but we should. However, if we implement a way to collect
         metrics, we will just need to set [final_result.extra] to
         [Core_result.Debug]/[Core_result.Time] and this line of code will not change. *)
      Metrics_.add_max_memory_bytes
        (Core_profiling.debug_info_to_option res.extra);
      Metrics_.add_targets_stats scanned
        (Core_profiling.debug_info_to_option res.extra);
      Ok res
[@@profiling]
