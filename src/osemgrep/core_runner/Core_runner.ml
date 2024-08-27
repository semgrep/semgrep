open Common
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

(* input
 * LATER: ideally we can cleanup Core_scan_config.ml enough that we don't
 * need this extra type can can just reuse Core_scan_config.t as is.
 *)
type conf = {
  (* opti and limits *)
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int; (* output flags *)
  (* features *)
  (* TODO: move nosem in Scan_CLI.conf and handled it Scan_subcommand.ml.
   * Core_scan does not use nosem anymore, or more precisely it always
   * have nosem=true and return findings with ignore=false and assume
   * the caller will handle the ignored findings.
   *)
  nosem : bool;
  strict : bool;
  (* useful for debugging rules *)
  time_flag : bool;
  matching_explanations : bool;
  (* TODO: actually seems like semgrep-core always return them,
   * even if it was not requested by the CLI
   *)
  dataflow_traces : bool;
}
[@@deriving show]

type pro_conf = {
  diff_config : Differential_scan_config.t;
  (* TODO: change to root: Fpath.t, like in Deep_scan_config.interfile_config *)
  roots : Scanning_root.t list;
  engine_type : Engine_type.t;
}

(* output *)
(* LATER: ideally we should just return Core_result.t
   without the need for the intermediate Out.core_output.
*)
type result = {
  (* ocaml: not in original python implem, but just enough to get
   * Cli_json_output.cli_output_of_core_results to work
   *)
  core : Semgrep_output_v1_t.core_output;
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
   mk_core_run_for_osemgrep() or set in Scan_subcommand.hook_pro_scan_func

   This is a record in an attempt to produce clearer error messages than
   with a type alias.

   It doesn't scan the filesystem since it takes a list of target files,
   not scanning roots.
*)
type func = {
  run :
    ?file_match_hook:(Fpath.t -> Core_result.matches_single_file -> unit) option ->
    conf ->
    (* TODO alt: pass a bool alongside each target path that indicates whether
       the target is explicit i.e. occurs directly on the command line *)
    Find_targets.conf ->
    (* LATER? alt: use Config_resolve.rules_and_origin instead? *)
    Rule_error.rules_and_invalid ->
    (* Takes a list of target files, not scanning roots. *)
    Fpath.t list ->
    Core_result.result_or_exn;
}

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
let (hook_mk_pro_core_run_for_osemgrep : (pro_conf -> func) option ref) =
  ref None

(* This hooks into the proprietary part of Semgrep, in order to access a
 * function that helps us quickly checkout and scan a remote git repo.
 * If a repo is checked out sparsely, this will only checkout the files
 * that are needed for the scan.
 *)
let (hook_pro_git_remote_scan_setup : (func -> func) option ref) = ref None

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

let split_jobs_by_language (conf : Find_targets.conf) all_rules all_targets :
    Lang_job.t list =
  let all_rules = add_typescript_to_javascript_rules_hack all_rules in
  let extract_languages = detect_extract_languages all_rules in
  all_rules |> group_rules_by_target_language
  |> List_.filter_map (fun (xlang, rules) ->
         let targets =
           all_targets
           |> List.filter (fun path ->
                  (* bypass normal analyzer detection for explicit targets with
                     '--scan-unknown-extensions' *)
                  let bypass_language_detection =
                    conf.always_select_explicit_targets
                    && Find_targets.Explicit_targets.mem conf.explicit_targets
                         path
                  in
                  bypass_language_detection
                  || Filter_target.filter_target_for_xlang xlang path)
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
   matching_explanations;
   nosem = _TODO;
   strict;
   time_flag;
   (* TODO *)
   dataflow_traces = _;
  } ->
      (* We do our own output in osemgrep, no need for Core_scan.scan() output *)
      let output_format = Core_scan_config.NoOutput in
      let filter_irrelevant_rules = optimizations in
      {
        Core_scan_config.default with
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        filter_irrelevant_rules;
        matching_explanations;
        strict;
        report_time = time_flag;
      }

let prepare_config_for_core_scan (config : Core_scan_config.t)
    (lang_jobs : Lang_job.t list) =
  let targets_and_rules_of_lang_job (x : Lang_job.t) :
      Target.t list * Rule.rules =
    let targets =
      x.targets
      |> List_.map (fun (path : Fpath.t) : Target.t ->
             Regular (Target.mk_regular x.xlang Product.all (File path)))
    in
    (targets, x.rules)
  in
  let targets, rules =
    List_.fold_right
      (fun lang_job (acc_targets, acc_rules) ->
        let targets, rules = targets_and_rules_of_lang_job lang_job in
        (List_.append targets acc_targets, List_.append rules acc_rules))
      lang_jobs ([], [])
  in
  {
    config with
    target_source = Some (Targets targets);
    rule_source = Rules rules;
  }

(* LATER: we want to avoid this intermediate data structure but
 * for now that's what pysemgrep used to get so simpler to return it.
 *)
let mk_result (all_rules : Rule.rule list) (res : Core_result.t) : result =
  (* similar to Core_command.output_core_results code *)
  let scanned = res.scanned |> List_.map Target.internal_path |> Set_.of_list in
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

(* Core_scan.core_scan_func adapter for osemgrep *)
let mk_core_run_for_osemgrep (core_scan_func : Core_scan.func) : func =
  let run ?(file_match_hook = None) (conf : conf)
      (targeting_conf : Find_targets.conf)
      (rules_and_invalid : Rule_error.rules_and_invalid)
      (all_targets : Fpath.t list) : Core_result.result_or_exn =
    (*
       At this point, we already have the full list of targets. These targets
       will populate the 'target_source' field of the config object
       (after splitting into "languages").
       This mode doesn't tolerate scanning roots. This is checked in
       Core_scan.ml.
    *)
    let rules, invalid_rules = rules_and_invalid in
    let rule_errors : Core_error.t list =
      invalid_rules |> List_.map Core_error.error_of_invalid_rule
    in
    let config : Core_scan_config.t = core_scan_config_of_conf conf in
    let config = { config with file_match_hook } in
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
    let lang_jobs = split_jobs_by_language targeting_conf rules all_targets in
    let rules_with_targets =
      List.concat_map (fun { Lang_job.rules; _ } -> rules) lang_jobs
      |> (* TODO: if this is using physical equality on purpose,
            explain why because otherwise it looks like a bug. *)
      List_.uniq_by Stdlib.( == )
    in
    Logs.app (fun m ->
        m "%a"
          (fun ppf () ->
            (* TODO: validate if target is actually within a git repo and
               perhaps set respect_git_ignore to false otherwise *)
            Status_report.pp_status ~num_rules:(List.length rules)
              ~num_targets:(List.length all_targets)
              ~respect_gitignore:targeting_conf.respect_gitignore lang_jobs ppf)
          ());
    lang_jobs
    |> List.iter (fun { Lang_job.xlang; _ } ->
           Metrics_.add_feature "language" (Xlang.to_string xlang));
    let config = prepare_config_for_core_scan config lang_jobs in

    (* !!!!Finally! this is where we branch to semgrep-core core scan fun!!! *)
    let/ res = core_scan_func config in
    (* Reinject rule errors *)
    let res =
      {
        res with
        errors = rule_errors @ res.errors;
        skipped_rules = invalid_rules @ res.skipped_rules;
        rules_with_targets;
      }
    in

    let scanned =
      res.scanned |> List_.map Target.internal_path |> Set_.of_list
    in

    (* TODO(dinosaure): currently, we don't collect metrics when we invoke
       semgrep-core but we should. However, if we implement a way to collect
       metrics, we will just need to set [final_result.extra] to
       [Core_result.Debug]/[Core_result.Time] and this line of code will not change. *)
    Metrics_.add_max_memory_bytes res.profiling;
    Metrics_.add_targets_stats scanned res.profiling;
    Ok res
  in
  { run }
[@@profiling]
