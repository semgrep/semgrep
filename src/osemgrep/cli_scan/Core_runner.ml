module Out = Semgrep_output_v1_t
module RP = Report

(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(*
   Translated from core_runner.py and core_output.py

   LATER: we should remove this file and call directly Run_semgrep
   and not go through the intermediate semgrep-core JSON output.
*)

(* python: Don't translate this:

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
   python: implement this but skip parsing the semgrep-core output,
   getting it directly as OCaml objects.
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

(* input *)
type conf = {
  num_jobs : int;
  optimizations : bool;
  max_memory_mb : int;
  timeout : float;
  timeout_threshold : int;
}
[@@deriving show]

(* output *)
(* LATER: ideally we should just return what Run_semgrep returns,
   without the need for the intermediate Out.core_match_results.
   LATER: get rid of Output_from_core_util.ml
*)
type result = {
  (* ocaml: not in original python implem, but just enough to get
   * Semgrep_scan.cli_output_of_core_results to work
   *)
  core : Out.core_match_results;
  hrules : Rule.hrules;
  scanned : Common.filename Set_.t;
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

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(* TODO: should return also exn opt,
 * like the return type of semgrep_with_raw_results_and_exn_handler.
 * TODO: we should not even need this function because
 * Run_semgrep.semgrep_with_raw_results_and_exn_handler can already
 * handle a list of targets in different language and so no need to
 * merge results in the first place.
 *
 * TODO? do we have utility functions like that already in Report.mli?
 * should move it there? or should not need it at all, see TODO above?
 *)
let merge_results
    (xresults : (Report.final_result * Common.filename Set_.t) list) :
    Report.final_result * Common.filename Set_.t =
  let results = xresults |> Common.map fst in
  let files =
    xresults |> Common.map snd |> List.fold_left Set_.union Set_.empty
  in
  let final_result =
    {
      RP.matches = List.concat_map (fun x -> x.RP.matches) results;
      errors = List.concat_map (fun x -> x.RP.errors) results;
      skipped_rules = List.concat_map (fun x -> x.RP.skipped_rules) results;
      extra = No_info;
      explanations = List.concat_map (fun x -> x.RP.explanations) results;
      rules_by_engine =
        List.concat_map (fun x -> x.Report.rules_by_engine) results;
    }
  in
  (final_result, files)

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

let runner_config_of_conf (conf : conf) : Runner_config.t =
  match conf with
  | { num_jobs; timeout; timeout_threshold; max_memory_mb; optimizations }
  (* TODO: time_flag = _;
  *) ->
      (* We should default to Json because we do not want the same text
       * displayed in osemgrep than in semgrep-core.
       * We also don't want the current incremental matches output.
       * LATER: probably provide a mechanism to display match as
       * we process files, but probably need different text output
       * of what semgrep-core match_hook currently provides.
       *)
      let output_format = Runner_config.Json false (* no dots *) in
      let filter_irrelevant_rules = optimizations in
      {
        Runner_config.default with
        ncores = num_jobs;
        output_format;
        timeout;
        timeout_threshold;
        max_memory_mb;
        filter_irrelevant_rules;
        version = Version.version;
      }

let pp_status rules targets lang_jobs respect_git_ignore ppf () =
  Fmt_helpers.pp_heading ppf "Scan status";
  (* TODO indentation of the body *)
  let pp_s ppf x = if x = 1 then Fmt.string ppf "" else Fmt.string ppf "s" in
  let rule_count = List.length rules in
  Fmt.pf ppf "Scanning %d files%s with %d Code rule%a" (List.length targets)
    (if respect_git_ignore then " tracked by git" else "")
    rule_count pp_s rule_count;
  (* TODO if sca_rules ...
     Fmt.(option ~none:(any "") (any ", " ++ int ++ any "Supply Chain rule" *)
  (* TODO pro_rule
         if get_path(rule.metadata, ("semgrep.dev", "rule", "origin"), default=None)
         == "pro_rules"
     if pro_rule_count:
         summary_line += f", {unit_str(pro_rule_count, 'Pro rule')}"
  *)
  Fmt.pf ppf ":@.@.";
  (* TODO origin table [Origin Rules] [Community N] *)
  Fmt_helpers.pp_table
    ("Language", [ "Rules"; "Files" ])
    ppf
    (lang_jobs
    |> List.fold_left
         (fun acc Runner_config.{ lang; targets; rules } ->
           (Xlang.to_string lang, [ List.length rules; List.length targets ])
           :: acc)
         []
    |> List.rev)

let errors_to_skipped (errors : Out.core_error list) : Out.skipped_target list =
  errors
  |> Common.map (fun Out.{ location; message; rule_id; _ } ->
         Out.
           {
             path = location.path;
             reason = Analysis_failed_parser_or_internal_error;
             details = message;
             rule_id;
           })

let analyze_skipped (skipped : Out.skipped_target list) =
  let reason_ht = Hashtbl.create 13 in
  List.iter
    (fun r -> Hashtbl.add reason_ht r [])
    Out.
      [
        Semgrepignore_patterns_match;
        Cli_include_flags_do_not_match;
        Cli_exclude_flags_match;
        Exceeded_size_limit;
      ];
  let skipped_by_reason (Out.{ reason; _ } as e : Out.skipped_target) =
    let reason =
      match reason with
      | Out.Gitignore_patterns_match
      | Semgrepignore_patterns_match ->
          Out.Semgrepignore_patterns_match
      | Too_big
      | Exceeded_size_limit ->
          Out.Exceeded_size_limit
      | Cli_include_flags_do_not_match -> Out.Cli_include_flags_do_not_match
      | Cli_exclude_flags_match -> Out.Cli_exclude_flags_match
      | Always_skipped
      | Analysis_failed_parser_or_internal_error
      | Excluded_by_config
      | Wrong_language
      | Minified
      | Binary
      | Irrelevant_rule
      | Too_many_matches ->
          assert false
    in
    let v = e :: Option.value ~default:[] (Hashtbl.find_opt reason_ht reason) in
    Hashtbl.replace reason_ht reason v
  in
  List.iter skipped_by_reason skipped;
  ( Hashtbl.find reason_ht Out.Semgrepignore_patterns_match,
    Hashtbl.find reason_ht Out.Cli_include_flags_do_not_match,
    Hashtbl.find reason_ht Out.Cli_exclude_flags_match,
    Hashtbl.find reason_ht Out.Exceeded_size_limit )

let pp_summary ppf
    (( _respect_git_ignore,
       semgrep_ignored,
       include_ignored,
       exclude_ignored,
       file_size_ignored,
       errors ) :
      bool
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list) =
  Fmt_helpers.pp_heading ppf "Scan summary";
  (* TODO
        if self.target_manager.baseline_handler:
            limited_fragments.append(
                "Scan was limited to files changed since baseline commit."
            )
  *)
  (* TODO
        elif self.target_manager.respect_git_ignore:
            # Each target could be a git repo, and we respect the git ignore
            # of each target, so to be accurate with this print statement we
            # need to check if any target is a git repo and not just the cwd
            targets_not_in_git = 0
            dir_targets = 0
            for t in self.target_manager.targets:
                if t.path.is_dir():
                    dir_targets += 1
                    try:
                        t.files_from_git_ls()
                    except (subprocess.SubprocessError, FileNotFoundError):
                        targets_not_in_git += 1
                        continue
            if targets_not_in_git != dir_targets:
                limited_fragments.append(f"Scan was limited to files tracked by git.")
  *)
  let opt_msg msg = function
    | [] -> None
    | xs -> Some (string_of_int (List.length xs) ^ " " ^ msg)
  in
  let out_skipped =
    Common.map_filter Fun.id
      [
        opt_msg "files not matching --include patterns" include_ignored;
        opt_msg "files matching --exclude patterns" exclude_ignored;
        opt_msg
          "files larger than {self.target_manager.max_target_bytes / 1000 / \
           1000} MB"
          file_size_ignored;
        opt_msg "files matching .semgrepignore patterns" semgrep_ignored;
      ]
  in
  let out_partial =
    opt_msg
      "files only partially analyzed due to a parsing or internal Semgrep error"
      errors
  in
  match (out_skipped, out_partial) with
  | [], None -> ()
  | xs, parts ->
      (* TODO if limited_fragments:
              for fragment in limited_fragments:
                  message += f"\n  {fragment}" *)
      Fmt.pf ppf "Some files were skipped or only partially analyzed.@.";
      Option.iter (fun txt -> Fmt.pf ppf "  Partially scanned: %s@." txt) parts;
      (match xs with
      | [] -> ()
      | xs ->
          Fmt.pf ppf "  Scan skipped: %s@." (String.concat ", " xs);
          Fmt.pf ppf
            "  For a full list of skipped files, run semgrep with the \
             --verbose flag.@.");
      Fmt.pf ppf "@."

let pp_skipped ppf
    (( respect_git_ignore,
       semgrep_ignored,
       include_ignored,
       exclude_ignored,
       file_size_ignored,
       errors ) :
      bool
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list
      * Out.skipped_target list) =
  Fmt_helpers.pp_heading ppf "Files skipped";
  (* TODO: always skipped *)
  Fmt.pf ppf " Skipped by .gitignore:@.";
  if respect_git_ignore then (
    Fmt.pf ppf " (Disable by passing --no-git-ignore)@.@.";
    Fmt.pf ppf "  o <all files not listed by `git ls-files` were skipped>@.")
  else (
    Fmt.pf ppf " (Disabled with --no-git-ignore)@.@.";
    Fmt.pf ppf "  o <none>@.");
  Fmt.pf ppf "@.";

  let pp_list (xs : Out.skipped_target list) =
    match xs with
    | [] -> Fmt.pf ppf "  o <none>@."
    | xs ->
        List.iter
          (fun (Out.{ path; _ } : Out.skipped_target) ->
            Fmt.pf ppf "  o %s@." path)
          xs
  in

  Fmt.pf ppf " Skipped by .semgrepignore:@.";
  Fmt.pf ppf
    " See: \
     https://semgrep.dev/docs/ignoring-files-folders-code/#understanding-semgrep-defaults)@.@.";
  pp_list semgrep_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " Skipped by --include patterns:@.@.";
  pp_list include_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf " Skipped by --exclude patterns:@.@.";
  pp_list exclude_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf
    " Skipped by limiting to files smaller than \
     {self.target_manager.max_target_bytes} bytes:@.";
  Fmt.pf ppf " (Adjust with the --max-target-bytes flag)@.@.";
  pp_list file_size_ignored;
  Fmt.pf ppf "@.";

  Fmt.pf ppf
    " Skipped by analysis failure due to parsing or internal Semgrep error@.@.";
  pp_list errors;
  Fmt.pf ppf "@."

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(*
   Take in rules and targets and return object with findings.
*)
let invoke_semgrep_core ?(respect_git_ignore = true) (conf : conf)
    (all_rules : Rule.t list) (rule_errors : Rule.invalid_rule_error list)
    ?(ignored_targets = []) (all_targets : Fpath.t list) : result =
  let config : Runner_config.t = runner_config_of_conf conf in

  match rule_errors with
  (* with semgrep-python, semgrep-core is passed all the rules unparsed,
   * and as soon as semgrep-core detects an error in a rule, it raises
   * InvalidRule which is caught and translated to JSON before exiting.
   * Here we emulate the same behavior, even though the rules
   * were parsed in the caller already.
   *)
  | err :: _ ->
      (* like in Run_semgrep.sanity_check_rules_and_invalid_rules *)
      let exn = Rule.Err (Rule.InvalidRule err) in
      (* like in Run_semgrep.semgrep_with_raw_results_and_exn_handler *)
      let e = Exception.catch exn in
      let err = Semgrep_error_code.exn_to_error "" e in
      (* like in semgrep_with_rules_and_formatted_output *)
      let error = JSON_report.error_to_error err in
      let core =
        {
          Out.matches = [];
          errors = [ error ];
          skipped_targets = [];
          skipped_rules = [];
          explanations = [];
          time = None;
          stats = { okfiles = 0; errorfiles = 0 };
          rules_by_engine = [];
          engine_requested = `OSS;
        }
      in
      { core; hrules = Rule.hrules_of_rules all_rules; scanned = Set_.empty }
  | [] ->
      (* TODO: we should not need to use Common.map below, because
       * Run_semgrep.semgrep_with_raw_results_and_exn_handler can accept
       * a list of targets with different languages! We just
       * need to pass the right target object (and not a lang_job)
       * TODO: Martin said the issue was that Run_semgrep.targets_of_config
       * requires the xlang object to contain a single language.
       *)
      let lang_jobs = split_jobs_by_language all_rules all_targets in
      Logs.app (fun m ->
          m "%a"
            (pp_status all_rules all_targets lang_jobs respect_git_ignore)
            ());
      (* TODO progress bar *)
      let results_by_language =
        lang_jobs
        |> Common.map (fun lang_job ->
               let _exn_optTODO, report, files =
                 (* !!!!Finally! this is where we branch to semgrep-core!!! *)
                 Run_semgrep.semgrep_with_prepared_rules_and_targets config
                   lang_job
               in
               (report, Set_.of_list files))
      in
      let res, scanned = merge_results results_by_language in
      (* TODO: should get this from Run_semgrep *)
      let _exnTODO = None in
      (* similar to Run_semgrep.semgrep_with_rules_and_formatted_output *)
      (* LATER: we want to avoid this intermediate data structure but
       * for now that's what semgrep-python used to get so simpler to
       * return it.
       *)
      let match_results =
        JSON_report.match_results_of_matches_and_errors
          (Some Autofix.render_fix) (Set_.cardinal scanned) res
      in
      let errors_skipped = errors_to_skipped match_results.errors in
      let semgrepignored, included, excluded, size =
        analyze_skipped ignored_targets
      in
      let match_results =
        let skipped_targets =
          ignored_targets @ errors_skipped @ match_results.skipped_targets
        in
        (* Add the targets that were semgrepignored or errorneous *)
        { match_results with skipped_targets }
      in

      Logs.info (fun m ->
          m "%a" pp_skipped
            ( respect_git_ignore,
              semgrepignored,
              included,
              excluded,
              size,
              errors_skipped ));
      Logs.app (fun m ->
          m "%a" pp_summary
            ( respect_git_ignore,
              semgrepignored,
              included,
              excluded,
              size,
              errors_skipped ));
      Logs.app (fun m ->
          m "Ran %d rules on %d files: %d findings@." (List.length all_rules)
            (List.length all_targets)
            (List.length match_results.matches));

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

      (* TOADAPT:
          match exn with
          | Some e -> Runner_exit.exit_semgrep (Unknown_exception e)
          | None -> ())
      *)
      { core = match_results; hrules = Rule.hrules_of_rules all_rules; scanned }
