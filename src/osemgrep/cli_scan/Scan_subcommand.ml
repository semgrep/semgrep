(* Yoann Padioleau, Martin Jambon
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module C = Rules_config
module Env = Semgrep_envvars
module OutJ = Semgrep_output_v1_t
module SS = Set.Make (String)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated mainly from scan.py, with parts translated also
   from semgrep_main.py and core_runner.py.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* TODO: probably far more needed at some point *)
type caps = < Cap.stdout ; Cap.network >

(*****************************************************************************)
(* Logging/Profiling/Debugging *)
(*****************************************************************************)

let setup_logging (conf : Scan_CLI.conf) =
  CLI_common.setup_logging ~force_color:conf.output_conf.force_color
    ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "Semgrep version: %s" Version.version);
  ()

(* ugly: also partially done in CLI.ml *)
let setup_profiling (conf : Scan_CLI.conf) =
  (* TOADAPT
      if config.debug then Report.mode := MDebug
      else if config.report_time then Report.mode := MTime
      else Report.mode := MNo_info;
  *)
  if conf.common.profile then (
    (* ugly: no need to set Common.profile, this was done in CLI.ml *)
    Logs.debug (fun m -> m "Profile mode On");
    Logs.debug (fun m -> m "disabling -j when in profiling mode");
    { conf with core_runner_conf = { conf.core_runner_conf with num_jobs = 1 } })
  else conf

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* python: this used to be done in a _final_raise method from output.py
 * but better separation of concern to do it here.
 *)
let exit_code_of_errors ~strict (errors : OutJ.core_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  (* TODO? why do we look at the last error? What about the other errors? *)
  | x :: _ -> (
      (* alt: raise a Semgrep_error that would be catched by CLI_Common
       * wrapper instead of returning an exit code directly? *)
      match () with
      | _ when x.severity =*= `Error ->
          Cli_json_output.exit_code_of_error_type x.error_type
      | _ when strict -> Cli_json_output.exit_code_of_error_type x.error_type
      | _else_ -> Exit_code.ok)

(*****************************************************************************)
(* Incremental display *)
(*****************************************************************************)

(* Note that this hook is run in parallel in Parmap at the end of processing
 * a file. Using Format.std_formatter in parallel requires some synchronization
 * to avoid having the output of multiple child processes interwinded, hence
 * the use of Unix.lockf below.
 *)
let file_match_results_hook (conf : Scan_CLI.conf) (rules : Rule.rules)
    (_file : Fpath.t) (match_results : Core_result.matches_single_file) : unit =
  let (cli_matches : OutJ.cli_match list) =
    (* need to go through a series of transformation so that we can
     * get something that Matches_report.pp_text_outputs can operate on
     *)
    let (pms : Pattern_match.t list) = match_results.matches in
    let (core_matches : OutJ.core_match list) =
      pms
      (* OK, because we don't need the postprocessing to report the matches. *)
      |> List_.map Core_result.mk_processed_match
      |> Either_.partition_either Core_json_output.match_to_match
      |> fst
    in
    let hrules = Rule.hrules_of_rules rules in
    core_matches
    |> List_.map
         (Cli_json_output.cli_match_of_core_match
            ~dryrun:conf.output_conf.dryrun hrules)
    |> Cli_json_output.dedup_and_sort
  in
  let cli_matches =
    cli_matches
    |> List_.exclude (fun (m : OutJ.cli_match) ->
           Option.value ~default:false m.extra.is_ignored)
  in
  if cli_matches <> [] then (
    Unix.lockf Unix.stdout Unix.F_LOCK 0;
    Common.protect
      (fun () ->
        (* coupling: similar to Output.dispatch_output_format for Text *)
        Matches_report.pp_text_outputs
          ~max_chars_per_line:conf.output_conf.max_chars_per_line
          ~max_lines_per_finding:conf.output_conf.max_lines_per_finding
          ~color_output:conf.output_conf.force_color Format.std_formatter
          cli_matches)
      ~finally:(fun () -> Unix.lockf Unix.stdout Unix.F_ULOCK 0))

(*****************************************************************************)
(* Pretty Printing for CLI UX *)
(*****************************************************************************)

(* TODO: Update pysemgrep and osemgrep tests to match new output *)
let new_cli_ux =
  match !Env.v.user_agent_append with
  | Some x -> (
      match String.lowercase_ascii x with
      | "pytest" -> false
      | _ -> true)
  | _ -> true

let print_logo () : unit =
  let logo =
    Ocolor_format.asprintf
      {|
┌──── @{<green>○○○@} ────┐
│ Semgrep CLI │
└─────────────┘
|}
  in
  Logs.app (fun m -> m "%s" logo);
  ()

let feature_status_str ~(enabled : bool) : string =
  let status =
    if enabled then Ocolor_format.asprintf {|@{<green>✔@}|}
    else Ocolor_format.asprintf {|@{<red>✘@}|}
  in
  status

let print_feature_section ~(includes_token : bool) ~(engine : Engine_type.t) :
    unit =
  let secrets_enabled =
    match engine with
    | PRO
        Engine_type.
          { secrets_config = Some Engine_type.{ allow_all_origins = _; _ }; _ }
      ->
        true
    | OSS
    | PRO Engine_type.{ secrets_config = None; _ } ->
        false
  in
  let features =
    [
      ( "Semgrep OSS",
        "Basic security coverage for first-party code vulnerabilities.",
        true );
      ( "Semgrep Code (SAST)",
        "Find and fix vulnerabilities in the code you write with advanced \
         scanning and expert security rules.",
        includes_token );
      ( "Semgrep Secrets",
        "Detect and validate potential secrets in your code.",
        secrets_enabled );
    ]
  in
  (* Print our set of features and whether each is enabled *)
  List.iter
    (fun (feature_name, desc, is_enabled) ->
      Logs.app (fun m ->
          m "%s %s"
            (feature_status_str ~enabled:is_enabled)
            (Ocolor_format.asprintf {|@{<bold>%s@}|} feature_name));
      Logs.app (fun m ->
          m "  %s %s\n" (feature_status_str ~enabled:is_enabled) desc))
    features;
  ()

let display_rule_source ~(rule_source : Rules_source.t) : unit =
  let msg =
    match rule_source with
    | Configs xs
      when List.exists
             (function
               | C.A _
               | R _ ->
                   true
               | _ -> false)
             (List_.map
                (fun str ->
                  Rules_config.parse_config_string ~in_docker:false str)
                xs) ->
        Ocolor_format.asprintf {|@{<bold>  %s@}|}
          "Loading rules from registry..."
    | Configs _ ->
        Ocolor_format.asprintf {|@{<bold>  %s@}|}
          "Loading rules from local config..."
    | Pattern _ -> Ocolor_format.asprintf {|@{  %s@}|} "Using custom pattern."
  in
  Logs.app (fun m -> m "%s" msg);
  ()

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(* This function counts how many matches we got by rules:
   [(Rule.t, number of matches : int) list].
   This is use for rule metrics.
*)
let rules_and_counted_matches (res : Core_runner.result) : (Rule.t * int) list =
  let update = function
    | Some n -> Some (succ n)
    | None -> Some 1
  in
  let fold acc (core_match : OutJ.core_match) =
    Map_.update core_match.check_id update acc
  in
  let xmap = List.fold_left fold Map_.empty res.core.results in
  Map_.fold
    (fun rule_id n acc ->
      let res =
        try Hashtbl.find res.hrules rule_id with
        | Not_found ->
            failwith
              (spf "could not find rule_id %s in hash"
                 (Rule_ID.to_string rule_id))
      in
      (res, n) :: acc)
    xmap []

(* Select and execute the scan func based on the configured engine settings.
 * Yet another mk_scan_func adapter. TODO: can we simplify?
 *)
let mk_scan_func (conf : Scan_CLI.conf) file_match_results_hook errors targets
    ?(diff_config = Differential_scan_config.WholeScan) rules () =
  let scan_func_for_osemgrep : Core_runner.scan_func_for_osemgrep =
    match conf.engine_type with
    | OSS ->
        Core_runner.mk_scan_func_for_osemgrep Core_scan.scan_with_exn_handler
    | PRO _ -> (
        match !Core_runner.hook_pro_scan_func_for_osemgrep with
        | None ->
            (* TODO: improve this error message depending on what the
             * instructions should be *)
            failwith
              "You have requested running semgrep with a setting that requires \
               the pro engine, but do not have the pro engine. You may need to \
               acquire a different binary."
        | Some pro_scan_func ->
            let roots = conf.target_roots in
            pro_scan_func roots ~diff_config conf.engine_type)
  in
  let scan_func_for_osemgrep : Core_runner.scan_func_for_osemgrep =
    match conf.targeting_conf.project_root with
    | Some (Find_targets.Git_remote git_remote) -> (
        match !Core_runner.hook_pro_git_remote_scan_setup with
        | None ->
            failwith
              "You have requested running semgrep with a setting that requires \
               the pro engine, but do not have the pro engine. You may need to \
               acquire a different binary."
        | Some pro_git_remote_scan_setup ->
            pro_git_remote_scan_setup git_remote scan_func_for_osemgrep)
    | _ -> scan_func_for_osemgrep
  in
  scan_func_for_osemgrep
    ~respect_git_ignore:conf.targeting_conf.respect_gitignore
    ~file_match_results_hook conf.core_runner_conf rules errors targets

let rules_from_rules_source ~token_opt ~rewrite_rule_ids ~registry_caching
    ~strict caps rules_source =
  (* Create the wait hook for our progress indicator *)
  let spinner_ls =
    if Console_Spinner.should_show_spinner () then
      [ Console_Spinner.spinner_async () ]
    else []
  in
  (* Fetch the rules *)
  let rules_and_origins =
    Rule_fetching.rules_from_rules_source_async ~token_opt ~rewrite_rule_ids
      ~registry_caching ~strict
      (caps :> < Cap.network >)
      rules_source
  in
  Lwt_platform.run (Lwt.pick (rules_and_origins :: spinner_ls))
[@@profiling]

(* The test test_autofix.py::terraform-ec2-instance-metadata-options.yaml
   carries a newline at the end of the "fix" string, which is not the case
   for PySemgrep.
   TODO Trimming the "fix" here is a hacky workaround, it may be better to dig
   down where and why the newline is inserted into "fix".
*)
let trim_core_match_fix (r : OutJ.core_match) =
  let fix = Option.map String.trim r.OutJ.extra.fix in
  let extra = { r.extra with fix } in
  { r with extra }

(*****************************************************************************)
(* Differential scanning *)
(*****************************************************************************)

(* This function removes duplicated matches from the results of the
   head commit scan if they are also present in the results of the
   baseline commit scan. Matches are considered identical if the
   tuples containing the rule ID, file path, and matched code snippet
   are equal. *)
let remove_matches_in_baseline (commit : string) (baseline : Core_result.t)
    (head : Core_result.t)
    (renamed : (string (* filename *) * string (* filename *)) list) =
  let extract_sig renamed (m : Pattern_match.t) =
    let rule_id = m.rule_id in
    let path =
      !!(m.path.internal_path_to_content) |> fun p ->
      Option.bind renamed
        (List_.find_some_opt (fun (before, after) ->
             if after = p then Some before else None))
      |> Option.value ~default:p
    in
    let start_range, end_range = m.range_loc in
    let syntactic_ctx =
      UFile.lines_of_file
        (start_range.pos.line, end_range.pos.line)
        m.path.internal_path_to_content
    in
    (rule_id, path, syntactic_ctx)
  in
  let sigs = Hashtbl.create 10 in
  Git_wrapper.run_with_worktree ~commit (fun () ->
      List.iter
        (fun ({ pm; _ } : Core_result.processed_match) ->
          pm |> extract_sig None |> fun x -> Hashtbl.add sigs x true)
        baseline.processed_matches);
  let removed = ref 0 in
  let processed_matches =
    List_.map_filter
      (fun (pm : Core_result.processed_match) ->
        let s = extract_sig (Some renamed) pm.pm in
        if Hashtbl.mem sigs s then (
          Hashtbl.remove sigs s;
          incr removed;
          None)
        else Some pm)
      (head.processed_matches
       (* Sort the matches in ascending order according to their byte positions.
          This ensures that duplicated matches are not removed arbitrarily;
          rather, priority is given to removing matches positioned closer to the
          beginning of the file. *)
      |> List.sort
           (fun ({ pm = x; _ } : Core_result.processed_match) { pm = y; _ } ->
             let x_start_range, x_end_range = x.Pattern_match.range_loc in
             let y_start_range, y_end_range = y.Pattern_match.range_loc in
             let start_compare =
               x_start_range.pos.bytepos - y_start_range.pos.bytepos
             in
             if start_compare <> 0 then start_compare
             else x_end_range.pos.bytepos - y_end_range.pos.bytepos))
  in
  Logs.app (fun m ->
      m "Removed %s that were in baseline scan"
        (String_.unit_str !removed "finding"));
  { head with processed_matches }

(* Execute the engine again on the baseline checkout, utilizing only
   the files and rules linked with matches from the head checkout
   scan. Subsequently, eliminate any previously identified matches
   from the results of the head checkout scan. *)
let scan_baseline_and_remove_duplicates (conf : Scan_CLI.conf)
    (profiler : Profiler.t) (result_or_exn : Core_result.result_or_exn)
    (rules : Rule.rules) (commit : string) (status : Git_wrapper.status)
    (core :
      Fpath.t list ->
      ?diff_config:Differential_scan_config.t ->
      Rule.rules ->
      unit ->
      Core_result.result_or_exn) : Core_result.result_or_exn =
  match result_or_exn with
  | Error _ as err -> err
  | Ok r ->
      if r.processed_matches <> [] then
        let add_renamed paths =
          List.fold_left (fun x (y, _) -> SS.add y x) paths status.renamed
        in
        let remove_added paths =
          List.fold_left (Fun.flip SS.remove) paths status.added
        in
        let rules_in_match =
          r.processed_matches
          |> List_.map (fun ({ pm; _ } : Core_result.processed_match) ->
                 pm.Pattern_match.rule_id.id |> Rule_ID.to_string)
          |> SS.of_list
        in
        (* only use the rules that have been identified within the existing
           matches. *)
        let baseline_rules =
          rules
          |> List.filter (fun x ->
                 SS.mem (x.Rule.id |> fst |> Rule_ID.to_string) rules_in_match)
        in
        let baseline_result =
          Profiler.record profiler ~name:"baseline_core_time" (fun () ->
              Git_wrapper.run_with_worktree ~commit (fun () ->
                  let prepare_targets paths =
                    paths |> SS.of_list |> add_renamed |> remove_added
                    |> SS.to_seq
                    |> Seq.filter_map (fun x ->
                           if
                             Sys.file_exists x
                             &&
                             match (Unix.lstat x).st_kind with
                             | S_LNK -> false
                             | _ -> true
                           then Some (Fpath.v x)
                           else None)
                    |> List.of_seq
                  in
                  let paths_in_match =
                    r.processed_matches
                    |> List_.map
                         (fun ({ pm; _ } : Core_result.processed_match) ->
                           !!(pm.path.internal_path_to_content))
                    |> prepare_targets
                  in
                  let paths_in_scanned =
                    r.scanned |> List_.map Fpath.to_string |> prepare_targets
                  in
                  let baseline_targets, baseline_diff_targets =
                    match conf.engine_type with
                    | PRO Engine_type.{ analysis = Interprocedural; _ } ->
                        let all_in_baseline, _ =
                          Find_targets.get_target_fpaths conf.targeting_conf
                            conf.target_roots
                        in
                        (* Performing a scan on the same set of files for the
                           baseline that were previously scanned for the head.
                           In Interfile mode, the matches are influenced not
                           only by the file displaying matches but also by its
                           dependencies. Hence, merely rescanning files with
                           matches is insufficient. *)
                        (all_in_baseline, paths_in_scanned)
                    | _ -> (paths_in_match, [])
                  in
                  core baseline_targets
                    ~diff_config:
                      (Differential_scan_config.BaseLine baseline_diff_targets)
                    baseline_rules ()))
        in
        match baseline_result with
        | Error _exn -> baseline_result
        | Ok baseline_r ->
            Ok (remove_matches_in_baseline commit baseline_r r status.renamed)
      else Ok r

(*****************************************************************************)
(* Conduct the scan *)
(*****************************************************************************)
let run_scan_files (_caps : < Cap.stdout >) (conf : Scan_CLI.conf)
    (profiler : Profiler.t)
    (rules_and_origins : Rule_fetching.rules_and_origin list)
    (targets_and_skipped : Fpath.t list * OutJ.skipped_target list) :
    (Rule.rule list * Core_runner.result * OutJ.cli_output, Exit_code.t) result
    =
  Metrics_.add_engine_type conf.engine_type;
  let rules, errors =
    Rule_fetching.partition_rules_and_errors rules_and_origins
  in
  (* TODO: we should probably warn the user about rules using the same id *)
  let rules =
    List_.uniq_by
      (fun r1 r2 -> Rule_ID.equal (fst r1.Rule.id) (fst r2.Rule.id))
      rules
  in
  (* desired/legacy semgrep behavior: fail if no valid rule was found

     Problem in case of all Apex rules being skipped by semgrep-core:
     - actual pysemgrep behavior:
       * doesn't count these rules as skipped, resulting in a successful exit
       * reports Apex targets as scanned that weren't scanned
     - osemgrep behavior:
       * reports skipped rules and skipped/scanned targets correctly
     How to fix this:
     - pysemgrep should read the 'scanned' field reporting the targets that
       were really scanned by semgrep-core instead of the current
       implementation that assumes semgrep-core will scan all the targets it
       receives.
     Should we fix this?
     - it's necessary to get the same output with pysemgrep and osemgrep
     - it's a bit of an effort on the Python side for something that's
       not very important
     Suggestion:
     - tolerate different output between pysemgrep and osemgrep
       for tests that we would mark as such.
  *)
  if List_.null rules then Error Exit_code.missing_config
  else
    (* step 1: last touch on rules *)
    let filtered_rules =
      Rule_filtering.filter_rules conf.rule_filtering_conf rules
    in
    Logs.info (fun m ->
        m "%a" Rules_report.pp_rules (conf.rules_source, filtered_rules));

    (* step 2: printing the skipped targets *)
    let targets, skipped = targets_and_skipped in
    Logs.debug (fun m ->
        m "%a" Targets_report.pp_targets_debug
          (conf.target_roots, skipped, targets));
    Logs.info (fun m ->
        skipped
        |> List.iter (fun (x : Semgrep_output_v1_t.skipped_target) ->
               m "Ignoring %s due to %s (%s)"
                 !!(x.Semgrep_output_v1_t.path)
                 (Semgrep_output_v1_t.show_skip_reason
                    x.Semgrep_output_v1_t.reason)
                 (x.Semgrep_output_v1_t.details ||| "")));

    (* step 3: choose the right engine and right hooks *)
    let output_format, file_match_results_hook =
      match conf with
      | {
       output_conf = { output_format = Output_format.Text; _ };
       common = { maturity = Maturity.Develop; _ };
       _;
      } ->
          ( Output_format.TextIncremental,
            Some (file_match_results_hook conf filtered_rules) )
      | { output_conf; _ } -> (output_conf.output_format, None)
    in
    let scan_func = mk_scan_func conf file_match_results_hook errors in
    (* step 3': call the engine! *)
    let exn_and_matches =
      match conf.targeting_conf.baseline_commit with
      | None ->
          Profiler.record profiler ~name:"core_time"
            (scan_func targets filtered_rules)
      | Some baseline_commit ->
          (* diff scan mode *)
          Metrics_.g.payload.environment.isDiffScan <- true;
          let commit = Git_wrapper.get_merge_base baseline_commit in
          let status = Git_wrapper.status ~cwd:(Fpath.v ".") ~commit () in
          let diff_depth = Differential_scan_config.default_depth in
          let targets, diff_targets =
            let added_or_modified =
              status.added @ status.modified |> List_.map Fpath.v
            in
            match conf.engine_type with
            | PRO Engine_type.{ analysis = Interfile; _ } ->
                Metrics_.g.payload.value.proFeatures <-
                  Some { diffDepth = Some diff_depth };
                (targets, added_or_modified)
            | _ -> (added_or_modified, [])
          in
          let head_scan_result =
            Profiler.record profiler ~name:"head_core_time"
              (scan_func targets
                 ~diff_config:
                   (Differential_scan_config.Depth (diff_targets, diff_depth))
                 filtered_rules)
          in
          scan_baseline_and_remove_duplicates conf profiler head_scan_result
            filtered_rules commit status scan_func
    in
    let (res : Core_runner.result) =
      let res = Core_runner.create_core_result filtered_rules exn_and_matches in
      (* step 3'': filter via nosemgrep *)
      let keep_ignored =
        (not conf.core_runner_conf.nosem)
        (* --disable-nosem *)
        || Output_format.keep_ignores output_format
      in
      let filtered_matches =
        res.core.results
        |> List_.map trim_core_match_fix
        |> Nosemgrep.filter_ignored ~keep_ignored
      in
      { res with core = { res.core with results = filtered_matches } }
    in

    (* step 4: adjust the skipped_targets *)
    let errors_skipped = Skipped_report.errors_to_skipped res.core.errors in
    let skipped = skipped @ errors_skipped in
    let (res : Core_runner.result) =
      (* TODO: what is in core.skipped_targets? should we add them to
       * skipped above too?
       *)
      let skipped =
        let skipped = skipped @ List_.optlist_to_list res.core.paths.skipped in
        let in_test =
          !Semgrep_envvars.v.user_agent_append
          |> Option.map (fun s -> String.equal s "pytest")
          |> Option.value ~default:false
        in
        let skipped =
          if in_test then
            List_.map
              (fun (x : OutJ.skipped_target) -> { x with OutJ.details = None })
              skipped
          else skipped
        in
        Some skipped
      in
      (* Add the targets that were semgrepignored or errorneous *)
      {
        res with
        core = { res.core with paths = { res.core.paths with skipped } };
      }
    in

    (* step 5: report the matches *)
    (* outputting the result on stdout! in JSON/Text/... depending on conf *)
    let cli_output =
      let is_logged_in = Semgrep_login.is_logged_in () in
      Output.output_result
        { conf.output_conf with output_format }
        profiler ~is_logged_in res
    in
    Profiler.stop_ign profiler ~name:"total_time";

    let rules_with_targets =
      match exn_and_matches with
      | Ok r ->
          r.rules_with_targets
          |> List_.map (fun (rv : Rule.rule) -> Rule_ID.to_string (fst rv.id))
      | _ -> []
    in

    if Metrics_.is_enabled () then (
      Metrics_.add_errors cli_output.errors;
      Metrics_.add_rules_hashes_and_rules_profiling ?profiling:res.core.time
        filtered_rules;
      Metrics_.add_rules_hashes_and_findings_count
        (rules_and_counted_matches res);
      Metrics_.add_profiling profiler);

    let skipped_groups = Skipped_report.group_skipped skipped in
    Logs.info (fun m ->
        m "%a" Skipped_report.pp_skipped
          ( conf.targeting_conf.respect_gitignore,
            conf.common.maturity,
            conf.targeting_conf.max_target_bytes,
            skipped_groups ));
    (* Note that Logs.app() is printing on stderr (but without any [XXX]
     * prefix), and is filtered when using --quiet.
     *)
    Logs.app (fun m ->
        m "%a"
          (Summary_report.pp_summary
             ~respect_gitignore:conf.targeting_conf.respect_gitignore
             ~maturity:conf.common.maturity
             ~max_target_bytes:conf.targeting_conf.max_target_bytes
             ~skipped_groups)
          ());
    Logs.app (fun m ->
        m "Ran %s on %s: %s."
          (String_.unit_str (List.length rules_with_targets) "rule")
          (String_.unit_str (List.length cli_output.paths.scanned) "file")
          (String_.unit_str (List.length cli_output.results) "finding"));

    (* step 6: apply autofixes *)
    (* this must happen posterior to reporting matches, or will report the
       already-fixed file
    *)
    if conf.output_conf.autofix then
      Autofix.apply_fixes_of_core_matches ~dryrun:conf.output_conf.dryrun
        res.core.results;

    (* TOPORT? was in formater/base.py
       def keep_ignores(self) -> bool:
         """
         Return True if ignored findings should be passed to this formatter;
         False otherwise.
         Ignored findings can still be distinguished using their _is_ignore property.
         """
         return False
    *)
    Ok (filtered_rules, res, cli_output)

let run_scan_conf (caps : caps) (conf : Scan_CLI.conf) : Exit_code.t =
  let profiler = Profiler.make () in
  Profiler.start profiler ~name:"total_time";

  (* Print Semgrep CLI logo ASAP to minimize time to first meaningful content paint *)
  if new_cli_ux then print_logo ();

  (* Metrics initialization (and finalization) is done in CLI.ml,
   * but here we "configure" it (enable or disable it) based on CLI flags.
   *)
  Metrics_.configure conf.metrics;
  let settings =
    (fun () ->
      let settings = Semgrep_settings.load ~maturity:conf.common.maturity () in
      (* TODO? why guard this one with is_enabled? because calling
       * git can take time (and generate errors on stderr)?
       *)
      if Metrics_.is_enabled () then
        Git_wrapper.get_project_url ()
        |> Option.iter Metrics_.add_project_url_hash;
      (match conf.rules_source with
      | Configs configs -> Metrics_.add_configs_hash configs
      | Pattern _ -> ());
      settings)
    |> Profiler.record profiler ~name:"config_time"
  in

  (* Print feature section for enabled products if pattern mode is not being used.
     Ideally, pattern mode should be a different subcommand, but for now we will
     conditionally print the feature section.
  *)
  (if new_cli_ux then
     match conf.rules_source with
     | Pattern _ ->
         Logs.app (fun m ->
             m "%s"
               (Ocolor_format.asprintf {|@{<bold>  %s@}|}
                  "Code scanning at ludicrous speed.\n"))
     | _ ->
         print_feature_section
           ~includes_token:(settings.api_token <> None)
           ~engine:conf.engine_type);

  (* step0: potentially notify user about metrics *)
  if not (settings.has_shown_metrics_notification =*= Some true) then (
    (* python compatibility: the 22m and 24m are "normal color or intensity",
     * and "underline off" *)
    let esc =
      if Fmt.style_renderer Fmt.stderr =*= `Ansi_tty then "\027[22m\027[24m"
      else ""
    in
    Logs.warn (fun m ->
        m
          "%sMETRICS: Using configs from the Registry (like --config=p/ci) \
           reports pseudonymous rule metrics to semgrep.dev.@.To disable \
           Registry rule metrics, use \"--metrics=off\".@.Using configs only \
           from local files (like --config=xyz.yml) does not enable \
           metrics.@.@.More information: https://semgrep.dev/docs/metrics"
          esc);
    Logs.app (fun m -> m "");
    let settings =
      {
        settings with
        Semgrep_settings.has_shown_metrics_notification = Some true;
      }
    in
    ignore (Semgrep_settings.save settings));

  (* step1: getting the rules *)

  (* Display a message to denote rule fetching that is made interactive when
   * possible *)
  if new_cli_ux then display_rule_source ~rule_source:conf.rules_source;

  let rules_and_origins =
    rules_from_rules_source ~token_opt:settings.api_token
      ~rewrite_rule_ids:conf.rewrite_rule_ids
      ~registry_caching:conf.registry_caching
      ~strict:conf.core_runner_conf.strict
      (caps :> < Cap.network >)
      conf.rules_source
  in
  (* step2: getting the targets *)
  let targets_and_skipped =
    Find_targets.get_target_fpaths conf.targeting_conf conf.target_roots
  in
  (* Change dir if project_root is a git_remote
   * note: sorry cooper, we gotta do this because
   * git sparse-checkout doesn't like absolute paths
   *)
  (match conf.targeting_conf.project_root with
  | Some (Find_targets.Git_remote { checkout_path; _ }) ->
      Sys.chdir (Fpath.to_string checkout_path)
  | _ -> ());
  (* step3: let's go *)
  let res =
    run_scan_files
      (caps :> < Cap.stdout >)
      conf profiler rules_and_origins targets_and_skipped
  in
  match res with
  | Error ex -> ex
  | Ok (_, res, cli_output) ->
      (* step4: exit with the right exit code *)
      (* final result for the shell *)
      if conf.error_on_findings && not (List_.null cli_output.results) then
        Exit_code.findings
      else
        exit_code_of_errors ~strict:conf.core_runner_conf.strict res.core.errors

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : caps) (conf : Scan_CLI.conf) : Exit_code.t =
  (* coupling: if you modify the pysemgrep fallback code below, you
   * probably also need to modify it in Ci_subcommand.ml
   *)
  (match conf.common.maturity with
  (* those are osemgrep-only option not available in pysemgrep,
   * so better print a good error message for it.
   * coupling: see the 'NEW' section in Scan_CLI.ml for all those new flags
   *)
  | Maturity.Default
    when conf.registry_caching || conf.core_runner_conf.ast_caching ->
      Error.abort "--registry_caching or --ast_caching require --experimental"
  | Maturity.Default -> (
      (* TODO: handle more confs, or fallback to pysemgrep further down *)
      match conf with
      | {
       show =
         Some
           {
             show_kind = Show_CLI.DumpEnginePath _ | Show_CLI.DumpCommandForCore;
             _;
           };
       _;
      } ->
          raise Pysemgrep.Fallback
      | { show = Some _; _ } -> ()
      | _else_ -> raise Pysemgrep.Fallback)
  (* this should never happen because --legacy is handled in cli/bin/semgrep *)
  | Maturity.Legacy -> raise Pysemgrep.Fallback
  (* ok the user explicitely requested --experimental (or --develop),
   * let's keep going with osemgrep then
   *)
  | Maturity.Experimental
  | Maturity.Develop ->
      ());
  setup_logging conf;
  (* return a new conf because can adjust conf.num_jobs (-j) *)
  let conf = setup_profiling conf in
  Logs.debug (fun m -> m "conf = %s" (Scan_CLI.show_conf conf));

  match () with
  (* "alternate modes" where no search is performed.
   * coupling: if you add a new alternate mode, you probably need to modify
   * Scan_CLI.cmdline_term.combine.rules_source match cases and allow
   * more cases returning an empty 'Configs []'.
   * LATER: this should be real separate subcommands instead of abusing
   * semgrep scan flags. Maybe a 'semgrep show version',
   * 'semgrep show supported_languages', 'semgrep show ast foo.py',
   * 'semgrep test dir/'
   *)
  | _ when conf.version ->
      Out.put Version.version;
      (* TOPORT: if enable_version_check: version_check() *)
      Exit_code.ok
  | _ when conf.test <> None ->
      Test_subcommand.run_conf
        (caps :> < Cap.stdout ; Cap.network >)
        (Common2.some conf.test)
  | _ when conf.validate <> None ->
      Validate_subcommand.run_conf
        (caps :> < Cap.stdout ; Cap.network >)
        (Common2.some conf.validate)
  | _ when conf.show <> None ->
      Show_subcommand.run_conf
        (caps :> < Cap.stdout ; Cap.network >)
        (Common2.some conf.show)
  | _ when conf.ls ->
      Ls_subcommand.run ~target_roots:conf.target_roots
        ~targeting_conf:conf.targeting_conf ()
  | _ ->
      (* --------------------------------------------------------- *)
      (* Let's go *)
      (* --------------------------------------------------------- *)
      run_scan_conf caps conf

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : caps) (argv : string array) : Exit_code.t =
  let conf = Scan_CLI.parse_argv argv in
  run_conf caps conf
