(* Yoann Padioleau, Martin Jambon
 *
 * Copyright (C) 2023 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-scan command, execute it and exit.

   Translated mainly from scan.py, with parts translated also
   from semgrep_main.py and core_runner.py.
*)

module Env = Semgrep_envvars
module Out = Semgrep_output_v1_t
module RP = Report
module SS = Set.Make (String)

(*****************************************************************************)
(* To run the pro engine, including multistep rules *)
(*****************************************************************************)

type diff_config = { diff_targets : Fpath.t list; diff_depth : int option }

let default_diff_config = { diff_targets = []; diff_depth = None }

let (invoke_semgrep_core_proprietary :
      (Fpath.t list ->
      ?diff_config:diff_config ->
      Engine_type.t ->
      Core_runner.semgrep_core_runner)
      option
      ref) =
  ref None

(*****************************************************************************)
(* Logging/Profiling/Debugging *)
(*****************************************************************************)

let setup_logging (conf : Scan_CLI.conf) =
  CLI_common.setup_logging ~force_color:conf.force_color
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
let exit_code_of_errors ~strict (errors : Out.core_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok
  (* TODO? why do we look at the last error? What about the other errors? *)
  | x :: _ -> (
      (* alt: raise a Semgrep_error that would be catched by CLI_Common
       * wrapper instead of returning an exit code directly? *)
      match () with
      | _ when x.severity =*= Out.Error ->
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
    (_file : Fpath.t) (match_results : RP.partial_profiling RP.match_result) :
    unit =
  let (cli_matches : Out.cli_match list) =
    (* need to go through a series of transformation so that we can
     * get something that Matches_report.pp_text_outputs can operate on
     *)
    let (pms : Pattern_match.t list) = match_results.matches in
    let (core_matches : Out.core_match list) =
      pms
      |> Common.partition_either (Core_json_output.match_to_match None)
      |> fst
    in
    let hrules = Rule.hrules_of_rules rules in
    core_matches
    |> Common.map (Cli_json_output.cli_match_of_core_match hrules)
    |> Cli_json_output.dedup_and_sort
  in
  (* TODO? needed? given the call to dedup_and_sort above? I just imitate
   * what we do in the regular code path
   *)
  let cli_matches = Matches_report.sort_matches cli_matches in
  (* TODO? not sure why the order in sort_matches is wrong *)
  let cli_matches = List.rev cli_matches in
  let cli_matches =
    cli_matches
    |> Common.exclude (fun m ->
           let to_ignore, _errs = Nosemgrep.rule_match_nosem ~strict:false m in
           to_ignore)
  in
  if cli_matches <> [] then (
    Unix.lockf Unix.stdout Unix.F_LOCK 0;
    (* coupling: similar to Output.dispatch_output_format for Text *)
    Matches_report.pp_text_outputs ~max_chars_per_line:conf.max_chars_per_line
      ~max_lines_per_finding:conf.max_lines_per_finding
      ~color_output:conf.force_color Format.std_formatter cli_matches;
    (* TODO? use finalize? *)
    Unix.lockf Unix.stdout Unix.F_ULOCK 0)

(*****************************************************************************)
(* Skipped analysis *)
(*****************************************************************************)

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
  let groups =
    Common.group_by
      (fun (Out.{ reason; _ } : Out.skipped_target) ->
        match reason with
        | Out.Gitignore_patterns_match
        | Semgrepignore_patterns_match ->
            `Semgrepignore
        | Too_big
        | Exceeded_size_limit ->
            `Size
        | Cli_include_flags_do_not_match -> `Include
        | Cli_exclude_flags_match -> `Exclude
        | Always_skipped
        | Analysis_failed_parser_or_internal_error
        | Excluded_by_config
        | Wrong_language
        | Minified
        | Binary
        | Irrelevant_rule
        | Too_many_matches ->
            `Other)
      skipped
  in
  ( (try List.assoc `Semgrepignore groups with
    | Not_found -> []),
    (try List.assoc `Include groups with
    | Not_found -> []),
    (try List.assoc `Exclude groups with
    | Not_found -> []),
    (try List.assoc `Size groups with
    | Not_found -> []),
    try List.assoc `Other groups with
    | Not_found -> [] )

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(* This function counts how many matches we got by rules:
   [(Rule.t, number of matches : int) list]. *)
let rules_and_counted_matches (res : Core_runner.result) : (Rule.t * int) list =
  let update = function
    | Some n -> Some (succ n)
    | None -> Some 1
  in
  let fold acc (core_match : Out.core_match) =
    Map_.update core_match.check_id update acc
  in
  let map = List.fold_left fold Map_.empty res.core.Out.results in
  Map_.fold
    (fun rule_id n acc ->
      match Rule_ID.of_string_opt rule_id with
      | Some rule_id -> (Hashtbl.find res.hrules rule_id, n) :: acc
      | None -> acc)
    map []

(* Select and execute the Semgrep core engine based on the configured
   engine settings. *)
let core (conf : Scan_CLI.conf) file_match_results_hook errors targets
    ?(diff_config = default_diff_config) rules () =
  let invoke_semgrep_core =
    match conf.engine_type with
    | OSS ->
        Core_runner.invoke_semgrep_core
          ~engine:Run_semgrep.semgrep_with_raw_results_and_exn_handler
    | PRO _ -> (
        match !invoke_semgrep_core_proprietary with
        | None ->
            (* TODO: improve this error message depending on what the instructions should be *)
            failwith
              "You have requested running semgrep with a setting that requires \
               the pro engine, but do not have the pro engine. You may need to \
               acquire a different binary."
        | Some invoke_semgrep_core_proprietary ->
            let roots = conf.target_roots in
            invoke_semgrep_core_proprietary roots ~diff_config conf.engine_type)
  in
  invoke_semgrep_core ~respect_git_ignore:conf.targeting_conf.respect_git_ignore
    ~file_match_results_hook conf.core_runner_conf rules errors targets

(*****************************************************************************)
(* Differential scanning *)
(*****************************************************************************)

(* This function removes duplicated matches from the results of the
   head commit scan if they are also present in the results of the
   baseline commit scan. Matches are considered identical if the
   tuples containing the rule ID, file path, and matched code snippet
   are equal. *)
let remove_matches_in_baseline (commit : string) (baseline : RP.final_result)
    (head : RP.final_result) (renamed : (filename * filename) stack) =
  let extract_sig renamed m =
    let rule_id = m.Pattern_match.rule_id in
    let path =
      m.Pattern_match.file |> fun p ->
      renamed
      >>= Common.find_some_opt (fun (before, after) ->
              if after = p then Some before else None)
      |> Option.value ~default:p
    in
    let start_range, end_range = m.Pattern_match.range_loc in
    let syntactic_ctx =
      File.lines_of_file
        (start_range.pos.line, end_range.pos.line)
        (Fpath.v m.Pattern_match.file)
    in
    (rule_id, path, syntactic_ctx)
  in
  let sigs = Hashtbl.create 10 in
  Git_wrapper.run_with_worktree ~commit (fun () ->
      List.iter
        (fun m -> m |> extract_sig None |> fun x -> Hashtbl.add sigs x true)
        baseline.RP.matches);
  let removed = ref 0 in
  let matches =
    Common.map_filter
      (fun m ->
        let s = extract_sig (Some renamed) m in
        if Hashtbl.mem sigs s then (
          Hashtbl.remove sigs s;
          removed := !removed + 1;
          None)
        else Some m)
      (head.RP.matches
       (* Sort the matches in ascending order according to their byte positions.
          This ensures that duplicated matches are not removed arbitrarily;
          rather, priority is given to removing matches positioned closer to the
          beginning of the file. *)
      |> List.sort (fun x y ->
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
        (String_utils.unit_str !removed "finding"));
  { head with matches }

(* Execute the engine again on the baseline checkout, utilizing only
   the files and rules linked with matches from the head checkout
   scan. Subsequently, eliminate any previously identified matches
   from the results of the head checkout scan. *)
let scan_baseline_and_remove_duplicates (conf : Scan_CLI.conf)
    (profiler : Profiler.t)
    ( (exn : Exception.t option),
      (r : RP.final_result),
      (scanned : Fpath.t Set_.t) ) (rules : Rule.rules) (commit : string)
    (status : Git_wrapper.status)
    (core :
      Fpath.t list ->
      ?diff_config:diff_config ->
      Rule.rules ->
      unit ->
      Exception.t option * RP.final_result * Fpath.t Set_.t) =
  if r.matches <> [] then
    let add_renamed paths =
      List.fold_left (fun x (y, _) -> SS.add y x) paths status.renamed
    in
    let remove_added paths =
      List.fold_left (Fun.flip SS.remove) paths status.added
    in
    let rules_in_match =
      r.matches
      |> Common.map (fun m -> m.Pattern_match.rule_id.id |> Rule_ID.to_string)
      |> SS.of_list
    in
    (* only use the rules that have been identified within the existing
       matches. *)
    let baseline_rules =
      rules
      |> List.filter (fun x ->
             SS.mem (x.Rule.id |> fst |> Rule_ID.to_string) rules_in_match)
    in
    let _, baseline_r, _ =
      Profiler.record profiler ~name:"baseline_core_time" (fun () ->
          Git_wrapper.run_with_worktree ~commit (fun () ->
              let paths_in_match =
                r.matches
                |> Common.map (fun m -> m.Pattern_match.file)
                |> SS.of_list |> add_renamed |> remove_added |> SS.to_seq
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
              let baseline_targets, baseline_diff_targets =
                match conf.engine_type with
                | PRO Interfile ->
                    let all_in_baseline, _ =
                      Find_targets.get_targets conf.targeting_conf
                        conf.target_roots
                    in
                    (* Performing a scan on the same set of files for the
                       baseline that were previously scanned for the head.
                       In Interfile mode, the matches are influenced not
                       only by the file displaying matches but also by its
                       dependencies. Hence, merely rescanning files with
                       matches is insufficient. *)
                    (all_in_baseline, Set_.elements scanned)
                | _ -> (paths_in_match, [])
              in
              core baseline_targets
                ~diff_config:
                  { diff_targets = baseline_diff_targets; diff_depth = Some 0 }
                baseline_rules ()))
    in
    (exn, remove_matches_in_baseline commit baseline_r r status.renamed, scanned)
  else (exn, r, scanned)

(*****************************************************************************)
(* Conduct the scan *)
(*****************************************************************************)
let run_scan_files (conf : Scan_CLI.conf) (profiler : Profiler.t)
    (rules_and_origins : Rule_fetching.rules_and_origin list)
    (targets_and_ignored : Fpath.t list * Out.skipped_target list) :
    (Rule.rule list * Core_runner.result * Out.cli_output, Exit_code.t) result =
  let rules, errors =
    Rule_fetching.partition_rules_and_errors rules_and_origins
  in
  (* TODO: we should probably warn the user about rules using the same id *)
  let rules =
    Common.uniq_by
      (fun r1 r2 -> Rule_ID.equal (fst r1.Rule.id) (fst r2.Rule.id))
      rules
  in
  if Common.null rules then Error Exit_code.missing_config
  else
    (* step 1: last touch on rules *)
    let filtered_rules =
      Rule_filtering.filter_rules conf.rule_filtering_conf rules
    in
    Logs.info (fun m ->
        m "%a" Rules_report.pp_rules (conf.rules_source, filtered_rules));

    (* step 2: printing the ignored targets *)
    let targets, semgrepignored_targets = targets_and_ignored in
    Logs.debug (fun m ->
        m "%a" Targets_report.pp_targets_debug
          (conf.target_roots, semgrepignored_targets, targets));
    Logs.info (fun m ->
        semgrepignored_targets
        |> List.iter (fun (x : Semgrep_output_v1_t.skipped_target) ->
               m "Ignoring %s due to %s (%s)" x.Semgrep_output_v1_t.path
                 (Semgrep_output_v1_t.show_skip_reason
                    x.Semgrep_output_v1_t.reason)
                 x.Semgrep_output_v1_t.details));

    (* step 3: choose the right engine and right hooks *)
    let output_format, file_match_results_hook =
      match conf with
      | {
       output_format = Output_format.Text;
       common = { maturity = Maturity.Develop; _ };
       _;
      } ->
          ( Output_format.TextIncremental,
            Some (file_match_results_hook conf filtered_rules) )
      | { output_format; _ } -> (output_format, None)
    in
    let core = core conf file_match_results_hook errors in
    let exn_and_matches =
      match conf.targeting_conf.baseline_commit with
      | None ->
          Profiler.record profiler ~name:"core_time"
            (core targets filtered_rules)
      | Some baseline_commit ->
          (* diff scan mode *)
          let commit = Git_wrapper.get_merge_base baseline_commit in
          let status = Git_wrapper.status ~cwd:(Fpath.v ".") ~commit in
          let targets, diff_targets =
            let added_or_modified =
              status.added @ status.modified |> Common.map Fpath.v
            in
            match conf.engine_type with
            | PRO Interfile -> (targets, added_or_modified)
            | _ -> (added_or_modified, [])
          in
          let head_scan_result =
            Profiler.record profiler ~name:"head_core_time"
              (core targets
                 ~diff_config:{ diff_targets; diff_depth = None }
                 filtered_rules)
          in
          scan_baseline_and_remove_duplicates conf profiler head_scan_result
            filtered_rules commit status core
    in
    (* step 3': call the engine! *)
    let (res : Core_runner.result) =
      Core_runner.create_core_result filtered_rules exn_and_matches
    in

    Metrics_.add_engine_type
      ~name:
        (Format.asprintf "%a" Out.pp_engine_kind
           res.Core_runner.core.engine_requested);

    let filtered_matches = rules_and_counted_matches res in
    Metrics_.add_findings filtered_matches;
    Metrics_.add_errors res.core.errors;

    (* step 4: report matches *)
    let errors_skipped = errors_to_skipped res.core.errors in
    let semgrepignored, included, excluded, size, other_ignored =
      analyze_skipped semgrepignored_targets
    in
    let res =
      let skipped_targets =
        Some
          (semgrepignored_targets @ errors_skipped
          @ Common.optlist_to_list res.core.skipped_targets)
      in
      (* Add the targets that were semgrepignored or errorneous *)
      let core = { res.core with skipped_targets } in
      { res with core }
    in

    (* outputting the result on stdout! in JSON/Text/... depending on conf *)
    let cli_output =
      Output.output_result { conf with output_format } profiler res
    in
    Profiler.stop_ign profiler ~name:"total_time";
    if Metrics_.is_enabled conf.metrics then (
      Metrics_.add_rules ?profiling:res.core.time filtered_rules;
      Metrics_.add_profiling profiler);

    Logs.info (fun m ->
        m "%a" Skipped_report.pp_skipped
          ( conf.targeting_conf.respect_git_ignore,
            conf.common.maturity,
            conf.targeting_conf.max_target_bytes,
            semgrepignored,
            included,
            excluded,
            size,
            other_ignored,
            errors_skipped ));
    (* Note that Logs.app() is printing on stderr (but without any [XXX]
     * prefix), and is filtered when using --quiet.
     *)
    Logs.app (fun m ->
        m "%a" Summary_report.pp_summary
          ( conf.targeting_conf.respect_git_ignore,
            conf.common.maturity,
            conf.targeting_conf.max_target_bytes,
            semgrepignored,
            included,
            excluded,
            size,
            other_ignored,
            errors_skipped ));
    Logs.app (fun m ->
        m "Ran %s on %s: %s."
          (String_utils.unit_str (List.length filtered_rules) "rule")
          (String_utils.unit_str (List.length cli_output.paths.scanned) "file")
          (String_utils.unit_str (List.length cli_output.results) "finding"));

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

let run_scan_conf (conf : Scan_CLI.conf) : Exit_code.t =
  let profiler = Profiler.make () in
  Profiler.start profiler ~name:"total_time";
  let settings =
    (fun () ->
      Metrics_.configure conf.metrics;
      let settings = Semgrep_settings.load ~maturity:conf.common.maturity () in
      if Metrics_.is_enabled conf.metrics then
        Metrics_.add_project_url (Git_wrapper.get_project_url ());
      Metrics_.add_integration_name Env.v.integration_name;
      (match conf.rules_source with
      | Rules_source.Configs configs -> Metrics_.add_configs configs
      | _ -> ());
      settings)
    |> Profiler.record profiler ~name:"config_time"
  in

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

  (* Rule_fetching.rules_and_origin record also contain errors *)
  let rules_and_origins =
    Rule_fetching.rules_from_rules_source ~token_opt:settings.api_token
      ~rewrite_rule_ids:conf.rewrite_rule_ids
      ~registry_caching:conf.registry_caching conf.rules_source
  in
  Metrics_.add_token settings.api_token;

  (* step2: getting the targets *)
  let targets_and_ignored =
    Find_targets.get_targets conf.targeting_conf conf.target_roots
  in
  (* step3: let's go *)
  let res =
    run_scan_files conf profiler rules_and_origins targets_and_ignored
  in
  match res with
  | Error ex -> ex
  | Ok (_, res, cli_output) ->
      (* step4: exit with the right exit code *)
      (* final result for the shell *)
      if conf.error_on_findings && not (Common.null cli_output.results) then
        Exit_code.findings
      else exit_code_of_errors ~strict:conf.strict res.core.errors

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (conf : Scan_CLI.conf) : Exit_code.t =
  (match conf.common.maturity with
  | Maturity.Default -> (
      match conf with
      (* TODO: handle more confs, or fallback to pysemgrep further down *)
      | { show_supported_languages = true; _ } -> ()
      | _else_ -> raise Pysemgrep.Fallback)
  (* this should never happen because --legacy is handled in cli/bin/semgrep *)
  | Maturity.Legacy -> raise Pysemgrep.Fallback
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
      Common.pr Version.version;
      (* TOPORT: if enable_version_check: version_check() *)
      Exit_code.ok
  | _ when conf.show_supported_languages ->
      Common.pr (spf "supported languages are: %s" Xlang.supported_xlangs);
      Exit_code.ok
  | _ when conf.test <> None -> Test_subcommand.run (Common2.some conf.test)
  | _ when conf.validate <> None ->
      Validate_subcommand.run (Common2.some conf.validate)
  | _ when conf.show <> None -> Show_subcommand.run (Common2.some conf.show)
  | _else_ ->
      (* --------------------------------------------------------- *)
      (* Let's go *)
      (* --------------------------------------------------------- *)
      run_scan_conf conf

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Scan_CLI.parse_argv argv in
  run_conf conf
