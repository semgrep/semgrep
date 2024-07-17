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
module Out = Semgrep_output_v1_t

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
type caps =
  < Cap.stdout
  ; (* mainly to access the registry *)
    Cap.network
  ; (* TODO: we should get rid of that *)
    Cap.tmp
  ; (* this is for Git_remote for semgrep query console and also for
     * differential scans as we use Git_wrapper.run_with_worktree.
     *)
    Cap.chdir >

(*****************************************************************************)
(* Metrics *)
(*****************************************************************************)

let add_project_and_config_metrics (conf : Scan_CLI.conf) : unit =
  (* TODO? why guard this one with is_enabled? because calling
   * git can take time (and generate errors on stderr)?
   *)
  if Metrics_.is_enabled () then
    Git_wrapper.get_project_url () |> Option.iter Metrics_.add_project_url_hash;
  match conf.rules_source with
  | Configs configs -> Metrics_.add_configs_hash configs
  | Pattern _ -> ()

let notify_user_about_metrics_once (settings : Semgrep_settings.t) : unit =
  if not (settings.has_shown_metrics_notification =*= Some true) then (
    (* python compatibility: the 22m and 24m are "normal color or intensity",
       and "underline off". It doesn't change how the text is rendered
       but allows us to produce the same exact output as pysemgrep.
       Remove the insertion of pysemgrep_hack once pysemgrep is gone.
       Tip: to visualize special characters that are otherwise invisible
       in a diff, use something like this:
         grep 'METRICS: Using' path/to/output | LESS="X-E"
    *)
    let pysemgrep_hack1, pysemgrep_hack2 =
      (*
         1: make the line yellow using pysemgrep's exact escape sequence
         2: ???
      *)
      match Console.get_highlight () with
      | On -> ("\027[33m\027[22m\027[24m", "\027[0m")
      | Off -> ("", "")
    in
    Logs.app (fun m ->
        m
          "%sMETRICS: Using configs from the Registry (like --config=p/ci) \
           reports pseudonymous rule metrics to semgrep.dev."
          pysemgrep_hack1);
    Logs.app (fun m ->
        m
          "To disable Registry rule metrics, use \"--metrics=off\".@.Using \
           configs only from local files (like --config=xyz.yml) does not \
           enable metrics.@.@.More information: \
           https://semgrep.dev/docs/metrics");
    Logs.app (fun m -> m "%s" pysemgrep_hack2);
    Semgrep_settings.save
      { settings with has_shown_metrics_notification = Some true }
    |> ignore)

(* This function counts how many matches we got by rules:
   [(Rule.t, number of matches : int) list].
   This is use for rule metrics.
*)
let rules_and_counted_matches (res : Core_runner.result) : (Rule.t * int) list =
  let update = function
    | Some n -> Some (succ n)
    | None -> Some 1
  in
  let fold acc (core_match : Out.core_match) =
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

(*****************************************************************************)
(* Error management *)
(*****************************************************************************)

(* python: this used to be done in a _final_raise method from output.py
 * but better separation of concern to do it here.
 *)
let exit_code_of_errors ~strict (errors : Out.core_error list) : Exit_code.t =
  match List.rev errors with
  | [] -> Exit_code.ok ~__LOC__
  (* TODO? why do we look at the last error? What about the other errors? *)
  | x :: _ -> (
      (* alt: raise a Semgrep_error that would be caught by CLI_Common
       * wrapper instead of returning an exit code directly? *)
      match () with
      | _ when x.severity =*= `Error ->
          let exit_code =
            Cli_json_output.exit_code_of_error_type x.error_type
          in
          Logs.info (fun m ->
              m
                "Exiting semgrep scan due to error of severity level=Error: %s \
                 -> exit code %i"
                (Semgrep_output_v1_j.string_of_error_type x.error_type)
                (Exit_code.to_int exit_code));
          exit_code
      | _ when strict ->
          let exit_code =
            Cli_json_output.exit_code_of_error_type x.error_type
          in
          Logs.info (fun m ->
              m
                "Exiting semgrep scan due to error in strict mode: %s -> exit \
                 code %i"
                (Semgrep_output_v1_j.string_of_error_type x.error_type)
                (Exit_code.to_int exit_code));
          exit_code
      | _ -> Exit_code.ok ~__LOC__)

(*****************************************************************************)
(* Incremental display *)
(*****************************************************************************)

(* Note that this hook is run in parallel in Parmap at the end of processing
 * a file. Using Format.std_formatter in parallel requires some synchronization
 * to avoid having the output of multiple child processes interwinded, hence
 * the use of Unix.lockf below.
 *)

let mk_file_match_hook (conf : Scan_CLI.conf) (rules : Rule.rules)
    (printer : Scan_CLI.conf -> Out.cli_match list -> unit) (_file : Fpath.t)
    (match_results : Core_result.matches_single_file) : unit =
  let (cli_matches : Out.cli_match list) =
    (* need to go through a series of transformation so that we can
     * get something that Matches_report.pp_text_outputs can operate on
     *)
    let (pms : Pattern_match.t list) = match_results.matches in
    let (core_matches : Out.core_match list) =
      pms
      (* OK, because we don't need the postprocessing to report the matches. *)
      |> List_.map Core_result.mk_processed_match
      |> Either_.partition_either Core_json_output.match_to_match
      |> fst |> Core_json_output.dedup_and_sort
    in
    let hrules = Rule.hrules_of_rules rules in
    let fixes_env = Fixed_lines.mk_env () in
    core_matches
    |> List_.map
         (Cli_json_output.cli_match_of_core_match
            ~dryrun:conf.output_conf.dryrun fixes_env hrules)
  in
  let cli_matches =
    cli_matches
    |> List_.exclude (fun (m : Out.cli_match) ->
           Option.value ~default:false m.extra.is_ignored)
  in
  if cli_matches <> [] then (
    Unix.lockf Unix.stdout Unix.F_LOCK 0;
    Common.protect
      (fun () ->
        (* coupling: similar to Output.dispatch_output_format for Text *)
        printer conf cli_matches)
      ~finally:(fun () -> Unix.lockf Unix.stdout Unix.F_ULOCK 0))

let incremental_text_printer (conf : Scan_CLI.conf)
    (cli_matches : Out.cli_match list) : unit =
  Matches_report.pp_text_outputs
    ~max_chars_per_line:conf.output_conf.max_chars_per_line
    ~max_lines_per_finding:conf.output_conf.max_lines_per_finding
    ~color_output:conf.output_conf.force_color Format.std_formatter cli_matches

let incremental_json_printer (conf : Scan_CLI.conf)
    (cli_matches : Out.cli_match list) : unit =
  ignore conf;
  List.iter
    (fun cli_match ->
      Fmt.pr "%s@." (Semgrep_output_v1_j.string_of_cli_match cli_match))
    cli_matches

let choose_output_format_and_match_hook (conf : Scan_CLI.conf)
    (rules : Rule.rules) =
  match conf with
  | {
      output_conf = { output_format = Output_format.Text; _ };
      incremental_output = true;
      _;
    }
  | {
      output_conf = { output_format = Output_format.Text; _ };
      common = { maturity = Maturity.Develop; _ };
      _;
    } ->
      ( Output_format.Incremental,
        Some (mk_file_match_hook conf rules incremental_text_printer) )
  | {
   output_conf = { output_format = Output_format.Json; _ };
   incremental_output = true;
   _;
  } ->
      ( Output_format.Incremental,
        Some (mk_file_match_hook conf rules incremental_json_printer) )
  | { output_conf; _ } -> (output_conf.output_format, None)

(*****************************************************************************)
(* Printing stuff for CLI UX *)
(*****************************************************************************)

(* TODO: Update pysemgrep and osemgrep snapshot tests to match new output *)
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

let feature_status ~(enabled : bool) : string =
  if enabled then Ocolor_format.asprintf {|@{<green>✔@}|}
  else Ocolor_format.asprintf {|@{<red>✘@}|}

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
            (feature_status ~enabled:is_enabled)
            (Ocolor_format.asprintf {|@{<bold>%s@}|} feature_name));
      Logs.app (fun m ->
          m "  %s %s\n" (feature_status ~enabled:is_enabled) desc))
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

let uniq_rules_and_error_if_empty_rules rules =
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
  if List_.null rules then Error (Exit_code.missing_config ~__LOC__)
  else Ok rules

(* Select and execute the scan func based on the configured engine settings *)
let mk_core_run_for_osemgrep (caps : < Cap.tmp >) (conf : Scan_CLI.conf)
    (diff_config : Differential_scan_config.t) : Core_runner.func =
  let core_run_for_osemgrep : Core_runner.func =
    match conf.engine_type with
    | OSS -> Core_runner.mk_core_run_for_osemgrep (Core_scan.scan caps)
    | PRO _ -> (
        match !Core_runner.hook_mk_pro_core_run_for_osemgrep with
        | None ->
            (* TODO: improve this error message depending on what the
             * instructions should be *)
            failwith
              "You have requested running semgrep with a setting that requires \
               the pro engine, but do not have the pro engine. You may need to \
               acquire a different binary."
        | Some pro_scan_func ->
            pro_scan_func
              {
                roots = conf.target_roots;
                diff_config;
                engine_type = conf.engine_type;
              })
  in
  let core_run_for_osemgrep : Core_runner.func =
    match conf.targeting_conf.force_project_root with
    | Some (Find_targets.Git_remote _) -> (
        match !Core_runner.hook_pro_git_remote_scan_setup with
        | None ->
            failwith
              "You have requested running semgrep with a setting that requires \
               the pro engine, but do not have the pro engine. You may need to \
               acquire a different binary."
        | Some pro_git_remote_scan_setup ->
            pro_git_remote_scan_setup core_run_for_osemgrep)
    | _ -> core_run_for_osemgrep
  in
  core_run_for_osemgrep

let rules_from_rules_source ~token_opt ~rewrite_rule_ids ~strict caps
    rules_source =
  (* Create the wait hook for our progress indicator *)
  let spinner_ls =
    if Console_Spinner.should_show_spinner () then
      [ Console_Spinner.spinner_async () ]
    else []
  in
  (* Fetch the rules *)
  let rules_and_origins =
    Rule_fetching.rules_from_rules_source_async ~token_opt ~rewrite_rule_ids
      ~strict
      (caps :> < Cap.network ; Cap.tmp >)
      rules_source
  in
  Lwt_platform.run (Lwt.pick (rules_and_origins :: spinner_ls))
[@@profiling]

let adjust_skipped (skipped : Out.skipped_target list)
    (res : Core_runner.result) : Core_runner.result =
  let errors_skipped = Skipped_report.errors_to_skipped res.core.errors in
  let skipped = skipped @ errors_skipped in
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
          (fun (x : Out.skipped_target) -> { x with Out.details = None })
          skipped
      else skipped
    in
    Some skipped
  in
  (* Add the targets that were semgrepignored or errorneous *)
  { res with core = { res.core with paths = { res.core.paths with skipped } } }

(*****************************************************************************)
(* Nosemgrep and autofix *)
(*****************************************************************************)

(* The test test_autofix.py::terraform-ec2-instance-metadata-options.yaml
   carries a newline at the end of the "fix" string, which is not the case
   for PySemgrep.
   TODO Trimming the "fix" here is a hacky workaround, it may be better to dig
   down where and why the newline is inserted into "fix".
*)
let trim_core_match_fix (r : Out.core_match) =
  let fix = Option.map String.trim r.Out.extra.fix in
  let extra = { r.extra with fix } in
  { r with extra }

let adjust_nosemgrep_and_autofix ~keep_ignored (res : Core_runner.result) :
    Core_runner.result =
  let filtered_matches =
    res.core.results
    |> List_.map trim_core_match_fix
    |> Nosemgrep.filter_ignored ~keep_ignored
  in
  { res with core = { res.core with results = filtered_matches } }

(*****************************************************************************)
(* Yet another check targets with rules *)
(*****************************************************************************)
(* this is called also from Ci_subcommand.ml *)
let check_targets_with_rules (caps : < Cap.stdout ; Cap.chdir ; Cap.tmp >)
    (conf : Scan_CLI.conf) (profiler : Profiler.t)
    (rules_and_origins : Rule_fetching.rules_and_origin list)
    (targets_and_skipped : Fpath.t list * Out.skipped_target list) :
    (Rule.rule list * Core_runner.result * Out.cli_output, Exit_code.t) result =
  Metrics_.add_engine_type conf.engine_type;
  (* step 1: last touch on rules *)
  let rules, rule_errors =
    Rule_fetching.partition_rules_and_errors rules_and_origins
  in
  let/ rules = uniq_rules_and_error_if_empty_rules rules in
  let rules = Rule_filtering.filter_rules conf.rule_filtering_conf rules in
  Logs.info (fun m -> m "%a" Rules_report.pp_rules (conf.rules_source, rules));

  (* step 2: printing the skipped targets *)
  let targets, skipped = targets_and_skipped in
  Log_targeting.Log.debug (fun m ->
      m "%a" Targets_report.pp_targets_debug
        (conf.target_roots, skipped, targets));
  Log_targeting.Log.debug (fun m ->
      skipped
      |> List.iter (fun (x : Semgrep_output_v1_t.skipped_target) ->
             m "Ignoring %s due to %s (%s)" !!(x.path)
               (Semgrep_output_v1_t.show_skip_reason x.reason)
               (x.details ||| "")));

  (* step 3: choose the right engine and right hooks *)
  let output_format, file_match_hook =
    choose_output_format_and_match_hook conf rules
  in
  (* step 3': call the engine! *)
  Logs.info (fun m -> m "running the semgrep engine");
  let (result_or_exn : Core_result.result_or_exn) =
    match conf.targeting_conf.baseline_commit with
    | None ->
        Profiler.record profiler ~name:"core_time" (fun () ->
            let core_run_for_osemgrep =
              mk_core_run_for_osemgrep
                (caps :> < Cap.tmp >)
                conf Differential_scan_config.WholeScan
            in
            core_run_for_osemgrep.run ~file_match_hook conf.core_runner_conf
              conf.targeting_conf (rules, rule_errors) targets)
    | Some baseline_commit ->
        (* scan_baseline calls internally Profiler.record "head_core_time"  *)
        (* diff scan mode *)
        let diff_scan_func : Diff_scan.diff_scan_func =
         fun ?(diff_config = Differential_scan_config.WholeScan) targets rules ->
          let core_run_for_osemgrep =
            mk_core_run_for_osemgrep (caps :> < Cap.tmp >) conf diff_config
          in
          core_run_for_osemgrep.run ~file_match_hook conf.core_runner_conf
            conf.targeting_conf (rules, rule_errors) targets
        in
        Diff_scan.scan_baseline
          (caps :> < Cap.chdir ; Cap.tmp >)
          conf profiler baseline_commit targets rules diff_scan_func
  in
  match result_or_exn with
  | Error (e, _core_error_opt) ->
      (* TOADAPT? Runner_exit.exit_semgrep (Unknown_exception e) instead *)
      Exception.reraise e
  | Ok result ->
      let (res : Core_runner.result) = Core_runner.mk_result rules result in
      (* step 3'': adjust the matches, filter via nosemgrep and part1 autofix *)
      let keep_ignored =
        (not conf.core_runner_conf.nosem)
        (* --disable-nosem *)
        || Output_format.keep_ignores output_format
      in
      let res = adjust_nosemgrep_and_autofix ~keep_ignored res in

      (* step 4: adjust the skipped_targets *)
      let res = adjust_skipped skipped res in

      (* step 5: report the matches *)
      Logs.info (fun m -> m "reporting matches if any");
      (* outputting the result on stdout! in JSON/Text/... depending on conf *)
      let cli_output =
        let runtime_params =
          Output.
            {
              is_logged_in = Semgrep_login.is_logged_in ();
              is_using_registry =
                Metrics_.g.is_using_registry
                || !Semgrep_envvars.v.mock_using_registry;
            }
        in
        Output.output_result
          (caps :> < Cap.stdout >)
          { conf.output_conf with output_format }
          runtime_params profiler res
      in
      Profiler.stop_ign profiler ~name:"total_time";

      let rules_with_targets =
        match result_or_exn with
        | Ok r ->
            r.rules_with_targets
            |> List_.map (fun (rv : Rule.rule) -> Rule_ID.to_string (fst rv.id))
        | _ -> []
      in

      if Metrics_.is_enabled () then (
        Metrics_.add_errors cli_output.errors;
        Metrics_.add_rules_hashes_and_rules_profiling ?profiling:res.core.time
          rules;
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
      if conf.autofix then
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
      Ok (rules, res, cli_output)

(*****************************************************************************)
(* Run the real 'scan' subcommand *)
(*****************************************************************************)

let run_scan_conf (caps : caps) (conf : Scan_CLI.conf) : Exit_code.t =
  (* step0: more initializations *)
  (* Print The logo ASAP to minimize time to first meaningful content paint *)
  if new_cli_ux then print_logo ();

  (* imitate pysemgrep for backward compatible profiling metrics ? *)
  let profiler = Profiler.make () in
  (* the corresponding stop is done in check_targets_with_rules () *)
  Profiler.start profiler ~name:"total_time";

  (* This needs to be placed here, near the entry point of the CLI, so that
     it applies for both `--pro` and regular `osemgrep` scanning.
     If we put it in just `Core_scan`, we miss out on `Deep_scan`.
  *)
  let is_debug =
    match conf.common.logging_level with
    | Some Logs.Debug -> true
    | _ -> false
  in
  Core_profiling.profiling := is_debug || conf.core_runner_conf.time_flag;

  (* Metrics initialization (and finalization) is done in CLI.ml,
   * but here we "configure" it (enable or disable it) based on CLI flags.
   *)
  Metrics_.configure conf.metrics;
  let settings : Semgrep_settings.t =
    (fun () ->
      let settings = Semgrep_settings.load ~maturity:conf.common.maturity () in
      add_project_and_config_metrics conf;
      settings)
    |> Profiler.record profiler ~name:"config_time"
  in

  (* Print feature section for enabled products if pattern mode is not used.
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

  notify_user_about_metrics_once settings;

  (* step1: getting the rules *)
  Logs.info (fun m -> m "Getting the rules");
  (* Display a (possibly interactive) message to denote rule fetching *)
  if new_cli_ux then display_rule_source ~rule_source:conf.rules_source;
  let rules_and_origins =
    rules_from_rules_source
      (caps :> < Cap.network ; Cap.tmp >)
      ~token_opt:settings.api_token ~rewrite_rule_ids:conf.rewrite_rule_ids
      ~strict:conf.core_runner_conf.strict conf.rules_source
  in

  (* step2: getting the targets *)
  Logs.info (fun m -> m "Computing the targets");
  let targets_and_skipped =
    Find_targets.get_target_fpaths conf.targeting_conf conf.target_roots
  in

  (* step3: let's go *)
  let res =
    check_targets_with_rules
      (caps :> < Cap.stdout ; Cap.chdir ; Cap.tmp >)
      conf profiler rules_and_origins targets_and_skipped
  in

  (* step4: exit with the right exit code *)
  match res with
  | Error exit_code -> exit_code
  | Ok (_rules, res, cli_output) ->
      (* final result for the shell *)
      if conf.error_on_findings && not (List_.null cli_output.results) then
        Exit_code.findings ~__LOC__
      else
        exit_code_of_errors ~strict:conf.core_runner_conf.strict res.core.errors

(*****************************************************************************)
(* Run 'scan' or 'test' or 'validate' or 'show' (or fallback to pysemgrep) *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : caps) (conf : Scan_CLI.conf) : Exit_code.t =
  (* coupling: if you modify the pysemgrep fallback code below, you
   * probably also need to modify it in Ci_subcommand.ml
   *)
  (match conf.common.maturity with
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

  (* Note that basic logging (Logs_.setup_basic()) was done in CLI.ml before, but
   * in CLI_common.setup_logging() we do the full setup (Logs_.setup()) now
   * that we have a conf object.
   *)
  CLI_common.setup_logging ~force_color:conf.output_conf.force_color
    ~level:conf.common.logging_level;
  Logs.info (fun m -> m "Semgrep version: %s" Version.version);

  let conf =
    (* ugly: also partially done in CLI.ml *)
    (* TOADAPT
       if config.debug then Report.mode := MDebug
       else if config.report_time then Report.mode := MTime
       else Report.mode := MNo_info;
    *)
    if conf.common.profile then (
      (* ugly: no need to set Common.profile, this was done in CLI.ml *)
      Logs.warn (fun m -> m "Profile mode On (running one job, ignoring -j)");
      {
        conf with
        core_runner_conf = { conf.core_runner_conf with num_jobs = 1 };
      })
    else conf
  in
  Logs.debug (fun m -> m "conf = %s" (Scan_CLI.show_conf conf));

  (* some legacy subcommand dispatch *)
  match () with
  (* "alternate modes" where no search is performed.
   * coupling: if you add a new alternate mode, you probably need to modify
   * Scan_CLI.cmdline_term.combine.rules_source match cases and allow
   * more cases returning an empty 'Configs []'.
   * LATER: people should use the new separate subcommands
   * (e.g., 'semgrep show version') instead of abusing 'semgrep scan' flags.
   *)
  | _ when conf.version ->
      CapConsole.print caps#stdout Version.version;
      (* TOPORT: if enable_version_check: version_check() *)
      Exit_code.ok ~__LOC__
  | _ when conf.test <> None ->
      Test_subcommand.run_conf
        (caps :> < Cap.stdout ; Cap.network ; Cap.tmp >)
        (Common2.some conf.test)
  | _ when conf.validate <> None ->
      Validate_subcommand.run_conf
        (caps :> < Cap.stdout ; Cap.network ; Cap.tmp >)
        (Common2.some conf.validate)
  | _ when conf.show <> None ->
      Show_subcommand.run_conf
        (caps :> < Cap.stdout ; Cap.network ; Cap.tmp >)
        (Common2.some conf.show)
  | _ when conf.ls ->
      Ls_subcommand.run ~target_roots:conf.target_roots
        ~targeting_conf:conf.targeting_conf ()
  | _ ->
      (* --------------------------------------------------------- *)
      (* Let's go, this is an actual scan subcommand *)
      (* --------------------------------------------------------- *)
      run_scan_conf caps conf

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : caps) (argv : string array) : Exit_code.t =
  let conf = Scan_CLI.parse_argv (caps :> < Cap.tmp >) argv in
  run_conf caps conf
