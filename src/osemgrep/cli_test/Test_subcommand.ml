open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There was no 'pysemgrep test' subcommand. Tests were run via
 * 'semgrep scan --test ...' but it's better to have a separate
 * subcommand. Note that the legacy 'semgrep scan --test' is redirected
 * to this file after having built a compatible Test_CLI.conf.
 *
 * For more info on how to use Semgrep rule testing infrastructure, see
 * https://semgrep.dev/docs/writing-rules/testing-rules/.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
type caps = < Cap.stdout ; Cap.network ; Cap.tmp >
type fixtest_result = Fpath.t (* target name *) * Out.fixtest_result

(* TODO: define clearly in semgrep_output_v1.atd config_with_errors type
 * and also the errors in rule_result.
 * type config_with_error_output = ...
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* coupling: mostly copy paste of Test_engine.single_xlang_from_rules *)
let xlang_for_rules_and_target (rules_origin : string) (rules : Rule.t list)
    (_target : Fpath.t) : Xlang.t =
  let xlangs = Test_engine.xlangs_of_rules rules in
  match xlangs with
  | [] -> failwith ("no language found in rules " ^ rules_origin)
  | [ x ] -> x
  (* Note that this test whether we have multiple Xlang, but
   * remember that one Xlang.L can contain itself multiple languages
   *)
  | _ :: _ :: _ ->
      let fst = Test_engine.first_xlang_of_rules rules in
      Logs.warn (fun m ->
          m "too many languages found in %s, picking the first one: %s"
            rules_origin (Xlang.show fst));
      fst

let rule_files_and_rules_of_config_string caps
    (config_string : Rules_config.config_string) : (Fpath.t * Rule.t list) list
    =
  let (config : Rules_config.t) =
    Rules_config.parse_config_string ~in_docker:false config_string
  in
  (* LESS: restrict to just File? *)
  let (rules_and_origin : Rule_fetching.rules_and_origin list), errors =
    Rule_fetching.rules_from_dashdash_config ~rewrite_rule_ids:false
      ~token_opt:None caps config
  in

  if errors <> [] then
    raise
      (Error.Semgrep_error
         ( Common.spf "invalid configuration string found: %s" config_string,
           Some (Exit_code.missing_config ~__LOC__) ));

  rules_and_origin
  |> List_.map_filter (fun (x : Rule_fetching.rules_and_origin) ->
         match x.origin with
         | Local_file f ->
             (* TODO: return also rule errors *)
             Some (f, x.rules)
         | CLI_argument
         | Registry
         | App
         | Untrusted_remote _ ->
             Logs.warn (fun m ->
                 m "skipping rules not from local files: %s"
                   (Rule_fetching.show_origin x.origin));
             None)

let combine_checks (xs : Out.checks list) : Out.checks =
  Out.{ checks = xs |> List.concat_map (fun x -> x.checks) }

let fixtest_result_for_target (target : Fpath.t) (pms : Pattern_match.t list) :
    fixtest_result option =
  let fixtest_target_opt =
    (* TODO? Use Fpath instead? Move to Rule_tests.ml?  *)
    let d, b, e = Filename_.dbe_of_filename !!target in
    let fixtest = Filename_.filename_of_dbe (d, b, "fixed." ^ e) in
    if Sys.file_exists fixtest then Some (Fpath.v fixtest) else None
  in
  let (textedits : Textedit.t list) =
    pms |> List.concat_map (fun pm -> Autofix.render_fix pm |> Option.to_list)
  in
  match (fixtest_target_opt, textedits) with
  | None, [] -> None
  | None, _ :: _ ->
      (* stricter: *)
      Logs.warn (fun m ->
          m "no fixtest for test %s but the matches had fixes" !!target);
      None
  | Some fixtest_target, _ :: _ ->
      Logs.info (fun m -> m "Using %s for fixtest" !!fixtest_target);
      let expected_content = UFile.read_file fixtest_target in
      let actual_res =
        Textedit.apply_edits_to_text target (UFile.read_file target) textedits
      in
      let passed =
        match actual_res with
        | Textedit.Success actual_content ->
            let passed = expected_content = actual_content in
            (* TODO: print the diff *)
            if not passed then
              Logs.err (fun m -> m "fixtest differ for %s" !!fixtest_target);
            passed
        | Overlap _ ->
            Logs.err (fun m -> m "fixes overlap for %s" !!target);
            (* TODO? return an error instead ?*)
            false
      in
      Some (target, Out.{ passed })
  | Some _, [] ->
      (* stricter? *)
      Logs.err (fun m -> m "no autofix generated for %s" !!target);
      Some (target, Out.{ passed = false })

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

let report_tests_result (caps : < Cap.stdout >) ~json (res : Out.tests_result) :
    unit =
  if json then
    let s = Out.string_of_tests_result res in
    CapConsole.print caps#stdout s
  else
    let passed = ref 0 in
    let total = ref 0 in
    let fixtest_passed = ref 0 in
    let fixtest_total = ref 0 in
    res.results
    |> List.iter (fun (_rule_file, (checks : Out.checks)) ->
           checks.checks
           |> List.iter (fun (_rule_id, (rule_res : Out.rule_result)) ->
                  incr total;
                  if rule_res.passed then incr passed));
    res.fixtest_results
    |> List.iter (fun (_target_file, (fixtest_result : Out.fixtest_result)) ->
           incr fixtest_total;
           if fixtest_result.passed then incr fixtest_passed);

    (* unit tests *)
    (match () with
    | _ when !total =|= 0 ->
        CapConsole.print caps#stdout
          "No unit tests found. See \
           https://semgrep.dev/docs/writing-rules/testing-rules"
    | _ when !passed =|= !total ->
        CapConsole.print caps#stdout
          (spf "%d/%d: ✓ All tests passed" !passed !total)
    | _else_ ->
        CapConsole.print caps#stdout
          (spf "%d/%d: %dunit tests did not pass:" !passed !total
             (!total - !passed));
        () (* TODO print(check_output_lines) *));
    (* fix tests *)
    (match () with
    | _ when List_.null res.fixtest_results ->
        CapConsole.print caps#stdout "No tests for fixes found."
    | _ when !fixtest_passed =|= !fixtest_total ->
        CapConsole.print caps#stdout
          (spf "%d/%d: ✓ All fix tests passed" !fixtest_passed !fixtest_total)
    | _else_ ->
        CapConsole.print caps#stdout
          (spf "%d/%d: %d fix tests did not pass:" !fixtest_passed
             !fixtest_total
             (!fixtest_total - !fixtest_passed))
        (* TODO print(fixtest_file_diffs) *));
    (* TODO: if config_with_errors_output: ... *)
    ()

(*****************************************************************************)
(* Calling the engine *)
(*****************************************************************************)
(* There are multiple entry points to the engine:
 *  - 1: matching/Match_patterns.check(), many patterns vs 1 target,
 *       but no rule
 *  - 2: engine/Match_search_mode.check_rule(), 1 (search) rule vs 1 target,
 *       but just search rule
 *  - 3: engine/Match_rules.check(), many rules vs 1 target,
 *       but just the checking part, and for just one target
 *  - 3': engine/Test_engine.check(), which is used by semgrep-core -test_rules,
 *       and make core-test, and which calls Match_rules.check(),
 *       but too tied to our semgrep-core test infra (Testo)
 *  - 4: core_scan/Core_scan.scan(), many rules vs many targets in //, and
 *       also handle nosemgrep, and errors, and cache, and many other things,
 *       but require complex arguments (a Core_scan_config)
 *  - 5: core_scan/Pre_post_core_scan.call_with_pre_and_post_processor()
 *       to handle autofix and secrets validations
 *  - 6: osemgrep/core_runner/Core_runner.mk_scan_func_for_osemgrep()
 *       to fit osemgrep,
 *       but it requires even more complex arguments than Core_scan.scan()
 *  - 7: osemgrep/cli_scan/Scan_subcommand.run_scan_conf()
 *       but requires a dependency to cli_scan/, and is a bit heavyweight
 *       for our need which is just to run a few rules on a target test file.
 *
 * For 'semgrep test', it's probably better to call directly
 * Match_rules.check() (with some helpers from Test_engine.ml).
 *
 * See also semgrep-server/src/.../Studio_service.ml comment
 * on where to plug to the semgrep engine.
 *)

(* In theory, we should not return the Fpath.t associated with Out.fixtest_result
 * because it is the same than the target parameter, but it is convenient
 * for the caller as the Out.fix_results field in Out.test_results except the
 * a target name.
 * TODO: return
 *)
let run_rules_against_target (xlang : Xlang.t) (rules : Rule.t list)
    (target : Fpath.t) : Out.checks * fixtest_result option =
  (* actual matches *)
  let xtarget = Test_engine.xtarget_of_file xlang target in
  let xconf = Match_env.default_xconfig in
  (* LATER: extract rules? not part of semgrep OSS anymore, and
   * not added back to semgrep Pro yet, so we should be good for now.
   *)
  let (res : Core_result.matches_single_file) =
    Match_rules.check
      ~match_hook:(fun _ _ -> ())
      ~timeout:0. ~timeout_threshold:0 xconf rules xtarget
  in

  (* fixtest *)
  let fixtest_res = fixtest_result_for_target target res.matches in

  let actual_errors =
    Common.save_excursion Core_error.g_errors [] (fun () ->
        res.matches |> List.iter Core_json_output.match_to_push_error;
        !Core_error.g_errors)
  in

  (* expected matches *)
  (* not tororuleid! not ok:! not todook:
     see https://semgrep.dev/docs/writing-rules/testing-rules/
      for the meaning of those labels.
  *)
  let regexp = ".*\\b\\(ruleid\\|todook\\):.*" in
  let expected_error_lines =
    Core_error.expected_error_lines_of_files ~regexp [ target ]
  in

  let (matches_by_ruleid : (Rule_ID.t, Pattern_match.t list) Assoc.t) =
    if List_.null res.matches then (
      (* maybe some files with todoruleid: without any match yet, but we
       * still want to include them in the json output like pysemgrep
       *)
      (* stricter: *)
      Logs.warn (fun m -> m "nothing matched in %s" !!target);
      rules |> List_.map (fun (r : Rule.t) -> (fst r.id, [])))
    else
      res.matches
      |> Assoc.group_by (fun (pm : Pattern_match.t) -> pm.rule_id.id)
  in
  match
    Core_error.compare_actual_to_expected actual_errors expected_error_lines
  with
  | Ok () ->
      ( Out.
          {
            checks =
              matches_by_ruleid
              |> List_.map (fun (id, matches) ->
                     (* alt: we could group by filename in matches, but all those
                      * matches should have the same file
                      *)
                     let (reported_lines : int list) =
                       matches
                       |> List_.map (fun (pm : Pattern_match.t) ->
                              pm.range_loc |> fst |> fun (loc : Loc.t) ->
                              loc.pos.line)
                       |> List.sort_uniq Int.compare
                     in
                     let expected_lines = reported_lines in
                     (* TODO: not sure why but pysemgrep uses realpaths here *)
                     let filename = Unix.realpath !!target in
                     let (rule_result : Out.rule_result) =
                       Out.
                         {
                           passed = true;
                           matches =
                             [ (filename, { reported_lines; expected_lines }) ];
                           errors = [];
                         }
                     in
                     (Rule_ID.to_string id, rule_result));
          },
        fixtest_res )
  (* TODO, need passed = false above!! *)
  | Error (_num_errors, _msg) -> (Out.{ checks = [] }, fixtest_res)

(*****************************************************************************)
(* Pad's temporary version *)
(*****************************************************************************)
let run_conf (caps : caps) (conf : Test_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:true ~level:conf.common.logging_level;
  (* Metrics_.configure Metrics_.On; ?? and allow to disable it?
   * semgrep-rules/Makefile is running semgrep --test with metrics=off
   * (and also --disable-version-check), but maybe because it is used from
   *  'semgrep scan'; in 'osemgrep test' context, we should not even have
   *  those options and we should disable metrics (and version-check) by default.
   *)
  Logs.debug (fun m -> m "conf = %s" (Test_CLI.show_conf conf));

  (* The first Fpath is a rule file, the one before fixtest_result is a target *)
  let (results
        : (Fpath.t * Out.checks * (Fpath.t * Out.fixtest_result) list) list) =
    match conf.target with
    | Test_CLI.Dir (dir, None) ->
        (* coupling: similar to Test_engine.test_rules() *)
        let rule_files =
          [ dir ] |> UFile.files_of_dirs_or_files_no_vcs_nofilter
          |> List.filter Rule_file.is_valid_rule_filename
        in
        rule_files
        |> List_.map (fun (rule_file : Fpath.t) ->
               Logs.info (fun m -> m "processing rule file %s" !!rule_file);
               (* TODO? sanity check? call metachecker Check_rule.check()? *)
               let rules = Parse_rule.parse rule_file in
               match Test_engine.find_target_of_yaml_file_opt rule_file with
               | None ->
                   (* stricter: *)
                   Logs.warn (fun m ->
                       m "could not find target for %s" !!rule_file);
                   (rule_file, Out.{ checks = [] }, [])
               | Some target ->
                   Logs.info (fun m -> m "processing target %s" !!target);
                   let xlang =
                     xlang_for_rules_and_target !!rule_file rules target
                   in
                   let checks, fixtest_res =
                     run_rules_against_target xlang rules target
                   in
                   (rule_file, checks, fixtest_res |> Option.to_list))
    | Test_CLI.File (path, config_str)
    | Test_CLI.Dir (path, Some config_str) ->
        let rule_files_and_rules =
          rule_files_and_rules_of_config_string
            (caps :> < Cap.network ; Cap.tmp >)
            config_str
        in
        (* alt: use Find_targets.get_target_fpaths but then it requires
         * a Find_targets.conf, and this will respect the .semgrepignore
         * which may be annoying, so simpler to just get all the files
         * under the directory
         *)
        let targets = UFile.files_of_dirs_or_files_no_vcs_nofilter [ path ] in

        rule_files_and_rules
        |> List_.map (fun (rule_file, rules) ->
               Logs.info (fun m -> m "processing rule file %s" !!rule_file);
               let all_checks, all_fixtest =
                 targets
                 |> List_.map (fun target ->
                        Logs.info (fun m -> m "processing target %s" !!target);
                        let xlang =
                          xlang_for_rules_and_target config_str rules target
                        in
                        let checks, fixtest_res =
                          run_rules_against_target xlang rules target
                        in
                        (checks, fixtest_res |> Option.to_list))
                 |> List.split
               in
               (rule_file, combine_checks all_checks, List.flatten all_fixtest))
  in
  let res : Out.tests_result =
    Out.
      {
        results =
          results
          |> List_.map (fun (rule_file, checks, _fix) -> (!!rule_file, checks));
        fixtest_results =
          results
          |> List.concat_map (fun (_rule_file, _checks, fixtest_results) ->
                 fixtest_results
                 |> List_.map (fun (target_file, passed) ->
                        (!!target_file, passed)));
        (* TODO *)
        config_missing_tests = [];
        config_missing_fixtests = [];
        config_with_errors = [];
      }
  in
  (* just to reproduce what pysemgrep is doing *)
  res.fixtest_results
  |> List.iter (fun (_target, (fixtest_res : Out.fixtest_result)) ->
         if fixtest_res.passed then
           Logs.app (fun m -> m "scucessfully modified 1 file."));

  (* final report *)
  report_tests_result (caps :> < Cap.stdout >) ~json:conf.json res;
  (* TODO: strict  and bool(config_with_errors_output) *)
  let strict_error = false in
  let any_failures =
    res.results
    |> List.exists (fun (_rule_file, (checks : Out.checks)) ->
           checks.checks
           |> List.exists (fun (_rule_id, (res : Out.rule_result)) ->
                  not res.passed))
  in
  let any_fixtest_failures =
    res.fixtest_results
    |> List.exists (fun (_target_file, (res : Out.fixtest_result)) ->
           not res.passed)
  in
  if strict_error || any_failures || any_fixtest_failures then
    Exit_code.findings ~__LOC__
  else Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : caps) (argv : string array) : Exit_code.t =
  let conf = Test_CLI.parse_argv argv in
  run_conf caps conf
