open Common
open Fpath_.Operators
module OutJ = Semgrep_output_v1_j

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
type caps = < Cap.stdout ; Cap.network >

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
           Some Exit_code.missing_config ));

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

(* TODO: have run_rules_against_target to take multiple targets instead
*)
let combine_checks (xs : OutJ.checks list) : OutJ.checks =
  OutJ.{ checks = xs |> List.concat_map (fun x -> x.checks) }

let report_tests_result ~json (res : OutJ.tests_result) : unit =
  if json then
    let s = OutJ.string_of_tests_result res in
    Out.put s
  else
    let passed = ref 0 in
    let total = ref 0 in
    res.results
    |> List.iter (fun (_rule_file, (checks : OutJ.checks)) ->
           checks.checks
           |> List.iter (fun (_rule_id, (rule_res : OutJ.rule_result)) ->
                  incr total;
                  if rule_res.passed then incr passed));
    Out.put
      (spf "%d/%d: %s" !passed !total
         (if !passed =|= !total then "✓ All tests passed" else "TODO failure"));
    (* TODO *)
    Out.put "No tests for fixes found."

(*****************************************************************************)
(* Calling the engine *)
(*****************************************************************************)
(* There are multiple entry points to the engine:
 *  - 1: matching/Match_patterns.check(), many patterns vs 1 target,
 *       but no rule
 *  - 2: engine/Match_search_mode.check_rule(), 1 (search) rule vs 1 target,
 *       but just search rule
 *  - 3: engine/Match_rules.check(), many rules vs 1 target,
 *       but just checking part, and for just one target
 *  - 4: core_scan/Core_scan.scan(), many rules vs many targets in //, and
 *       also handle nosemgrep, and errors, and cache, and many other things
 *  - 5: core_scan/Pre_post_core_scan.call_with_pre_and_post_processor()
 *       to handle autofix and secrets validations
 *  - 6: osemgrep/core_runner/Core_runner.mk_scan_func_for_osemgrep()
 *       to fit osemgrep
 *  - 7: osemgrep/cli_scan/Scan_subcommand.run_scan_conf()
 *
 * For 'semgrep test', it's probably better to call directly
 * Check_rules.check() (and some helpers in Test_engine.ml)
 *
 * See also semgrep-server/src/.../Studio_service.ml comment
 * on where to plug to the semgrep engine.
 *)

let run_rules_against_target (xlang : Xlang.t) (rules : Rule.t list)
    (target : Fpath.t) : OutJ.checks * int =
  (* actual matches *)
  let xtarget = Test_engine.xtarget_of_file xlang target in
  let xconf = Match_env.default_xconfig in
  (* TODO? extract rules *)
  let (res : Core_result.matches_single_file) =
    Match_rules.check
      ~match_hook:(fun _ _ -> ())
      ~timeout:0. ~timeout_threshold:0 xconf rules xtarget
  in
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
    res.matches |> Assoc.group_by (fun (pm : Pattern_match.t) -> pm.rule_id.id)
  in
  match
    Core_error.compare_actual_to_expected actual_errors expected_error_lines
  with
  | Ok () ->
      ( OutJ.
          {
            checks =
              matches_by_ruleid
              |> List_.map (fun (id, matches) ->
                     (* alt: we could group by filename in matches, but all those
                      * matches should have the same file
                      *)
                     let reported_lines =
                       matches
                       |> List_.map (fun (pm : Pattern_match.t) ->
                              pm.range_loc |> fst |> fun (loc : Loc.t) ->
                              loc.pos.line)
                     in
                     let expected_lines = reported_lines in
                     let (rule_result : OutJ.rule_result) =
                       OutJ.
                         {
                           passed = true;
                           matches =
                             [ (!!target, { reported_lines; expected_lines }) ];
                           errors = [];
                         }
                     in
                     (Rule_ID.to_string id, rule_result));
          },
        0 )
  | Error (num_errors, _msg) -> (OutJ.{ checks = [] }, num_errors)

(*****************************************************************************)
(* Pad's temporary version *)
(*****************************************************************************)
let run_conf (caps : caps) (conf : Test_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:true ~level:conf.common.logging_level;
  (* Metrics_.configure Metrics_.On; *)
  Logs.debug (fun m -> m "conf = %s" (Test_CLI.show_conf conf));

  let total_mismatch = ref 0 in
  let (results : (Fpath.t * OutJ.checks) list) =
    match conf.target with
    | Test_CLI.Dir (dir, None) ->
        (* coupling: similar to Test_engine.test_rules() *)
        let rule_files =
          [ dir ] |> UFile.files_of_dirs_or_files_no_vcs_nofilter
          |> List.filter Rule_file.is_valid_rule_filename
        in
        rule_files
        |> List_.map (fun rule_file ->
               Logs.info (fun m -> m "processing rule file %s" !!rule_file);
               (* TODO? sanity check? call Check_rule.check()? *)
               let rules = Parse_rule.parse rule_file in
               match Test_engine.find_target_of_yaml_file_opt rule_file with
               | None ->
                   Logs.warn (fun m ->
                       m "could not find target for %s" !!rule_file);
                   (rule_file, OutJ.{ checks = [] })
               | Some target ->
                   Logs.info (fun m -> m "processing target %s" !!target);
                   let xlang =
                     xlang_for_rules_and_target !!rule_file rules target
                   in
                   let checks, num_errors =
                     run_rules_against_target xlang rules target
                   in
                   total_mismatch := !total_mismatch + num_errors;
                   (rule_file, checks))
    | Test_CLI.File (path, config_str)
    | Test_CLI.Dir (path, Some config_str) ->
        let rule_files_and_rules =
          rule_files_and_rules_of_config_string (caps :> Cap.network) config_str
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
               let all_checks =
                 targets
                 |> List_.map (fun target ->
                        Logs.info (fun m -> m "processing target %s" !!target);
                        let xlang =
                          xlang_for_rules_and_target config_str rules target
                        in
                        let checks, num_errors =
                          run_rules_against_target xlang rules target
                        in
                        total_mismatch := !total_mismatch + num_errors;
                        checks)
               in
               (rule_file, combine_checks all_checks))
  in
  Logs.app (fun m -> m "total mismatch: %d" !total_mismatch);
  let res : OutJ.tests_result =
    OutJ.
      {
        results = results |> List_.map (fun (file, checks) -> (!!file, checks));
        (* TODO *)
        fixtest_results = [];
        config_missing_tests = [];
        config_missing_fixtests = [];
        config_with_errors = [];
      }
  in
  report_tests_result ~json:conf.json res;
  if !total_mismatch > 0 then Exit_code.fatal else Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (caps : caps) (argv : string array) : Exit_code.t =
  let conf = Test_CLI.parse_argv argv in
  run_conf caps conf
