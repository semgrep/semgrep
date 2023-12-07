open Fpath_.Operators

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
(* Types *)
(*****************************************************************************)

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
    (target : Fpath.t) : int =
  (* actual matches *)
  let xtarget = Test_engine.xtarget_of_file xlang target in
  let xconf = Match_env.default_xconfig in
  (* TODO? extract rules *)
  let res =
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

  match
    Core_error.compare_actual_to_expected actual_errors expected_error_lines
  with
  | Ok () -> 0
  | Error (num_errors, _msg) -> num_errors

(*****************************************************************************)
(* Pad's temporary version *)
(*****************************************************************************)
let run_conf (conf : Test_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:true ~level:conf.common.logging_level;
  (* Metrics_.configure Metrics_.On; *)
  Logs.debug (fun m -> m "conf = %s" (Test_CLI.show_conf conf));

  match conf.target with
  | Test_CLI.Dir (dir, None) ->
      let total_mismatch = ref 0 in
      (* coupling: similar to Test_engine.test_rules() *)
      let rule_files =
        [ dir ] |> UFile.files_of_dirs_or_files_no_vcs_nofilter
        |> List.filter Parse_rule.is_valid_rule_filename
      in
      rule_files
      |> List.iter (fun rule_file ->
             Logs.info (fun m -> m "processing rule file %s" !!rule_file);
             (* TODO? sanity check? call Check_rule.check()? *)
             let rules = Parse_rule.parse rule_file in
             match Test_engine.find_target_of_yaml_file_opt rule_file with
             | None ->
                 Logs.warn (fun m ->
                     m "could not find target for %s" !!rule_file)
             | Some target ->
                 Logs.info (fun m -> m "processing target %s" !!target);
                 let xlang =
                   xlang_for_rules_and_target !!rule_file rules target
                 in
                 let num_errors = run_rules_against_target xlang rules target in
                 total_mismatch := !total_mismatch + num_errors);
      Logs.app (fun m -> m "total mismatch: %d" !total_mismatch);
      if !total_mismatch > 0 then Exit_code.fatal else Exit_code.ok
  | _else_ -> failwith "TODO2"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (argv : string array) : Exit_code.t =
  let conf = Test_CLI.parse_argv argv in
  run_conf conf
