(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
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
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse a semgrep-test command, execute it and return an exit code.
 *
 * There are multiple ways to call `semgrep test` but its main use is to
 * take a directory as an argument as in `semgrep test semgrep-rules/ocaml`
 * and run all the tests in the directory recursively.
 * For each directory containing YAML rules, it run those rules on the file in
 * the same directory with the same name but a different extension
 * (e.g., eqeq.yaml runs on eqeq.py). It then validates that the output is
 * annotated in the source file with by looking for a comment like:
 * ```
 * # ruleid:eqeq-is-bad
 * ```
 * On the preceeding line.
 *
 * For more info on how to use the Semgrep rule testing infrastructure, see
 * https://semgrep.dev/docs/writing-rules/testing-rules/.
 *
 * Note that there was no 'pysemgrep test' subcommand. Tests were run via
 * 'semgrep scan --test ...' but it's better to have a separate subcommand.
 * Note that the legacy 'semgrep scan --test' is redirected to this file after
 * having built a compatible Test_CLI.conf.
 *
 * TODO: conf.ignore_todo? conf.strict?
 * LATER: factorize code with Unit_engine.ml and Test_engine.ml
 *
 * This is a port of test.py
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)
(* no need for Cap.network; the tested rules should be local *)
type caps = < Cap.stdout ; Cap.fork ; Cap.alarm >

(* rules and targets to test together
 * TODO: extend to target list for .js/.ts and for interfile tests later
 *)
type tests = (Fpath.t (* rule file *) * Fpath.t (* target *)) list

type tests_result =
  (Fpath.t (* rule file *) * test_result list * fixtest_result list) list

(* Intermediate type because semgrep_output_v1.atd does not have
 * a good type name for those. Note that we can't easily change the .atd
 * because we must remain backward compatible with pysemgrep and current
 * users of semgrep --test.
 * For example, the Out.checks type introduces a useless intermediate record
 * and the rule ids are strings instead of a proper type, but because
 * we use <json repr="object">, we can't even use a proper wrap rule_id type
 * for it. At least here we use Rule_ID.t.
 *)
and test_result = Rule_ID.t * Out.rule_result

(* TODO? add diff between .fixed and actual for error management? *)
and fixtest_result = Fpath.t (* target name *) * Out.fixtest_result

(* TODO: define clearly in semgrep_output_v1.atd config_with_errors type
 * and also the errors in rule_result.
 * type config_with_error_output = ...
 * alt: add a | UnparsableRule of Fpath.t, but simpler to just raise early.
 *)
type error =
  (* there is a rule but there is no target file *)
  | MissingTest of Fpath.t (* rule file *)
  (* the rule when applied produces fixes, but there is no .fixed file *)
  | MissingFixtest of Fpath.t (* rule file *)

(* Useful for error management *)
type env = {
  (* current processed rule file *)
  rule_file : Fpath.t;
  (* use a ref so easy to store all the errors returned by different functions.
   * alt: get each functions returning different kind of errors
   *)
  errors : error list ref;
}

type linenb = Test_annotation.linenb

(* TODO: move in core/ ? used in other files? was in constants.py in pysemgrep *)
let break_line =
  "--------------------------------------------------------------------------------"

(*****************************************************************************)
(* File targeting (the set of tests) *)
(*****************************************************************************)

let rules_and_targets (kind : Test_CLI.target_kind) (errors : error list ref) :
    tests =
  match kind with
  | Test_CLI.Dir (dir, None) ->
      (* coupling: similar to Test_engine.test_rules() *)
      let rule_files =
        [ dir ] |> UFile.files_of_dirs_or_files_no_vcs_nofilter
        |> List.filter Rule_file.is_valid_rule_filename
      in
      rule_files
      |> List_.filter_map (fun (rule_file : Fpath.t) ->
             (* TODO: just get all targets with same basename *)
             match Test_engine.find_target_of_yaml_file_opt rule_file with
             | None ->
                 (* stricter: (but reported via config_missing_tests in JSON)*)
                 Logs.warn (fun m ->
                     m "could not find target for %s" !!rule_file);
                 Stack_.push (MissingTest rule_file) errors;
                 None
             | Some target -> Some (rule_file, target))
  | Test_CLI.File (target, config_str) -> (
      match Rules_config.parse_config_string ~in_docker:false config_str with
      | File rule_file -> [ (rule_file, target) ]
      | Dir _
      | URL _
      | R _
      | A _ ->
          (* stricter: *)
          failwith "the config must be a local file")
  (* this is to allow to have the rules in a different directories than
   * the targets as in `osemgrep test --config tests/rules/ tests/targets/
   * see https://semgrep.dev/docs/writing-rules/testing-rules#storing-rules-and-test-targets-in-different-directories
   *)
  | Test_CLI.Dir (_dir_targets, Some config_str) -> (
      match Rules_config.parse_config_string ~in_docker:false config_str with
      | Dir _dir_rules ->
          (* TODO: this was working at some point when we were both doing
           * the file tarteting phase (plan) and analysis phase (results)
           * together because we were (incorrectly) getting all the rules
           * in the directory and then running all those rules on all
           * the targets.
           * The right thing to do instead is to match each rule file
           * in dir with a target file in the other dir
           *)
          failwith
            "TODO: the split of tests/ and rules/ is not supported yet in \
             semgrep test"
      | File _
      | URL _
      | R _
      | A _ ->
          (* stricter: *)
          failwith "the config must be a local directory")

(*****************************************************************************)
(* Fixtest *)
(*****************************************************************************)

let fixtest_of_target_opt (target : Fpath.t) : Fpath.t option =
  (* TODO? Use Fpath instead? Move to Rule_tests.ml?
   * TODO? does it handle the foo.fixed.test.yaml for yaml?
   *)
  let d, b, e = Filename_.dbe_of_filename !!target in
  let fixtest = Filename_.filename_of_dbe (d, b, "fixed." ^ e) in
  if Sys.file_exists fixtest then Some (Fpath.v fixtest) else None

(* TODO use capability and cleanup Test_parsing.ml and remove
 * Common2.unix_diff
 *)
let unix_diff (str1 : string) (str2 : string) : string list =
  UTmp.with_temp_file ~contents:str1 ~suffix:".x" (fun file1 ->
      UTmp.with_temp_file ~contents:str2 ~suffix:".x" (fun file2 ->
          Common2.unix_diff !!file1 !!file2))

let fixtest_result_for_target (_env : env) (target : Fpath.t)
    (fixtest_target : Fpath.t) (pms : Pattern_match.t list) : fixtest_result =
  Logs.info (fun m -> m "Using %s for fixtest" !!fixtest_target);
  let (textedits : Textedit.t list) =
    pms |> List.concat_map (fun pm -> Autofix.render_fix pm |> Option.to_list)
  in
  (* stricter? *)
  if List_.null textedits then
    Logs.warn (fun m -> m "no autofix generated for %s" !!target);

  let expected_content = UFile.read_file fixtest_target in
  let actual_res =
    Textedit.apply_edits_to_text target (UFile.read_file target) textedits
  in
  let passed =
    match actual_res with
    | Textedit.Success actual_content ->
        let passed = expected_content = actual_content in
        (if not passed then
           let diff = unix_diff expected_content actual_content in
           Logs.err (fun m ->
               m "fixtest failed for %s, diff =\n%s" !!fixtest_target
                 (String.concat "\n" diff)));
        passed
    | Overlap _ ->
        Logs.err (fun m -> m "fixes overlap for %s" !!target);
        (* TODO? return an error instead ?*)
        false
  in
  (target, Out.{ passed })

(* LATER: still enough in steps mode? *)
let rule_contain_fix_or_fix_regex (rule : Rule.t) : bool =
  match rule with
  | { fix = Some _; _ }
  | { fix_regexp = Some _; _ } ->
      true
  | _else_ -> false

(*****************************************************************************)
(* Diagnosis (Brandon's experiment) *)
(*****************************************************************************)

let report_diagnosis print (res : Out.tests_result) : unit =
  let diagnoses =
    res.results
    |> List.concat_map (fun (rule_file, (checks : Out.checks)) ->
           checks.checks
           |> List.concat_map (fun (_rule_id, (rule_res : Out.rule_result)) ->
                  match rule_res.diagnosis with
                  | Some d -> [ (rule_file, d) ]
                  | None -> []))
  in
  if List.length diagnoses <> 0 then (
    print break_line;
    print "Matching diagnosis:";
    diagnoses
    |> List.iter (fun (rule_file, d) ->
           print (Diagnosis.report ~rule_file:(Fpath.v rule_file) d)))

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

let tests_result_of_tests_result (results : tests_result) (errors : error list)
    : Out.tests_result =
  Out.
    {
      results =
        results
        |> List_.map (fun (rule_file, checks, _fix) ->
               ( !!rule_file,
                 {
                   checks =
                     checks
                     |> List_.map (fun (id, xs) -> (Rule_ID.to_string id, xs));
                 } ));
      fixtest_results =
        results
        |> List.concat_map (fun (_rule_file, _checks, fixtest_results) ->
               fixtest_results
               |> List_.map (fun (target_file, passed) ->
                      (!!target_file, passed)));
      (* TODO: change the schema and use an enum instead of those fields *)
      config_missing_tests =
        errors
        |> List_.filter_map (function
             | MissingTest rule_file -> Some rule_file
             | _else_ -> None)
        |> List.sort Fpath.compare;
      config_missing_fixtests =
        errors
        |> List_.filter_map (function
             | MissingFixtest rule_file -> Some rule_file
             | _else_ -> None)
        |> List.sort Fpath.compare;
      (* TODO: rename to just 'errors' and put the missing_tests and missing
       * fixtests here as a kind of error.
       *)
      config_with_errors = [];
    }

let report_tests_result (caps : < Cap.stdout >) ~matching_diagnosis ~json
    (res : Out.tests_result) : unit =
  let print str = CapConsole.print caps#stdout str in
  if json then
    let s = Out.string_of_tests_result res in
    print s
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

    (* "unit" tests *)
    (match () with
    | _ when !total =|= 0 ->
        (* TODO: exit error code instead? *)
        print
          "No unit tests found. See \
           https://semgrep.dev/docs/writing-rules/testing-rules"
    | _ when !passed =|= !total ->
        print (spf "%d/%d: ✓ All tests passed" !passed !total)
    | _else_ ->
        print
          (spf "%d/%d: %d unit tests did not pass:" !passed !total
             (!total - !passed));
        print break_line;
        (* TODO *)
        print "TODO: print(check_output_lines)");
    (* fix tests *)
    (match () with
    | _ when List_.null res.fixtest_results -> print "No tests for fixes found."
    | _ when !fixtest_passed =|= !fixtest_total ->
        print
          (spf "%d/%d: ✓ All fix tests passed" !fixtest_passed !fixtest_total)
    | _else_ ->
        print
          (spf "%d/%d: %d fix tests did not pass:" !fixtest_passed
             !fixtest_total
             (!fixtest_total - !fixtest_passed));
        print break_line;
        (* TODO *)
        print "TODO: print(fixtest_file_diffs)");
    if matching_diagnosis then report_diagnosis print res;
    (* TODO: if config_with_errors_output: ... *)
    ()

(*****************************************************************************)
(* Calling the engine *)
(*****************************************************************************)
(* There are multiple entry points to the "engine":
 *  - 1: matching/Match_patterns.check(), many patterns vs 1 target,
 *       but no rule (no formula)
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
 * For 'osemgrep test', it is better to call Core_scan.scan(), especially
 * now that Core_scan_config.t has been simplified. We used to call
 * Match_rules.check() and use a few helpers from Test_engine.ml,
 * but this was then difficult to extend to support --pro. By using
 * Core_scan.scan(), it's relatively easy to add hooks to switch to
 * Deep_scan.scan() for pro rules and interfile tests.
 * Using Core_scan.scan() would also make it easier to support extract rules.
 *
 * See also server/src/.../Studio_service.ml comment
 * on where to plug to the semgrep engine.
 *)

let run_rules_against_targets ~matching_diagnosis caps (rules : Rule.t list)
    (targets : Target.t list) : Core_result.result_or_exn =
  (* old:
   * let xtarget = Test_engine.xtarget_of_file xlang target in
   * let xconf = { Match_env.default_xconfig with matching_explanations = true}in
   * Match_rules.check ~match_hook:(fun _ ->()) ~timeout:None xconf rules xtarget
   *)
  let config : Core_scan_config.t =
    {
      Core_scan_config.default with
      rule_source = Rules rules;
      target_source = Targets targets;
      output_format = NoOutput;
      (* activate matching explanations for Diagnosis to work *)
      matching_explanations = matching_diagnosis;
      (* try to be as close as possible as a real scan to avoid differences
       * between semgrep test and semgrep scan behavior
       *)
      filter_irrelevant_rules = true;
      (* in a test context, we don't want to honor the paths: (include/exclude)
       * directive since the test target file, which must have the same
       * basename than the rule, may not match the paths: of the rule
       *)
      respect_rule_paths = false;
    }
  in
  Core_scan.scan caps config

(*****************************************************************************)
(* Comparing *)
(*****************************************************************************)

let compare_actual_to_expected (env : env) (target : Fpath.t)
    (rules : Rule.t list) (matches : Pattern_match.t list)
    (explanations : Matching_explanation.t list option)
    (annots : (Test_annotation.t * linenb) list) :
    test_result list * fixtest_result option =
  (* actual matches *)
  let (matches_by_ruleid : (Rule_ID.t, Pattern_match.t list) Assoc.t) =
    if List_.null matches then (
      (* stricter: *)
      Logs.warn (fun m -> m "nothing matched in %s" !!target);
      (* Probably some files with todoruleid: without any match yet, but we
       * still want to include them in the JSON output like pysemgrep.
       *)
      if not (List_.null annots) then
        rules |> List_.map (fun (r : Rule.t) -> (fst r.id, []))
      else [])
    else matches |> Assoc.group_by (fun (pm : Pattern_match.t) -> pm.rule_id.id)
  in
  (* expected matches *)
  let (expected_by_ruleid : (Rule_ID.t, linenb list) Assoc.t) =
    Test_annotation.group_positive_annotations annots
  in

  (* regular ruleid tests *)
  let (checks : (Rule_ID.t * Out.rule_result) list) =
    matches_by_ruleid
    |> List_.map (fun (id, (matches : Pattern_match.t list)) ->
           (* alt: use Core_error.compare_actual_to_expected  *)
           (* alt: we could group by filename in matches, but all those
            * matches should have the same file
            *)
           let (reported_lines : linenb list) =
             matches
             |> List_.map (fun (pm : Pattern_match.t) ->
                    pm.range_loc |> fst |> fun (loc : Loc.t) -> loc.pos.line)
             |> List.sort_uniq Int.compare
           in
           let (expected_lines : linenb list) =
             match Assoc.find_opt id expected_by_ruleid with
             | Some xs -> xs
             | None -> []
           in
           let passed = reported_lines =*= expected_lines in
           if not passed then
             Logs.err (fun m ->
                 m "test failed for rule id %s on target %s"
                   (Rule_ID.to_string id) !!target);
           (* TODO: not sure why but pysemgrep uses realpaths here, which is
            * a bit annoying because it forces us to use masks in test snapshots
            *)
           let filename = Unix.realpath !!target in
           (* TODO: not sure why pysemgrep does not report the real
            * reported_lines (and expected_lines) and filter those todook:
            *)
           let expected_reported =
             let reported_lines =
               Test_annotation.filter_todook annots reported_lines
             in
             let expected_lines =
               Test_annotation.filter_todook annots expected_lines
             in
             { Out.reported_lines; expected_lines }
           in
           let diagnosis =
             let* explanations = explanations in
             Some
               (Diagnosis.diagnose ~target ~rule_file:env.rule_file
                  expected_reported explanations)
           in
           let (rule_result : Out.rule_result) =
             Out.
               {
                 passed;
                 matches = [ (filename, expected_reported) ];
                 (* TODO: error from the engine ? *)
                 errors = [];
                 diagnosis;
               }
           in
           (id, rule_result))
  in
  (* optional fixtest *)
  let (fixtest_res : (Fpath.t (* target *) * Out.fixtest_result) option) =
    match
      ( fixtest_of_target_opt target,
        rules |> List.exists rule_contain_fix_or_fix_regex )
    with
    | None, true ->
        (* stricter: (reported in JSON at least via config_missing_fixtests) *)
        Logs.warn (fun m ->
            m "no fixtest for test %s but the rule file %s uses autofix"
              !!target !!(env.rule_file));
        Stack_.push (MissingFixtest env.rule_file) env.errors;
        None
    | Some fixtest, false ->
        (* stricter? *)
        Logs.err (fun m ->
            m
              "found the fixtest %s but the rule file %s does not contain \
               autofix"
              !!fixtest !!(env.rule_file));
        None
    | None, false -> None
    | Some fixtest_target, true ->
        Some (fixtest_result_for_target env target fixtest_target matches)
  in
  (checks, fixtest_res)

(*****************************************************************************)
(* Run the tests *)
(*****************************************************************************)
let run_tests (caps : Core_scan.caps) ~matching_diagnosis (tests : tests)
    (errors : error list ref) :
    (Fpath.t (* rule file *) * test_result list * fixtest_result list) list =
  (* LATER: in theory we could use Parmap here *)
  tests
  |> List_.map (fun (rule_file, target_file) ->
         Logs.info (fun m -> m "processing rule file %s" !!rule_file);
         (* TODO? sanity check? call metachecker Check_rule.check()? *)
         match Parse_rule.parse_and_filter_invalid_rules rule_file with
         | Ok (rules, []) -> (
             Logs.info (fun m -> m "processing target %s" !!target_file);
             (* one target file can result in different targets
              * if the rules contain multiple xlangs
              *)
             let targets : Target.t list =
               Core_runner.targets_for_files_and_rules [ target_file ] rules
             in
             let env = { rule_file; errors } in
             let res_or_exn : Core_result.result_or_exn =
               run_rules_against_targets ~matching_diagnosis caps rules targets
             in
             let (expected : (Test_annotation.t * linenb) list) =
               Test_annotation.annotations target_file
             in
             match res_or_exn with
             | Error exn -> Exception.reraise exn
             | Ok res ->
                 let matches =
                   res.processed_matches
                   |> List_.map (fun (x : Core_result.processed_match) -> x.pm)
                 in
                 let explanations = res.explanations in
                 let checks, fixtest_res =
                   compare_actual_to_expected env target_file rules matches
                     explanations expected
                 in
                 (rule_file, checks, fixtest_res |> Option.to_list))
         | Ok (_, _ :: _)
         | Error _ ->
             (* alt: use List_.filter_map above and be more fault tolerant
              * with a Stack_.push (UnparsableRule rule_file) errors;
              * but simpler to raise errors early
              *)
             raise
               (Error.Semgrep_error
                  ( spf "invalid configuration found in %s" !!rule_file,
                    Some (Exit_code.missing_config ~__LOC__) ))
         | (exception Parsing_error.Syntax_error _)
         | (exception Parsing_error.Other_error _) ->
             failwith
               "impossible: parse_and_filter_invalid_rules should not raise \
                exns")

(*****************************************************************************)
(* Run the conf *)
(*****************************************************************************)
let run_conf (caps : caps) (conf : Test_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:true ~level:conf.common.logging_level;
  (* Metrics_.configure Metrics_.On; ?? and allow to disable it?
   * semgrep-rules/Makefile is running semgrep --test with metrics=off
   * (and also --disable-version-check), but maybe because it is used from
   * 'semgrep scan'; in 'osemgrep test' context, we should not even have
   * those options and we should disable metrics (and version-check) by default.
   *)
  Logs.debug (fun m -> m "conf = %s" (Test_CLI.show_conf conf));
  let matching_diagnosis = conf.matching_diagnosis in
  let errors = ref [] in

  (* step1: compute the set of tests (rule + target) *)
  (* TODO: support multiple targets (e.g., .jsx and .tsx) analyzed independently
   * and later multiple targets analyzed together for --pro interfile analysis.
   *)
  let tests : tests = rules_and_targets conf.target errors in

  (* step2: run the tests *)
  let result : tests_result =
    run_tests (caps :> Core_scan.caps) ~matching_diagnosis tests errors
  in

  (* step3: report the test results *)
  let res : Out.tests_result = tests_result_of_tests_result result !errors in
  (* pysemgrep is reporting some "successfully modified 1 file."
   * before the final report, but actually it reports that even on failing
   * fixtests, so better to not imitate for now.
   *)
  (* final report *)
  report_tests_result
    (caps :> < Cap.stdout >)
    ~matching_diagnosis ~json:conf.json res;

  (* step4: compute the exit code *)

  (* TODO: and bool(config_with_errors_output) *)
  let strict_error = conf.strict && false in
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
