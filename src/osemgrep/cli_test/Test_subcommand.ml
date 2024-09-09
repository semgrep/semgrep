open Common
open Fpath_.Operators
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse a semgrep-test command, execute it and exit.
 *
 * For each directory containing YAML rules, run those rules on the file in the
 * same directory with the same name but different extension.
 * E.g. eqeq.yaml runs on eqeq.py.
 * Validate that the output is annotated in the source file with by looking for
 * a comment like:
 * ```
 * # ruleid:eqeq-is-bad
 * ```
 * On the preceeding line.
 *
 * For more info on how to use Semgrep rule testing infrastructure, see
 * https://semgrep.dev/docs/writing-rules/testing-rules/.
 *
 *
 * There was no 'pysemgrep test' subcommand. Tests were run via
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
type caps = < Cap.stdout >

(* Intermediate type because semgrep_output_v1.atd does not have
 * a good type name for those. Note that we can't easily change the .atd
 * because we must remain backward compatible with pysemgrep and current
 * users of semgrep --test.
 * For example, the Out.checks type introduces a useless intermediate record
 * and the rule ids are strings instead of a proper type, but because
 * we use <json repr="object">, we can't even use a proper wrap rule_id type
 * for it. At least here we use Rule_ID.t.
 *)
type test_result = Rule_ID.t * Out.rule_result

(* TODO? add diff between .fixed and actual for error management? *)
type fixtest_result = Fpath.t (* target name *) * Out.fixtest_result
type linenb = Test_annotation.linenb

(* TODO: move in core/ ? used in other files? was in constants.py in pysemgrep *)
let break_line =
  "--------------------------------------------------------------------------------"

(* TODO: define clearly in semgrep_output_v1.atd config_with_errors type
 * and also the errors in rule_result.
 * type config_with_error_output = ...
 *)
type error =
  (* there is a rule but there is no target file *)
  | MissingTest of Fpath.t (* rule file *)
  (* the rule when applied produces fixes, but there is no .fixed file *)
  | MissingFixtest of Fpath.t

(* rule file *)
(* alt: UnparsableRule of Fpath.t, but simpler to just raise early *)

(* Useful for error management *)
type env = {
  (* current processed rule file *)
  rule_file : Fpath.t;
  (* alt: get each functions returning different kind of errors *)
  errors : error list ref;
}

(*****************************************************************************)
(* File targeting *)
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

(*****************************************************************************)
(* Fixtest *)
(*****************************************************************************)

let fixtest_of_target_opt (target : Fpath.t) : Fpath.t option =
  (* TODO? Use Fpath instead? Move to Rule_tests.ml?  *)
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
(* Reporting *)
(*****************************************************************************)

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
    if matching_diagnosis then (
      (* diagnosis *)
      let diagnoses =
        List.concat_map
          (fun (rule_file, (checks : Out.checks)) ->
            List.concat_map
              (fun (_rule_id, (rule_res : Out.rule_result)) ->
                match rule_res.diagnosis with
                | Some d -> [ (rule_file, d) ]
                | None -> [])
              checks.checks)
          res.results
      in
      if List.length diagnoses <> 0 then (
        print break_line;
        print "Matching diagnosis:";
        List.iter
          (fun (rule_file, d) ->
            print (Diagnosis.report ~rule_file:(Fpath.v rule_file) d))
          diagnoses);
      (* TODO: if config_with_errors_output: ... *)
      ())

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
 * For 'osemgrep test', it is better to call directly
 * Match_rules.check() and use a few helpers from Test_engine.ml.
 * TODO: switch to Core_scan which now has a simpler Core_scan_config.t
 * and will help to switch to Deep_scan when using semgrep test --pro.
 *
 * LATER: what about extract rules? They are not handled by Match_rules.check().
 * But they are not part of semgrep OSS anymore, and were not added back to
 * semgrep Pro yet, so we should be good for now. If we want to write
 * extract-mode rule tests, we'll need to adjust things.
 *
 * See also server/src/.../Studio_service.ml comment
 * on where to plug to the semgrep engine.
 *)

let run_rules_against_target (xlang : Xlang.t) (rules : Rule.t list)
    (target : Fpath.t) : Core_result.matches_single_file =
  (* running the engine *)
  let xtarget = Test_engine.xtarget_of_file xlang target in
  (* activate matching explanations for Diagnosis to work *)
  let xconf = { Match_env.default_xconfig with matching_explanations = true } in
  (* TODO switch to Core_scan.scan so easier to add a hook so we would
   * call Deep_scan.scan instead when using osemgrep test --pro
   *)
  Match_rules.check
    ~match_hook:(fun _pm -> ())
    ~timeout:None xconf rules xtarget

let compare_actual_to_expected ~matching_diagnosis (env : env)
    (target : Fpath.t) (rules : Rule.t list)
    (res : Core_result.matches_single_file)
    (annots : (Test_annotation.t * linenb) list) :
    test_result list * fixtest_result option =
  (* actual matches *)
  let (matches_by_ruleid : (Rule_ID.t, Pattern_match.t list) Assoc.t) =
    if List_.null res.matches then (
      (* stricter: *)
      Logs.warn (fun m -> m "nothing matched in %s" !!target);
      (* Probably some files with todoruleid: without any match yet, but we
       * still want to include them in the JSON output like pysemgrep.
       *)
      if not (List_.null annots) then
        rules |> List_.map (fun (r : Rule.t) -> (fst r.id, []))
      else [])
    else
      res.matches
      |> Assoc.group_by (fun (pm : Pattern_match.t) -> pm.rule_id.id)
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
             if matching_diagnosis then
               Some
                 (Diagnosis.diagnose ~target ~rule_file:env.rule_file
                    expected_reported res.explanations)
             else None
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
        Some (fixtest_result_for_target env target fixtest_target res.matches)
  in
  (checks, fixtest_res)

(*****************************************************************************)
(* Run the conf *)
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

  let errors = ref [] in

  (* TODO: switch to Target.t. Then support multiple targets tested
   * independently (e.g., .jsx and .tsx) and later multiple targets
   * analyzed together for --pro interfile analysis.
   *)
  let plan : (Fpath.t (* rule file *) * Fpath.t (* target *)) list =
    match conf.target with
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
  in

  let (results
        : (Fpath.t (* rule file *) * test_result list * fixtest_result list)
          list) =
    plan
    |> List_.map (fun (rule_file, target) ->
           Logs.info (fun m -> m "processing rule file %s" !!rule_file);
           (* TODO? sanity check? call metachecker Check_rule.check()? *)
           match Parse_rule.parse_and_filter_invalid_rules rule_file with
           | Ok (rules, []) ->
               Logs.info (fun m -> m "processing target %s" !!target);
               let xlang =
                 xlang_for_rules_and_target !!rule_file rules target
               in
               let env = { rule_file; errors } in
               let res = run_rules_against_target xlang rules target in
               let (expected : (Test_annotation.t * linenb) list) =
                 Test_annotation.annotations target
               in
               let checks, fixtest_res =
                 compare_actual_to_expected
                   ~matching_diagnosis:conf.matching_diagnosis env target rules
                   res expected
               in
               (rule_file, checks, fixtest_res |> Option.to_list)
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
  in

  let res : Out.tests_result =
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
          !errors
          |> List_.filter_map (function
               | MissingTest rule_file -> Some rule_file
               | _else_ -> None)
          |> List.sort Fpath.compare;
        config_missing_fixtests =
          !errors
          |> List_.filter_map (function
               | MissingFixtest rule_file -> Some rule_file
               | _else_ -> None)
          |> List.sort Fpath.compare;
        (* TODO: rename to just 'errors' and put the missing_tests and missing
         * fixtests here as a kind of error.
         *)
        config_with_errors = [];
      }
  in
  (* pysemgrep is reporting some "successfully modified 1 file."
   * before the final report, but actually it reports that even on failing
   * fixtests, so better to not imitate for now.
   *)
  (* final report *)
  report_tests_result
    (caps :> < Cap.stdout >)
    ~matching_diagnosis:conf.matching_diagnosis ~json:conf.json res;
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
