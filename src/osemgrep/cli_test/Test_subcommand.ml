open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There is currently no 'semgrep test' subcommand. Tests are run via
 * 'semgrep scan --test ...' but internally it's quite similar to
 * a subcommand.
 *
 * For more info on how to use Semgrep rule testing infrastructure, see
 * https://semgrep.dev/docs/writing-rules/testing-rules/.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(* from test.py *)
let comment_syntaxes =
  [ ("#", "\n"); ("//", "\n"); ("<!--", "-->"); ("(*", "*)") ]

let remove_ending_comments rule =
  List.fold_left
    (fun rule (_, end_syntax) ->
      let rule = String.trim rule in
      if end_syntax = "\n" then rule
      else Str.global_replace (Str.regexp_string end_syntax) "" rule)
    rule comment_syntaxes

let normalize_rule_ids line =
  (* given a line like `     # ruleid:foobar`
     or `      // ruleid:foobar`
     or `      // ruleid:deepok:foobar`
     return `foobar` *)
  match String.split_on_char ':' (String.trim line) with
  | _ruleid_str :: fst :: rest ->
      let rule_text =
        String.concat ":"
          (* strip out "deepok" and "deepruleid" annotations if they are there to get rule name *)
          (if String.equal "deepok" fst || String.equal "deepruleid" fst then
             rest
           else fst :: rest)
      in
      String.split_on_char ',' (String.trim rule_text)
      (* remove comment ends for non-newline comment syntaxes *)
      |> List_.map remove_ending_comments
      |> List_.map String.trim |> Common2.StringSet.of_list
  | _ -> Common2.StringSet.empty

let annotations annotation =
  (* returns something like: {"#ruleid:", "# ruleid:", "//ruleid:", ...} *)
  List.concat_map
    (fun (fst, _) -> [ fst ^ annotation; fst ^ " " ^ annotation ])
    comment_syntaxes

let line_has_todo_rule line =
  annotations "todoruleid"
  |> List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let line_has_rule line =
  annotations "ruleid"
  |> List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let line_has_ok line =
  annotations "ok"
  |> List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let line_has_todo_ok line =
  annotations "todook"
  |> List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let add_line_to_dict_of_ruleids rule_ids line_dict effective_line_num
    test_file_resolved =
  (* given that 'todoruleid' or 'todook' or 'ok' or 'todo' are detected, add the line number
     flagged to the appropriate dictionary. *)
  let map =
    try Map_.find test_file_resolved line_dict with
    | Not_found -> Map_.empty
  in
  let map =
    Common2.StringSet.fold
      (fun rule_id map ->
        let els =
          try Map_.find rule_id map with
          | Not_found -> []
        in
        Map_.add rule_id (els @ [ effective_line_num ]) map)
      rule_ids map
  in
  Map_.add test_file_resolved map line_dict

let check_rule_id_mismatch reported_lines test_lines =
  (*reported_lines: Dict[str, Dict[str, List[int]]],
    test_lines: Dict[str, Set]*)
  (* checks whether there exists a #ruleid: <rule name> annotation where a rule matching <rule name> doesn't exist.
      leads to exit of 1 if there does exist such an occurence. *)
  let rule_id_mismatch =
    Map_.fold
      (fun file_path test_ids r ->
        let reported =
          try Map_.find file_path reported_lines with
          | Not_found -> Map_.empty
        in
        let reported_ids =
          Map_.fold
            (fun key _ acc -> Common2.StringSet.add key acc)
            reported Common2.StringSet.empty
        in
        if not (Common2.StringSet.equal test_ids reported_ids) then (
          let test_id_no_reported_ids =
            Common2.StringSet.diff test_ids reported_ids
          in
          Logs.err (fun m ->
              m
                "Found rule id mismatch - file=%s 'ruleid' annotation with no \
                 YAML rule=%a"
                file_path
                Fmt.(list ~sep:(any ", ") string)
                (Common2.StringSet.elements test_id_no_reported_ids));
          true)
        else r)
      test_lines false
  in
  if rule_id_mismatch then (
    Logs.err (fun m ->
        m
          "Failing due to rule id mismatch. There is a test denoted with \
           'ruleid: <rule name>' where the rule name does not exist or is not \
           expected in the test file.");
    exit 2 (* EXIT_FAILURE *))

module IS = Set.Make (Int)

let get_expected_and_reported_lines result test_files =
  (* Collects the expected lines (which are the lines annotated with '#ruleid')
     and the reported lines (which are the lines that the run of semgrep flagged on)
     Returns the 'matches' dictionary, which maps check_ids -> file_paths involved -> expected
     and reported line lists.

     Note: we need matches to map check_ids -> file paths because some rule_ids have two
      distinct test files (notably, for rules that work on both tsx and jsx) *)
  let ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines =
    List.fold_left
      (fun (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines) test_file ->
        let test_file_resolved = Rpath.of_fpath test_file |> Rpath.to_string in
        let all_lines = UFile.cat test_file in
        snd
        @@ List.fold_left
             (fun (i, (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines))
                  line ->
               (* +1 because we are 0 based and semgrep output is not, plus skip the comment line *)
               let effective_line_num = i + 2 in

               let rule_in_line = line_has_rule line
               and ok_in_line = line_has_ok line
               and todo_rule_in_line = line_has_todo_rule line
               and todo_ok_in_line = line_has_todo_ok line in

               let has_parseable_rule_id =
                 (rule_in_line || todo_rule_in_line || ok_in_line
                || todo_ok_in_line)
                 && String.contains line ':'
               in

               if not has_parseable_rule_id then
                 ( i + 1,
                   (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines) )
               else
                 let rule_ids = normalize_rule_ids line in
                 if Common2.StringSet.equal rule_ids Common2.StringSet.empty
                 then (
                   Logs.warn (fun m ->
                       m
                         "Could not parse %s as a test annotation in file %s. \
                          Skipping this line"
                         line test_file_resolved);
                   ( i + 1,
                     (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines)
                   ))
                 else
                   let ruleid_lines =
                     if todo_rule_in_line || rule_in_line then
                       add_line_to_dict_of_ruleids rule_ids ruleid_lines
                         effective_line_num test_file_resolved
                     else ruleid_lines
                   and ok_lines =
                     if todo_rule_in_line || ok_in_line then
                       add_line_to_dict_of_ruleids rule_ids ok_lines
                         effective_line_num test_file_resolved
                     else ok_lines
                   and todo_ok_lines =
                     if todo_ok_in_line then
                       add_line_to_dict_of_ruleids rule_ids todo_ok_lines
                         effective_line_num test_file_resolved
                     else todo_ok_lines
                   and todo_ruleid_lines =
                     if todo_rule_in_line then
                       add_line_to_dict_of_ruleids rule_ids todo_ruleid_lines
                         effective_line_num test_file_resolved
                     else todo_ruleid_lines
                   in
                   ( i + 1,
                     (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines)
                   ))
             (0, (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines))
             all_lines)
      (Map_.empty, Map_.empty, Map_.empty, Map_.empty)
      test_files
  in
  let reported_lines =
    result.Core_result.matches_with_fixes
    |> List.fold_left
         (fun reported_lines (result, _textedit) ->
           let path = Unix.realpath !!(result.Pattern_match.file)
           and check_id = Rule_ID.to_string result.rule_id.id
           and start_line = (fst result.range_loc).pos.line in
           let path_map =
             try Map_.find path reported_lines with
             | Not_found -> Map_.empty
           in
           let check_ids =
             try Map_.find check_id path_map with
             | Not_found -> []
           in
           let check_ids' =
             Map_.add check_id (check_ids @ [ start_line ]) path_map
           in
           Map_.add path check_ids' reported_lines)
         Map_.empty
  in

  let test_lines : (string, _) Map_.t =
    let add_ids =
      Map_.fold (fun file_name annotations map ->
          let keys =
            Map_.fold
              (fun k _ acc -> Common2.StringSet.add k acc)
              annotations Common2.StringSet.empty
          in
          let k =
            try Map_.find file_name map with
            | Not_found -> Common2.StringSet.empty
          in
          Map_.add file_name (Common2.StringSet.union k keys) map)
    in
    add_ids ruleid_lines (add_ids ok_lines Map_.empty)
  in

  check_rule_id_mismatch reported_lines test_lines;

  let file_paths =
    let add_paths = Map_.fold (fun k _ acc -> Common2.StringSet.add k acc) in
    add_paths ruleid_lines (add_paths reported_lines Common2.StringSet.empty)
  in

  Common2.StringSet.fold
    (fun file_path matches_by_check_id ->
      let check_ids =
        let add_check_ids =
          Map_.fold (fun rule_id _ acc -> Common2.StringSet.add rule_id acc)
        in
        add_check_ids
          (try Map_.find file_path ruleid_lines with
          | Not_found -> Map_.empty)
          (add_check_ids
             (try Map_.find file_path reported_lines with
             | Not_found -> Map_.empty)
             Common2.StringSet.empty)
      in
      Common2.StringSet.fold
        (fun check_id matches_by_check_id ->
          let all_reported =
            IS.of_list (Map_.find check_id (Map_.find file_path reported_lines))
          and expected =
            IS.of_list (Map_.find check_id (Map_.find file_path ruleid_lines))
          and todo_oked =
            IS.of_list (Map_.find check_id (Map_.find file_path todo_ok_lines))
          and todo_ruleid =
            IS.of_list
              (Map_.find check_id (Map_.find file_path todo_ruleid_lines))
          in
          let reported = IS.diff (IS.diff all_reported todo_oked) todo_ruleid
          and expected = IS.diff (IS.diff expected todo_ruleid) todo_oked in
          let v =
            ( ("expected_lines", IS.elements expected),
              ("reported_lines", IS.elements reported) )
          in
          let check_id_map =
            try Map_.find check_id matches_by_check_id with
            | Not_found -> Map_.empty
          in
          let check_id_map' = Map_.add file_path v check_id_map in
          Map_.add check_id check_id_map' matches_by_check_id)
        check_ids matches_by_check_id)
    file_paths Map_.empty

let generate_check_output_line check_id matches soft_errors =
  let json_error_report =
    if soft_errors <> [] then
      (* Display partial parsing errors and such. These are tolerated
         when running a semgrep scan but fatal in test mode. *)
      [ JSON.string_of_json (JSON.Object [ ("errors", JSON.Array []) ]) ]
      (* TODO: print errors (List_.map (fun e -> ... *)
    else []
  in

  let generate_missed_vs_incorrect_lines ((_, expected), (_, reported)) =
    let exp_set = IS.of_list expected and rep_set = IS.of_list reported in
    let missed = IS.diff exp_set rep_set
    and incorrect = IS.diff rep_set exp_set in
    Printf.sprintf "missed lines: %s, incorrect lines: %s"
      (String.concat ", " (List_.map string_of_int (IS.elements missed)))
      (String.concat ", " (List_.map string_of_int (IS.elements incorrect)))
  in
  let missed_vs_incorrect_lines =
    Map_.fold
      (fun _filename exp_rep acc ->
        generate_missed_vs_incorrect_lines exp_rep :: acc)
      matches []
  in
  let all_errors =
    String.concat "\n\t" (json_error_report @ missed_vs_incorrect_lines)
  in

  let test_file_names =
    Map_.fold (fun k _ acc -> k :: acc) matches [] |> String.concat " "
  in
  Printf.sprintf "\t✖ %s\n\t%s\n\ttest file path: %s\n\n" check_id all_errors
    test_file_names

let checkid_passed matches_for_checkid =
  Map_.fold
    (fun _filename ((_, expected), (_, reported)) acc ->
      acc && IS.(equal (of_list expected) (of_list reported)))
    matches_for_checkid true

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* TODO: reuse Rule_tests.ml *)
let get_config_filenames _targets = failwith "TODO"

(* TODO: reuse Rule_tests.ml *)
let get_config_test_filenames _a _b = failwith "TODO"

let _run_conf_TODO (conf : Test_CLI.conf) : Exit_code.t =
  let config_filenames = get_config_filenames conf.target in
  let config_test_filenames =
    get_config_test_filenames conf.target config_filenames
  in

  (* TODO
     # Extract the subset of tests using autofix.
     # They'll be run later again, with autofix on (why run them twice?).
     config_fixtest_filenames: Dict[
         Path, List[Tuple[Path, Path]]
     ] = get_config_fixtest_filenames(target, config_test_filenames)
  *)
  let config_with_tests, config_without_tests =
    Map_.fold
      (fun k v (with_test, without_test) ->
        if v = [] then (with_test, k :: without_test)
        else (k :: with_test, without_test))
      config_test_filenames ([], [])
  in

  let config_missing_tests_output = config_without_tests in

  (* let scan_func_for_osemgrep =
       Core_runner.mk_scan_func_for_osemgrep Core_scan.scan_with_exn_handler
     in
     let scan_func =
       scan_func_for_osemgrep
         ~respect_git_ignore:conf.targeting_conf.respect_git_ignore
         ~file_match_results_hook conf.core_runner_conf rules errors targets
     in
     let (res : Core_runner.result) =
       Core_runner.create_core_result filtered_rules scan_func
     in
  *)
  let (results : (Fpath.t * Core_result.result_or_exn) list) =
    Obj.magic config_with_tests (* TODO *)
  in

  (* invoke_semgrep_fn = functools.partial(
         invoke_semgrep_multi,
         engine_type=engine_type,
         no_git_ignore=True,
         no_rewrite_rule_ids=True,
         strict=strict,
         optimizations=optimizations,
       )
     with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
            results = pool.starmap(invoke_semgrep_fn, config_with_tests)
  *)
  let config_with_errors, config_without_errors =
    List.partition_map
      (function
        | f, Ok y -> Right (f, y)
        | f, Error e -> Left (f, e))
      results
  in
  let config_with_errors_output =
    List_.map
      (fun (filename, (exn, _error_opt)) ->
        ( filename,
          Printexc.to_string (Exception.get_exn exn),
          Exception.to_string exn ))
      config_with_errors
  in

  let tested =
    List_.map
      (fun (filename, result) ->
        ( filename,
          get_expected_and_reported_lines result
            (Map_.find filename config_test_filenames),
          result.Core_result.errors ))
      config_without_errors
  in

  let results_output =
    List_.map
      (fun (filename, matches, errors) ->
        ( filename,
          Map_.fold
            (fun check_id filename_and_matches acc ->
              let passed = checkid_passed filename_and_matches && errors = []
              and matches = filename_and_matches in
              Map_.add check_id (passed, matches, errors) acc)
            matches Map_.empty ))
      tested
  in

  (* TODO
      fixtest_filenames: Dict[Path, List[Tuple[Path, Path]]] = {
          config: [
              (target, fixtest)
              for target, fixtest in testfiles
              # if os.path.abspath(target) in passed_test_filenames
          ]
          for config, testfiles in config_fixtest_filenames.items()
      }

      # List[Tuple[Path, List[Tuple[Path,Path]]]]
      config_with_fixtests, config_without_fixtests = partition(
          fixtest_filenames.items(), lambda c: bool(c[1])
     )

      configs_missing_fixtests = [
          str(c) for c, _fixtest in config_without_fixtests if config_contains_fix_key(c)
     ]
  *)

  (* # this saves execution time: fix will not be correct, if regular test is not correct *)
  (*let passed_test_filenames =
    List.fold_left (fun acc (_config_filename, matches, soft_errors) ->
        Map_.fold (fun _checkid filename_and_matches acc ->
            Map_.fold (fun filename (expected, reported) acc ->
                if expected = reported && soft_errors = [] then
                  filename :: acc
                else
                  acc)
              filename_and_matches acc)
          matches acc)
      [] tested
    in *)
  (* configs_with_fixtests = {
         config: [
             (target, fixtest)
             for target, fixtest in testfiles
             if os.path.abspath(target) in passed_test_filenames
         ]
         for config, testfiles in config_with_fixtests
     }

     temp_copies: Dict[Path, str] = {
         target: create_temporary_copy(target)
         for _config, testfiles in config_with_fixtests
         for target, _fixtest in testfiles
     }

     config_with_tempfiles = [
         (config, [temp_copies[target] for target, _fixtest in testfiles])
         for config, testfiles in config_with_fixtests
     ]

     # This is the invocation of semgrep for testing autofix.
     #
     # TODO: should 'engine' be set to 'engine=engine' or always 'engine=EngineType.OSS'?
     invoke_semgrep_with_autofix_fn = functools.partial(
         invoke_semgrep_multi,
         no_git_ignore=True,
         no_rewrite_rule_ids=True,
         strict=strict,
         optimizations=optimizations,
         autofix=True,
       )

     with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
         results = pool.starmap(invoke_semgrep_with_autofix_fn, config_with_tempfiles)

     fixtest_comparisons = {
         temp_copies[target]: fixtest
         for _config, testfiles in config_with_fixtests
         for target, fixtest in testfiles
     }

     fixtest_results: Dict[Path, Tuple[List[str], Path]] = {}
     fixtest_results_output = {}
     for t, tempcopy in temp_copies.items():
         fixtest = fixtest_comparisons[tempcopy]
         filediff = fixed_file_comparison(fixtest, tempcopy)
         # fixtest_results[t] = {"filediff": filediff, "fixtest": fixtest}
         fixtest_results[t] = (filediff, fixtest)
         fixtest_results_output[str(t)] = {"passed": len(filediff) = 0}
         os.remove(tempcopy)
  *)
  let output =
    JSON.Object
      [
        ( "config_missing_tests",
          JSON.Array
            (List_.map
               (fun f -> JSON.String (Fpath.to_string f))
               config_missing_tests_output) );
        ("config_missing_fixtests", JSON.Array []);
        (* configs_missing_fixtests ; *)
        ( "config_with_errors",
          JSON.Array
            (List_.map
               (fun (file, exn_print, exn) ->
                 JSON.Object
                   [
                     ("filename", JSON.String (Fpath.to_string file));
                     ("error", JSON.String exn);
                     ("output", JSON.String exn_print);
                   ])
               config_with_errors_output) );
        ( "results",
          JSON.Object
            (List_.map
               (fun (filename, matches_and_errors) ->
                 let obj =
                   (* TODO output:
                      "checks": { check_id: { "passed" : <BOOL>, "matches": object, "errors": <list> } }
                   *)
                   Map_.fold
                     (fun check_id _filename_and_matches acc ->
                       (check_id, JSON.Null) :: acc)
                     matches_and_errors []
                 in
                 (Fpath.to_string filename, JSON.Object obj))
               results_output) );
        ("fixtest_results", JSON.Object [] (* fixtest_results_output ; *));
      ]
  in
  let strict_error = config_with_errors_output <> [] && conf.strict in
  let any_failures =
    List.exists
      (fun (_, result) ->
        Map_.fold (fun _ (passed, _, _) acc -> acc && passed) result true)
      results_output
  in

  (* any_fixtest_failures = any(
       not fixtest_file_results["passed"]
       for fixtest_file_results in fixtest_results_output.values()
     ) *)
  let exit_code =
    if strict_error && any_failures then Exit_code.fatal else Exit_code.ok
  in

  (* int(strict_error or any_failures or any_fixtest_failures) *)
  if conf.json then (
    print_endline (JSON.string_of_json output);
    exit_code)
  else
    let num_tests, num_tests_passed, check_output_lines =
      List.fold_left
        (fun (num_tests, num_tests_passed, check_output_lines) (_filename, rr) ->
          Map_.fold
            (fun check_id (passed, matches, errors)
                 (num_tests, num_tests_passed, check_output_lines) ->
              let num_tests = num_tests + 1
              and num_tests_passed, check_output_lines =
                if not passed then
                  ( num_tests_passed,
                    check_output_lines
                    ^ generate_check_output_line check_id matches errors )
                else (num_tests_passed + 1, check_output_lines)
              in
              (num_tests, num_tests_passed, check_output_lines))
            rr
            (num_tests, num_tests_passed, check_output_lines))
        (0, 0, "") results_output
    in

    (* num_fixtests = 0
       num_fixtests_passed = 0
       fixtest_file_diffs: str = ""
       for target_filename, f_results in fixtest_results.items():
           num_fixtests += 1
           filediff = f_results[0]
           fixtest = f_results[1]
           if len(filediff) > 0:
               fixtest_file_diffs += _generate_fixcheck_output_line(
                   target_filename, filediff, fixtest
               )
           else:
               num_fixtests_passed += 1 *)
    if num_tests = 0 then
      Logs.app (fun m ->
          m
            "No unit tests found. See \
             https://semgrep.dev/docs/writing-rules/testing-rules")
    else if num_tests = num_tests_passed then
      Logs.app (fun m ->
          m "%u/%u: ✓ All tests passed " num_tests_passed num_tests)
    else
      Logs.app (fun m ->
          m "%u/%u: %u unit tests did not pass:@.%s" num_tests_passed num_tests
            (num_tests - num_tests_passed)
            check_output_lines);

    (* if num_fixtests = 0:
           print("No tests for fixes found.")
       elif num_fixtests = num_fixtests_passed:
           print(f"{num_fixtests_passed}/{num_fixtests}: ✓ All fix tests passed ")
       else:
           print(
               f"{num_fixtests_passed}/{num_fixtests}: {num_fixtests - num_fixtests_passed} fix tests did not pass: "
           )
           print(BREAK_LINE)
           print(fixtest_file_diffs) *)
    if config_with_errors_output <> [] then
      Logs.app (fun m -> m "@.The following config files produced errors:");
    Logs.app (fun m -> m "");

    (* "\t"
       + "\n\t".join(
           f"{c['filename']}: {c['error']}" for c in config_with_errors_output
       )
         ); *)
    exit_code

(*****************************************************************************)
(* Pad's temporary version *)
(*****************************************************************************)
let run_conf (conf : Test_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:true ~level:conf.common.logging_level;
  (* Metrics_.configure Metrics_.On; *)
  Logs.debug (fun m -> m "conf = %s" (Test_CLI.show_conf conf));

  match conf.target with
  | Test_CLI.Dir (dir, None) ->
      let fail_callback _num _msg = () in
      let tests, total_mismatch =
        Test_engine.make_tests ~fail_callback [ dir ]
      in
      tests |> List.iter (fun (test : Alcotest_ext.test) -> test.func ());
      Logs.app (fun m -> m "total mismatch: %d" !total_mismatch);
      if !total_mismatch > 0 then Exit_code.fatal else Exit_code.ok
  | _else_ -> failwith "TODO2"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (argv : string array) : Exit_code.t =
  let conf = Test_CLI.parse_argv argv in
  run_conf conf
