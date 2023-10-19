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

type conf = {
  target : target_kind;
  ignore_todo : bool;
  (* TODO? do we need those options? people use the JSON output?
   * the playground? and the optimizations and strict?
   *)
  json : bool;
  (* take the whole core_runner_conf? like for validate? *)
  optimizations : bool;
  strict : bool;
}

(* alt: we could accept multiple dirs, and multiple files
 * TODO? should we restrict the config_str to File or Dir?
 *)
and target_kind =
  | Dir of Fpath.t * Rules_config.config_string option (* optional --config *)
  | File of Fpath.t * Rules_config.config_string (* mandatory --config *)
[@@deriving show]

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(* from util.py *)
let is_config_fixtest_suffix path =
  let any_ext =
    let base = Fpath.basename path in
    match String.split_on_char '.' base with
    | _ :: tl -> tl
    | [] -> []
  in
  List.mem "fixed" any_ext

let yml_extensions = [ ".yml" ; ".yaml" ]

let is_config_test_suffix path =
  let ext =
    let fst = Fpath.get_ext path in
    let snd = Fpath.(get_ext (rem_ext path)) in
    snd ^ fst
  in
  List.mem ext (List.map (fun e -> ".test" ^ e) yml_extensions) &&
  not (is_config_fixtest_suffix path)

let is_config_suffix path =
  List.mem (Fpath.get_ext path) yml_extensions &&
  not (is_config_test_suffix path)

let get_all_files path =
  let str = Fpath.to_string path in
  Sys.readdir str |> Array.to_list
  |> List.filter (fun f -> Sys.file_exists f && not (Sys.is_directory f))
  |> List.map (Fpath.add_seg path)

(* from test.py *)
let comment_syntaxes =
  [ ("#", "\n") ; ("//", "\n") ; ("<!--", "-->") ; ("(*", "*)") ]

let remove_ending_comments rule =
  List.fold_left (fun rule (_, end_syntax) ->
      let rule = String.trim rule in
      if end_syntax = "\n" then
        rule
      else
        Str.global_replace (Str.regexp_string end_syntax) "" rule)
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
         else
           fst :: rest)
    in
    String.split_on_char ',' (String.trim rule_text)
    (* remove comment ends for non-newline comment syntaxes *)
    |> List.map remove_ending_comments
    |> List.map String.trim
    |> Common2.StringSet.of_list
  | _ -> Common2.StringSet.empty

let annotations annotation =
  (* returns something like: {"#ruleid:", "# ruleid:", "//ruleid:", ...} *)
  List.flatten
    (List.map (fun (fst, _) ->
         [ fst ^ annotation ; fst ^ " " ^ annotation ])
        comment_syntaxes)

let line_has_todo_rule line =
  annotations "todoruleid" |>
  List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let line_has_rule line =
  annotations "ruleid" |>
  List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let line_has_ok line =
  annotations "ok" |>
  List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let line_has_todo_ok line =
  annotations "todook" |>
  List.exists (fun ann -> Str.string_match (Str.regexp_string ann) line 0)

let add_line_to_dict_of_ruleids rule_ids line_dict effective_line_num test_file_resolved =
    (* given that 'todoruleid' or 'todook' or 'ok' or 'todo' are detected, add the line number
       flagged to the appropriate dictionary. *)
    let map = try Map_.find test_file_resolved line_dict with Not_found -> Map_.empty in
    let map =
      Common2.StringSet.fold (fun rule_id map ->
          let els = try Map_.find rule_id map with Not_found -> [] in
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
    Map_.fold (fun file_path test_ids r ->
        let reported = try Map_.find file_path reported_lines with Not_found -> Map_.empty in
        let reported_ids = Map_.fold (fun key _ acc -> Common2.StringSet.add key acc) reported Common2.StringSet.empty in
        if not (Common2.StringSet.equal test_ids reported_ids) then begin
          let test_id_no_reported_ids = Common2.StringSet.diff test_ids reported_ids in
          Logs.err (fun m ->
              m "Found rule id mismatch - file=%s 'ruleid' annotation with no YAML rule=%a"
                file_path Fmt.(list ~sep:(any ", ") string) (Common2.StringSet.elements test_id_no_reported_ids));
          true
        end else
          r) test_lines false
  in
  if rule_id_mismatch then begin
    Logs.err (fun m -> m
                 "Failing due to rule id mismatch. There is a test denoted with 'ruleid: <rule name>' where the rule name does not exist or is not expected in the test file."
             );
    exit 2 (* EXIT_FAILURE *)
  end

module IS = Set.Make(Int)

let get_expected_and_reported_lines json_out test_files =
(*    json_out: Dict[str, Any], test_files: List[Path]
      ) -> Dict[str, Dict[str, Any]]: *)
  (* Collects the expected lines (which are the lines annotated with '#ruleid')
    and the reported lines (which are the lines that the run of semgrep flagged on)
    Returns the 'matches' dictionary, which maps check_ids -> file_paths involved -> expected
    and reported line lists.

    Note: we need matches to map check_ids -> file paths because some rule_ids have two
     distinct test files (notably, for rules that work on both tsx and jsx) *)

  let ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines =
    List.fold_left (
      fun
        (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines)
        test_file ->
        let test_file_resolved = Rpath.of_fpath test_file in
        let all_lines = File.cat test_file in
        snd @@
        List.fold_left (
          fun (i, (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines))
            line ->
            (* +1 because we are 0 based and semgrep output is not, plus skip the comment line *)
            let effective_line_num = i + 2 in

            let rule_in_line = line_has_rule line
            and ok_in_line = line_has_ok line
            and todo_rule_in_line = line_has_todo_rule line
            and todo_ok_in_line = line_has_todo_ok line
            in

            let has_parseable_rule_id = (
                rule_in_line || todo_rule_in_line || ok_in_line || todo_ok_in_line
              ) && String.contains line ':'
            in

            if not has_parseable_rule_id then
              (i + 1, (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines))
            else
              let rule_ids = normalize_rule_ids line in
              if Common2.StringSet.equal rule_ids Common2.StringSet.empty then (
                Logs.warn (fun m -> m
                              "Could not parse %s as a test annotation in file %s. Skipping this line" line (Rpath.to_string test_file_resolved));
                (i + 1, (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines))
              ) else
                let ruleid_lines =
                  if todo_rule_in_line || rule_in_line then
                    add_line_to_dict_of_ruleids
                      rule_ids ruleid_lines effective_line_num test_file_resolved
                  else
                    ruleid_lines
                and ok_lines =
                  if todo_rule_in_line || ok_in_line then
                    add_line_to_dict_of_ruleids rule_ids ok_lines effective_line_num test_file_resolved
                  else
                    ok_lines
                and todo_ok_lines =
                  if todo_ok_in_line then
                    add_line_to_dict_of_ruleids
                      rule_ids todo_ok_lines effective_line_num test_file_resolved
                  else
                    todo_ok_lines
                and todo_ruleid_lines =
                  if todo_rule_in_line then
                    add_line_to_dict_of_ruleids
                      rule_ids
                      todo_ruleid_lines
                      effective_line_num
                      test_file_resolved
                  else
                    todo_ruleid_lines
                in
                (i + 1, (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines)))
      (0, (ruleid_lines, ok_lines, todo_ok_lines, todo_ruleid_lines))
      all_lines)
      (Map_.empty, Map_.empty, Map_.empty, Map_.empty) test_files
  in
  let reported_lines =
    (try Map_.find "results" json_out with Not_found -> []) |>
    List.fold_left (fun reported_lines result ->
        let path = Unix.realpath (Map_.find "path" result)
        and check_id = Map_.find "check_id" result
        and start_line = Map_.find "line" (Map_.find "start" result) |> int_of_string
        in
        let path_map = try Map_.find path reported_lines with Not_found -> Map_.empty in
        let check_ids = try Map_.find check_id path_map with Not_found -> [] in
        let check_ids' = Map_.add check_id (check_ids @ [ start_line ]) path_map in
        Map_.add path check_ids' reported_lines)
      Map_.empty
  in

  let test_lines =
    let add_ids =
      Map_.fold (fun file_name annotations map ->
          let keys = Map_.fold (fun k _ acc -> Common2.StringSet.add k acc) annotations Common2.StringSet.empty in
          let k = try Map_.find file_name map with Not_found -> Common2.StringSet.empty in
          Map_.add file_name (Common2.StringSet.union k keys) map)
    in
    add_ids ruleid_lines (add_ids ok_lines Map_.empty)
  in

  check_rule_id_mismatch reported_lines test_lines;

  let file_paths =
    let add_paths =
      Map_.fold (fun k _ acc -> Common2.StringSet.add k acc)
    in
    add_paths ruleid_lines (add_paths reported_lines Common2.StringSet.empty)
  in


  Common2.StringSet.fold (fun file_path matches_by_check_id ->
      let check_ids =
        let add_check_ids =
          Map_.fold (fun rule_id _ acc -> Common2.StringSet.add rule_id acc)
        in
        add_check_ids (try Map_.find file_path ruleid_lines with Not_found -> Map_.empty)
          (add_check_ids (try Map_.find file_path reported_lines with Not_found -> Map_.empty)
               Common2.StringSet.empty)
      in
      Common2.StringSet.fold (fun check_id matches_by_check_id ->
          let all_reported =
            IS.of_list (Map_.find check_id (Map_.find file_path reported_lines))
          and expected =
            IS.of_list
              (Map_.find check_id (Map_.find file_path ruleid_lines))
          and todo_oked =
            IS.of_list
              (Map_.find check_id (Map_.find file_path todo_ok_lines))
          and todo_ruleid =
            IS.of_list
              (Map_.find check_id (Map_.find file_path todo_ruleid_lines))
          in
          let reported = IS.diff (IS.diff all_reported todo_oked) todo_ruleid
          and expected = IS.diff (IS.diff expected todo_ruleid) todo_oked
          in
          let v = (
            ("expected_lines", IS.elements expected),
            ("reported_lines", IS.elements reported)
          )
          in
          let check_id_map = try Map_.find check_id matches_by_check_id with Not_found -> Map_.empty in
          let check_id_map' = Map_.add file_path v check_id_map in
          Map_.add check_id check_id_map' matches_by_check_id)
        check_ids matches_by_check_id)
    file_paths Map_.empty

let relative_eq parent_target target parent_config config =
  let rel_to a par =
    match Fpath.find_prefix a par with
    | None -> a
    | Some pre -> Option.get (Fpath.rem_prefix pre a)
  in
  let rel1 = rel_to target parent_target
  and rel2 = rel_to config parent_config
  in
  Fpath.(equal (rem_ext ~multi:true rel1) (rem_ext ~multi:true rel2))

let get_config_filenames path =
  let does_not_start_with_dot p =
    not (String.starts_with ~prefix:"." (Fpath.basename p))
  in
  let str = Fpath.to_string path in
  if Sys.file_exists str then
    if Sys.is_directory str then
      get_all_files path
      |> List.filter is_config_suffix
      |> List.filter does_not_start_with_dot
      |> List.filter (fun path -> does_not_start_with_dot (Fpath.parent path))
    else
      [ path ]
  else
    []

let get_config_test_filenames original_config configs original_target =
  let is_file p =
    let s = Fpath.to_string p in
    Sys.file_exists s && not (Sys.is_directory s)
  in
  let cfg_is_file = is_file original_config
  and tgt_is_file = is_file original_target
  in
  if cfg_is_file && tgt_is_file then
    Map_.add original_config [ original_target ] Map_.empty
  else
    let targets =
      get_all_files (if tgt_is_file then Fpath.parent original_target else original_target)
    in
    let target_matches_config config target =
      let correct_suffix =
        (is_config_test_suffix target || not (is_config_suffix target))
        && not (is_config_fixtest_suffix target)
      in

      (tgt_is_file || relative_eq original_target target original_config config)
      && is_file target
      && correct_suffix
    in
    List.fold_left (fun m config ->
        let tgts = List.filter (target_matches_config config) targets in
        Map_.add config tgts m)
      Map_.empty configs

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run_conf (conf : conf) : Exit_code.t =
  let config_filenames = get_config_filenames conf.target in
  let config_test_filenames = get_config_test_filenames conf.target config_filenames in
  (* TODO
    # Extract the subset of tests using autofix.
    # They'll be run later again, with autofix on (why run them twice?).
    config_fixtest_filenames: Dict[
        Path, List[Tuple[Path, Path]]
    ] = get_config_fixtest_filenames(target, config_test_filenames)
  *)

  let config_with_tests, config_without_tests =
    Map_.fold (fun k v (with_test, without_test) ->
        if v = [] then
          with_test, k :: without_test
        else
          k :: with_test, without_test)
      config_test_filenames ([], [])
  in

  invoke_semgrep_fn = functools.partial(
    invoke_semgrep_multi,
    engine_type=engine_type,
    no_git_ignore=True,
    no_rewrite_rule_ids=True,
    strict=strict,
    optimizations=optimizations,
  )
with multiprocessing.Pool(multiprocessing.cpu_count()) as pool:
       results = pool.starmap(invoke_semgrep_fn, config_with_tests)

    config_with_errors, config_without_errors = partition(results, lambda r: bool(r[1]))
    config_with_errors_output = [
        {"filename": str(filename), "error": error, "output": output}
        for filename, error, output in config_with_errors
    ]

    tested = [
        (
            filename,
            get_expected_and_reported_lines(output, config_test_filenames[filename]),
            output["errors"] if "errors" in output else [],
        )
        for filename, _, output in config_without_errors
    ]

    results_output: Mapping[str, Mapping[str, Any]] = {
        str(filename): {
            "checks": {
                check_id: {
                    "passed": checkid_passed(filename_and_matches) and not errors,
                    "matches": filename_and_matches,
                    "errors": errors,
                }
                for check_id, filename_and_matches in matches.items()
            }
        }
        for filename, matches, errors in tested
    }

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

    # this saves execution time: fix will not be correct, if regular test is not correct
    passed_test_filenames = [
        filename
        for _config_filename, matches, soft_errors in tested
        for _check_id, filename_and_matches in matches.items()
        for filename, expected_and_reported_lines in filename_and_matches.items()
        if expected_and_reported_lines["expected_lines"]
        == expected_and_reported_lines["reported_lines"]
        and not soft_errors
    ]
(*    configs_with_fixtests = {
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
        fixtest_results_output[str(t)] = {"passed": len(filediff) == 0}
        os.remove(tempcopy)
*)
    output = {
        "config_missing_tests": config_missing_tests_output,
                                (* "config_missing_fixtests": configs_missing_fixtests, *)
        "config_with_errors": config_with_errors_output,
        "results": results_output,
                                (* "fixtest_results": fixtest_results_output, *)
    }

    strict_error = bool(config_with_errors_output) and strict
    any_failures = any(
        not check_results["passed"]
        for file_results in results_output.values()
        for check_results in file_results["checks"].values()
    )

(*    any_fixtest_failures = any(
        not fixtest_file_results["passed"]
        for fixtest_file_results in fixtest_results_output.values()
      ) *)

    exit_code = int(strict_error or any_failures or any_fixtest_failures)

    if json_output:
        print(json.dumps(output, indent=4, separators=(",", ": ")))
        sys.exit(exit_code)

    num_tests = 0
    num_tests_passed = 0
    check_output_lines: str = ""
    for _filename, rr in results_output.items():
        for check_id, check_results in sorted(rr["checks"].items()):
            num_tests += 1
            if not check_results["passed"]:
                check_output_lines += _generate_check_output_line(
                    check_id, check_results
                )
            else:
                num_tests_passed += 1

(*    num_fixtests = 0
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

    if num_tests == 0:
        print(
            "No unit tests found. See https://semgrep.dev/docs/writing-rules/testing-rules"
        )
    elif num_tests == num_tests_passed:
        print(f"{num_tests_passed}/{num_tests}: ✓ All tests passed ")
    else:
        print(
            f"{num_tests_passed}/{num_tests}: {num_tests - num_tests_passed} unit tests did not pass:"
        )
        print(BREAK_LINE)
        print(check_output_lines)

(*    if num_fixtests == 0:
        print("No tests for fixes found.")
    elif num_fixtests == num_fixtests_passed:
        print(f"{num_fixtests_passed}/{num_fixtests}: ✓ All fix tests passed ")
    else:
        print(
            f"{num_fixtests_passed}/{num_fixtests}: {num_fixtests - num_fixtests_passed} fix tests did not pass: "
        )
        print(BREAK_LINE)
        print(fixtest_file_diffs) *)

    if config_with_errors_output:
        print(BREAK_LINE)
        print("The following config files produced errors:")
        print(
            "\t"
            + "\n\t".join(
                f"{c['filename']}: {c['error']}" for c in config_with_errors_output
            )
        )

    sys.exit(exit_code)


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run (conf : conf) : Exit_code.t =
  run_conf conf
