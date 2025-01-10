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
module A = Test_annotation

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse a semgrep-test command, execute it and return an exit code.
 *
 * There are multiple ways to call `semgrep test` but the main one is to call
 * it with a directory as an argument as in `semgrep test semgrep-rules/ocaml`
 * which will run all the tests in the directory recursively.
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
(* = Cap.stdout + Core_scan.caps + Cap.tmp (for Deep_scan.caps)
 * (no need for Cap.network; the tested rules should be local)
 *)
type caps =
  < Cap.stdout ; Cap.fork ; Cap.time_limit ; Cap.memory_limit ; Cap.tmp >

(* Core_scan.caps | Deep_scan.caps *)
type scan_caps = < Cap.fork ; Cap.time_limit ; Cap.memory_limit ; Cap.tmp >

(* Rules and targets to test together.
 * Usually the target list contains just one file, but in some cases
 * one rule can be tested on multiple files such as a with a .js and a .ts
 * LATER: for interfile tests, it will be normal to have multiple targets.
 *)
type tests = (Fpath.t (* rule file *) * Fpath.t list (* targets *)) list

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

(* to avoid having functions with lots of parameters *)
type env = {
  (* currently processed rule and targets files *)
  rule_file : Fpath.t;
  target_files : Fpath.t list;
  engine : A.engine;
  (* use a ref so easy to store all the errors returned by different functions.
   * alt: get each functions returning different kind of errors
   *)
  errors : error list ref;
  conf : Test_CLI.conf;
}

(* TODO: move in core/ ? used in other files? was in constants.py in pysemgrep *)
let break_line =
  "--------------------------------------------------------------------------------"

(*****************************************************************************)
(* Pro hooks *)
(*****************************************************************************)

let hook_pro_init : (unit -> unit) ref =
  ref (fun () ->
      failwith "semgrep test --pro not available (need --install-semgrep-pro)")

let hook_pro_scan : (Core_scan.caps -> Core_scan.func) ref =
  ref (fun _caps _config ->
      failwith "semgrep test --pro not available (need --install-semgrep-pro)")

(* note that we run DeepScan with
 *  - force_interfile=true (no need for the interfile: true metadata in the rule)
 *  - experimental_languages=true (analyze with DeepScan also Ruby and a few
 *    other languages)
 *)
let hook_deep_scan :
    (scan_caps -> Core_scan_config.t -> Fpath.t -> Core_result.result_or_exn)
    ref =
  ref (fun _caps _config _root ->
      failwith "semgrep test --pro not available (need --install-semgrep-pro)")

(*****************************************************************************)
(* File targeting (the set of tests) *)
(*****************************************************************************)

(* TODO? Move to Rule_tests.ml? *)
let find_targets_for_rule (rule_file : Fpath.t) : Fpath.t list =
  let dir, base = Fpath.split_base rule_file in
  (* ex: "useless-if" (without the ".yaml") *)
  let base_no_ext = Fpath.rem_ext base in
  dir |> List_files.read_dir_entries_fpath
  |> List_.exclude (fun p ->
         Fpath.equal p base || List.mem "fixed" (Fpath_.exts p))
  |> List_.filter_map (fun p ->
         (* the ~multi:true should then handle the foo.test.yaml *)
         if Fpath.equal (Fpath.rem_ext ~multi:true p) base_no_ext then
           Some (dir // p)
         else None)

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
             match find_targets_for_rule rule_file with
             | [] ->
                 (* stricter: (but reported via config_missing_tests in JSON)*)
                 Logs.warn (fun m ->
                     m "could not find target for %s" !!rule_file);
                 Stack_.push (MissingTest rule_file) errors;
                 None
             | xs ->
                 Logs.debug (fun m ->
                     m "found targets for %s: %s" !!rule_file
                       (xs |> List_.map Fpath.to_string |> String.concat ", "));
                 Some (rule_file, xs))
  | Test_CLI.File (target, config_str) -> (
      match Rules_config.parse_config_string ~in_docker:false config_str with
      | File rule_file -> [ (rule_file, [ target ]) ]
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

(* TODO? Move to Rule_tests.ml? *)
let fixtest_of_target_opt (target : Fpath.t) : Fpath.t option =
  let stem, ext = Fpath_.split_ext ~multi:true target in
  let fixtest = stem |> Fpath.add_ext (".fixed" ^ ext) in
  if Sys.file_exists !!fixtest then Some fixtest else None

(* TODO use capability and cleanup Test_parsing.ml and remove
 * Common2.unix_diff
 *)
let unix_diff (str1 : string) (str2 : string) : string list =
  UTmp.with_temp_file ~contents:str1 ~suffix:".x" (fun file1 ->
      UTmp.with_temp_file ~contents:str2 ~suffix:".x" (fun file2 ->
          Common2.unix_diff !!file1 !!file2))

let fixtest_result_for_target (_env : env) (target : Fpath.t)
    (fixtest_target : Fpath.t) (pms : Core_match.t list) : fixtest_result =
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
 *       update: Core_scan_config.t is now simpler and smaller
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

let core_scan_config (conf : Test_CLI.conf) (rules : Rule.t list)
    (targets : Target.t list) : Core_scan_config.t =
  {
    Core_scan_config.default with
    rule_source = Rules rules;
    target_source = Targets targets;
    output_format = NoOutput;
    (* activate matching explanations for Diagnosis to work *)
    matching_explanations = conf.matching_diagnosis;
    (* try to be as close as possible as a real scan to avoid differences
     * between semgrep test and semgrep scan behavior
     *)
    filter_irrelevant_rules = true;
    (* in a test context, we don't want to honor the paths: (include/exclude)
     * directive since the test target file, which must have the same
     * basename without the extension than the rule, may not match the
     * paths: directive of the rule
     *)
    respect_rule_paths = false;
  }

let run_rules_against_targets_for_engine caps (env : env) (rules : Rule.t list)
    (targets : Target.t list) : Core_result.t =
  (* old:
   * let xtarget = Test_engine.xtarget_of_file analyzer target in
   * let xconf = { Match_env.default_xconfig with matching_explanations = true}in
   * Match_rules.check ~match_hook:(fun _ ->()) ~timeout:None xconf rules xtarget
   *)
  let config = core_scan_config env.conf rules targets in
  let res_or_exn : Core_result.result_or_exn =
    match env.engine with
    | A.OSS -> Core_scan.scan caps config
    | A.Pro -> !hook_pro_scan (caps :> Core_scan.caps) config
    | A.Deep ->
        (* LATER: support also interfile tests where many targets are in
         * a subdir (using the same name than the rule file)
         *)
        let root, _base = Fpath.split_base env.rule_file in
        (* Deep_scan.caps but can't reference it from OSS/ *)
        !hook_deep_scan (caps :> scan_caps) config root
  in
  match res_or_exn with
  | Error exn -> Exception.reraise exn
  (* TODO? fail early or add a kind of error in the json output?
     | Ok { errors = _x::_; _} -> failwith "TODO"
  *)
  | Ok res -> res

(* Annotations have also a few implicit rules to avoid having to annotate
 * too much:
 *  - ruleid => proruleid => deepruleid,
 *    so if you have a ruleid:, no need to annotate too with proruleid:
 *    or deepruleid:, it is implicit. However if some ruleid: are actually
 *    not found by ProScan or DeepScan you'll need to add further annotations
 *    such as prook: or deeptodoruleid:
 *  - prook => deepok, because if the intrafile engine is able to detect
 *    some FPs compared to CoreScan, then the interfile engine should too.
 *    If it's not the case it's probably a bug in DeepScan (probably a
 *    naming bug as DeepScan use Naming_SAST.ml instead of Naming_AST.ml)
 *    in which case it's good to annotate it with a deeptodook
 *)
let filter_annots_for_engine (running_engine : A.engine)
    (annots : A.annotations) : A.annotations =
  annots
  |> List.filter (fun (A.{ kind; engine; others; _ }, _) ->
         match (running_engine, kind, engine) with
         | Pro, _, _ when List.mem (A.Ok, A.Pro) others -> false
         | Pro, _, _ when List.mem (A.Todoruleid, A.Pro) others -> false
         | Deep, _, _ when List.mem (A.Ok, A.Deep) others -> false
         | Deep, _, _ when List.mem (A.Todoruleid, A.Deep) others -> false
         (* prook => deepok (TODO? protodoruleid => deeptodoruleid? *)
         | Deep, _, _ when List.mem (A.Ok, A.Pro) others -> false
         (* ruleid => proruleid => deepruleid
          * (and todok => protodok => deeptodook)
          *)
         | OSS, (Ruleid | Todook), OSS -> true
         | Pro, (Ruleid | Todook), (OSS | Pro) -> true
         | Deep, (Ruleid | Todook), (OSS | Pro | Deep) -> true
         | _ -> false)

(*****************************************************************************)
(* Comparing *)
(*****************************************************************************)

let diff_findings (actual : int list) (expected : int list) : string =
  let _common, only_in_expected, only_in_actual =
    Common2.diff_set_eff expected actual
  in
  (if List_.null only_in_expected then ""
   else
     spf "missing findings lines %s."
       (only_in_expected |> List_.map Int.to_string |> String.concat ", "))
  ^
  if List_.null only_in_actual then ""
  else
    spf "unexpected findings lines %s."
      (only_in_actual |> List_.map Int.to_string |> String.concat ", ")

(* alt: use Test_compare_matches.compare_actual_to_expected but
 * it does not handle the actual rule id in the annotations and is
 * not compatible with what 'pysemgrep test' was doing when comparing.
 *)
let compare_actual_to_expected (env : env) (matches : Core_match.t list)
    (annots : (Fpath.t * A.annotations) list)
    (explanations : Matching_explanation.t list option) : test_result list =
  let xtra =
    match env.engine with
    | OSS -> ""
    | Pro -> " (with pro engine)"
    | Deep -> " (with deep engine)"
  in
  (* actual matches *)
  let matches_by_ruleid_and_file :
      (Rule_ID.t, (Fpath.t, Core_match.t list) Assoc.t) Assoc.t =
    if List_.null matches then
      (* stricter: *)
      Logs.warn (fun m -> m "nothing matched for %s%s" !!(env.rule_file) xtra);
    matches
    |> Assoc.group_by (fun (pm : Core_match.t) -> pm.rule_id.id)
    |> List_.map (fun (rule_id, pms) ->
           ( rule_id,
             pms
             |> Assoc.group_by (fun (pm : Core_match.t) ->
                    (* We need Fpath.normalize because for unclear reasons DeepScan
                     * returns matches with paths that may differ from
                     * the one below in the annotations so simpler to normalize
                     * both so path like ./foo/bar.c and foo/bar.c are considered
                     * the same
                     * TODO: we don't need that for CoreScan and ProScan so we
                     * should probably fix DeepScan instead to not mess up
                     * with the Targets paths.
                     *)
                    Fpath.normalize pm.path.internal_path_to_content) ))
  in
  (* expected matches *)
  let expected_by_ruleid_and_file :
      (Rule_ID.t, (Fpath.t, A.linenb list) Assoc.t) Assoc.t =
    let h = Hashtbl.create 101 in
    annots
    |> List.iter (fun (file, annotations) ->
           let file = Fpath.normalize file in
           let expected_by_rule_id : (Rule_ID.t, A.linenb list) Assoc.t =
             A.group_by_rule_id annotations
           in
           expected_by_rule_id
           |> List.iter (fun (rule_id, lines) ->
                  Hashtbl_.push h rule_id (file, lines)));
    h |> Hashtbl_.map (fun _k vref -> !vref) |> Hashtbl_.hash_to_list
  in

  let all_rule_ids : Rule_ID.t list =
    Assoc.join_keys matches_by_ruleid_and_file expected_by_ruleid_and_file
  in
  (* regular ruleid tests *)
  let checks : (Rule_ID.t * Out.rule_result) list =
    all_rule_ids
    |> List_.map (fun (id : Rule_ID.t) ->
           let actual : (Fpath.t, Core_match.t list) Assoc.t =
             matches_by_ruleid_and_file |> Assoc.find_opt id
             |> List_.optlist_to_list
           in
           let expected : (Fpath.t, A.linenb list) Assoc.t =
             expected_by_ruleid_and_file |> Assoc.find_opt id
             |> List_.optlist_to_list
           in
           let all_files : Fpath.t list = Assoc.join_keys actual expected in
           let res : (bool * (Fpath.t * Out.expected_reported)) list =
             all_files
             |> List_.map (fun (target : Fpath.t) ->
                    let matches : Core_match.t list =
                      actual |> Assoc.find_opt target |> List_.optlist_to_list
                    in
                    let (reported_lines : A.linenb list) =
                      matches
                      |> List_.map (fun (pm : Core_match.t) ->
                             pm.range_loc |> fst |> fun (loc : Loc.t) ->
                             loc.pos.line)
                      |> List.sort_uniq Int.compare
                    in
                    let expected_lines : A.linenb list =
                      expected |> Assoc.find_opt target |> List_.optlist_to_list
                      |> List.sort_uniq Int.compare
                    in
                    let passed = reported_lines =*= expected_lines in
                    if not passed then
                      Logs.err (fun m ->
                          m "test failed for rule id %s on target %s%s (%s)"
                            (Rule_ID.to_string id) !!target xtra
                            (diff_findings reported_lines expected_lines));
                    (* TODO: not sure why pysemgrep does not report the real
                     * reported_lines (and expected_lines) and filter those
                     * 'todook:'. It's actually very confusing because sometimes
                     * a test will be marked as 'passed: false' even though
                     * in the --json the reported and expected match, but because
                     * there are some hidden differences with the matching of
                     * todook:.
                     *)
                    let file_annots =
                      Assoc.find_opt target annots |> List_.optlist_to_list
                    in
                    let expected_reported =
                      let reported_lines =
                        A.filter_todook file_annots reported_lines
                      in
                      let expected_lines =
                        A.filter_todook file_annots expected_lines
                      in
                      { Out.reported_lines; expected_lines }
                    in
                    (passed, (target, expected_reported)))
           in
           let diagnosis =
             let* explanations = explanations in
             match res with
             | [ (_passed, (target, expected_reported)) ] ->
                 Some
                   (Diagnosis.diagnose ~target ~rule_file:env.rule_file
                      expected_reported explanations)
             | _ -> failwith "diagnosis for multiple targets not supported"
           in
           let (rule_result : Out.rule_result) =
             Out.
               {
                 passed = res |> List_.map fst |> Common2.and_list;
                 matches =
                   res
                   |> List_.map (fun (_passed, (target, expected_reported)) ->
                          (* TODO: not sure why but pysemgrep uses realpaths
                           * here, which is a bit annoying because it forces
                           * us to use masks in test snapshots
                           *)
                          let filename = Unix.realpath !!target in
                          (filename, expected_reported));
                 (* TODO: error from the engine ? *)
                 errors = [];
                 diagnosis;
               }
           in
           (id, rule_result))
  in
  checks

let compare_for_autofix (env : env) (rules : Rule.t list)
    (matches : Core_match.t list) : fixtest_result list =
  env.target_files
  |> List_.filter_map (fun target ->
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
             let matches =
               matches
               |> List.filter (fun (pm : Core_match.t) ->
                      Fpath.equal pm.path.internal_path_to_content target)
             in
             Some (fixtest_result_for_target env target fixtest_target matches))

(*****************************************************************************)
(* Run the tests *)
(*****************************************************************************)

(* alt: call it run_env? *)
let run_engine (caps : < scan_caps ; .. >) (env : env) (rules : Rule.t list)
    (targets : Target.t list)
    (files_and_annots : (Fpath.t * A.annotations) list) :
    test_result list * fixtest_result list =
  let res : Core_result.t =
    run_rules_against_targets_for_engine caps env rules targets
  in
  let expected : (Fpath.t * A.annotations) list =
    files_and_annots
    |> List_.map (fun (file, annots) ->
           let annots = filter_annots_for_engine env.engine annots in
           (file, annots))
  in
  let matches =
    res.processed_matches
    |> List_.map (fun (x : Core_result.processed_match) -> x.pm)
  in
  let checks =
    compare_actual_to_expected env matches expected res.explanations
  in
  let fixtest =
    (* optional fixtest *)
    match env.engine with
    | OSS -> compare_for_autofix env rules matches
    (* TODO? we do not run the autofix for ProScan and DeepScan because
     * the matches might differ which could require some .pro.fixed
     * (or .deep.fixed) to separate them from the OSS one.
     * TODO? should we fix semgrep-rules-pro/paid/python/ which have weird
     * fixtest results when using --pro?
     *)
    | Pro
    | Deep ->
        []
  in
  (checks, fixtest)

(* run one test using the different engines if --pro *)
let run_test (caps : < scan_caps ; .. >) (conf : Test_CLI.conf)
    (rule_file : Fpath.t) (rules : Rule.t list) (target_files : Fpath.t list)
    (errors : error list ref) : test_result list * fixtest_result list =
  (* note that even one target file can result in different targets
   * if the rules contain multiple analyzers.
   *)
  let targets : Target.t list =
    Core_runner.targets_for_files_and_rules target_files rules
  in
  let files_and_annots : (Fpath.t * A.annotations) list =
    target_files |> List_.map (fun file -> (file, A.annotations file))
  in

  let env = { rule_file; target_files; engine = A.OSS; conf; errors } in
  let checks_oss, fixtest_oss =
    run_engine caps env rules targets files_and_annots
  in
  (* When conf.pro, we should run the engine 3 times:
   *  - with Core_scan and check just the ruleid:
   *  - with Pro_scan and check also the proruleid:
   *  - with Deep_scan and check also the deepruleid:.
   * The json will give information about those 3 different runs by using
   * different rule IDs suffix (e.g., "myrule--PRO")
   *)
  if conf.pro then
    (* note that we do not run fixtest for Pro because that would require
     * sometimes have a separate .pro.fixed as the matches are different
     * TODO? have those .pro.fixed? actually when I run on semgrep-rules-pro
     * the fixtest seems bad for some python tests
     *)
    let checks_pro, _ =
      let env = { env with engine = A.Pro } in
      run_engine caps env rules targets files_and_annots
    in
    (* adjust rule id to have the --PRO suffix so the different matching results
     * are visible in the JSON
     * alt: add some extra_checks: next to checks: in the JSON
     *)
    let checks_pro =
      checks_pro
      |> List_.map (fun (id, rule_result) ->
             let s = Rule_ID.to_string id in
             let id = Rule_ID.of_string_exn (s ^ "--PRO") in
             (id, rule_result))
    in
    (* same for deep *)
    let checks_deep, _ =
      let env = { env with engine = A.Deep } in
      run_engine caps env rules targets files_and_annots
    in
    let checks_deep =
      checks_deep
      |> List_.map (fun (id, rule_result) ->
             let s = Rule_ID.to_string id in
             let id = Rule_ID.of_string_exn (s ^ "--DEEP") in
             (id, rule_result))
    in
    (checks_oss @ checks_pro @ checks_deep, fixtest_oss)
  else (checks_oss, fixtest_oss)

let run_tests (caps : < scan_caps ; .. >) (conf : Test_CLI.conf) (tests : tests)
    (errors : error list ref) :
    (Fpath.t (* rule file *) * test_result list * fixtest_result list) list =
  (* LATER: in theory we could use Parmap here *)
  tests
  |> List_.map (fun (rule_file, target_files) ->
         Logs.info (fun m -> m "processing rule file %s" !!rule_file);
         (* TODO? sanity check? call metachecker Check_rule.check()? *)
         match Parse_rule.parse_and_filter_invalid_rules rule_file with
         | Ok (rules, []) ->
             Logs.info (fun m ->
                 m "processing target(s) %s"
                   (target_files |> Fpath_.to_strings |> String.concat ", "));
             let checks, fixtest =
               run_test caps conf rule_file rules target_files errors
             in
             (rule_file, checks, fixtest)
         (* capture 's' and return it in the error so the user will see something
          * like "Missing semgrep extenstion needed for parsing X. Try --pro"
          *)
         | Ok (_, (MissingPlugin s, _, _) :: _)
         | Error { kind = InvalidRule (MissingPlugin s, _, _); _ } ->
             (* alt: could Stack_.push (MissingPlugin rule_file) errors *)
             raise
               (Error.Semgrep_error (s, Some (Exit_code.missing_config ~__LOC__)))
         | Ok (_, _ :: _)
         | Error _ ->
             (* alt: use List_.filter_map above and be more fault tolerant
              * with a Stack_.push (UnparsableRule rule_file) errors;
              * but simpler to raise errors early for now
              *)
             raise
               (Error.Semgrep_error
                  ( spf "invalid configuration found in %s" !!rule_file,
                    Some (Exit_code.missing_config ~__LOC__) ))
         | (exception Parsing_error.Syntax_error _)
         | (exception Parsing_error.Other_error _) ->
             failwith "impossible: Parse_rule should not raise exns anymore")

(*****************************************************************************)
(* Run the conf *)
(*****************************************************************************)
let run_conf (caps : < caps ; .. >) (conf : Test_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:true ~level:conf.common.logging_level;
  (* Metrics_.configure Metrics_.On; ?? and allow to disable it?
   * semgrep-rules/Makefile is running semgrep --test with metrics=off
   * (and also --disable-version-check), but maybe because it is used from
   * 'semgrep scan'; in 'osemgrep test' context, we should not even have
   * those options and we should disable metrics (and version-check) by default.
   *)
  Logs.debug (fun m -> m "conf = %s" (Test_CLI.show_conf conf));
  if conf.pro then !hook_pro_init ();
  let matching_diagnosis = conf.matching_diagnosis in
  let errors = ref [] in

  (* step1: compute the set of tests (rule + target) *)
  (* We now support multiple targets (e.g., .jsx/.tsx) analyzed independently.
   * TODO: multiple targets analyzed together for --pro interfile analysis.
   *)
  let tests : tests = rules_and_targets conf.target errors in

  (* step2: run the tests *)
  let result : tests_result = run_tests caps conf tests errors in

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
let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Test_CLI.parse_argv argv in
  run_conf caps conf
