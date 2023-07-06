open Common
open File.Operators
open Testutil
module R = Rule
module MR = Mini_rule
module P = Pattern_match
module E = Semgrep_error_code
module Out = Output_from_core_t

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit (and integration) tests exercising the engine.
 *
 * Some of those tests exercise just the semgrep patterns part (with the
 * .sgrep), and not the whole rule part (with the .yaml). We could move
 * them in matching/Unit_matcher.ml but matching/ does not depend
 * on parsing/, so it's simpler to put those tests here.
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* TODO: move these to the "main" for the test suite. *)
(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"
let tests_path_patterns = tests_path / "patterns"
let polyglot_pattern_path = tests_path_patterns / "POLYGLOT"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Maturity tests *)
(*****************************************************************************)

(* coupling: https://semgrep.dev/docs/language-support/
 * See also https://r2c.quip.com/FOAuA4ThzULc/How-to-promote-a-language-
 *)
type maturity_level = GA | Beta | Experimental
[@@deriving show { with_path = false }]

(* coupling:
 * https://semgrep.dev/docs/language-support/#maturity-definitions
 * ../../scripts/generate-cheatsheet.py
 *)
let experimental_features =
  [
    "concrete_syntax";
    "deep_exprstmt";
    "dots_args";
    "dots_nested_stmts";
    "dots_stmts";
    "dots_string";
    "metavar_arg";
    "metavar_call";
    "metavar_equality_var" (* TODO: add dots_params? *);
  ]

let beta_features =
  experimental_features
  @ [
      "metavar_class_def";
      "metavar_func_def";
      "metavar_cond";
      "metavar_equality_expr";
      "metavar_equality_stmt";
      "metavar_import";
      "metavar_stmt";
    ]

let ga_features =
  beta_features
  @ [
      "deep_expr_operator";
      "dots_method_chaining";
      "equivalence_constant_propagation";
      "equivalence_naming_import";
      "metavar_anno";
      "metavar_key_value";
      "metavar_typed";
      "metavar_ellipsis_args";
      (* TODO: metavar_ellipsis_params *)
      (* TODO: metavar_string? *)
      "regexp_string";
    ]

let assoc_maturity_level =
  [
    (GA, ga_features);
    (Beta, beta_features);
    (Experimental, experimental_features);
  ]

(* coupling: ../../scripts/generate_cheatsheet.py LANGUAGE_EXCEPTIONS
 * Note that for some languages, e.g., JSON, certain tests do not
 * apply (NA), hence the exceptions listed above.
 * For others, we should really add the test and/or corresponding feature.
 *)
let language_exceptions =
  [
    (* GA languages *)

    (* TODO: why not regexp_string? NA for naming_import? *)
    (Lang.Csharp, [ "equivalence_naming_import"; "regexp_string" ]);
    (* TODO: metavar_anno sounds like an NA, but the other?? *)
    (Lang.Go, [ "metavar_class_def"; "metavar_import"; "metavar_anno" ]);
    (* TODO: NA for Java? *)
    (Lang.Java, [ "equivalence_naming_import"; "metavar_key_value" ]);
    (* metavar_typed is NA (dynamic language) *)
    (Lang.Js, [ "equivalence_naming_import"; "metavar_typed" ]);
    ( Lang.Ts,
      [
        "equivalence_naming_import";
        "metavar_typed";
        "metavar_anno";
        "metavar_class_def";
      ] );
    ( Lang.Php,
      [ "equivalence_naming_import"; "metavar_key_value"; "metavar_typed" ] );
    (* good boy, metavar_typed is working just for constants though *)
    (Lang.Python, []);
    (* metavar_typed is NA (dynamic language), metavar_anno also NA? *)
    (Lang.Ruby, [ "equivalence_naming_import"; "metavar_typed"; "metavar_anno" ]);
    (* regexp_string feature has been deprecated *)
    (Lang.Scala, [ "regexp_string"; "metavar_ellipsis_args" ]);
    (* Beta languages *)

    (* TODO: to fix *)
    (Lang.Kotlin, [ "dots_stmts"; "metavar_equality_var" ]);
    (* good boy *)
    (Lang.Rust, []);
    (* Experimental languages *)

    (* TODO: dots_nested_stmts to fix for C and C++ *)
    (Lang.C, [ "dots_nested_stmts" ]);
    (Lang.Cpp, [ "dots_nested_stmts" ]);
    (* good boy *)
    (Lang.Lua, []);
    (* dots_stmts is maybe NA, same with deep_exprstmt *)
    (Lang.Ocaml, [ "deep_exprstmt"; "dots_stmts" ]);
    (* Experimental languages *)
    (Lang.R, [ "deep_exprstmt" ]);
    (Lang.Elixir, [ "dots_nested_stmts" ]);
    (* xxx_stmts is NA *)
    (Lang.Jsonnet, [ "dots_stmts"; "deep_exprstmt"; "dots_nested_stmts" ]);
    (* TODO *)
    (Lang.Clojure, [ "deep_exprstmt"; "dots_nested_stmts" ]);
  ]

let maturity_tests () =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let check_maturity lang dir ext maturity =
    pack_tests
      (spf "Maturity %s for %s" (show_maturity_level maturity) (Lang.show lang))
      (let dir = tests_path_patterns / dir in
       let features = assoc_maturity_level |> List.assoc maturity in
       let exns =
         try List.assoc lang language_exceptions with
         | Not_found -> []
       in
       (* sanity check exns *)
       exns
       |> List.iter (fun base ->
              let path = dir / (base ^ ext) in
              if Sys.file_exists !!path then
                failwith
                  (spf "%s actually exist! remove it from exceptions" !!path));
       let features = Common2.minus_set features exns in
       features
       |> Common.map (fun base ->
              ( base,
                fun () ->
                  let path = dir / (base ^ ext) in
                  (* if it's a does-not-apply (NA) case, consider adding it
                   * to language_exceptions above
                   *)
                  if not (Sys.file_exists !!path) then
                    failwith
                      (spf "missing test file %s for maturity %s" !!path
                         (show_maturity_level maturity)) )))
  in
  (* coupling: https://semgrep.dev/docs/language-support/ *)
  pack_suites "Maturity level testing"
    [
      (* GA *)
      check_maturity Lang.Csharp "csharp" ".cs" GA;
      check_maturity Lang.Go "go" ".go" GA;
      check_maturity Lang.Java "java" ".java" GA;
      check_maturity Lang.Js "js" ".js" GA;
      (* JSON has too many NA, not worth it *)
      check_maturity Lang.Php "php" ".php" GA;
      check_maturity Lang.Python "python" ".py" GA;
      check_maturity Lang.Ruby "ruby" ".rb" GA;
      check_maturity Lang.Ts "ts" ".ts" GA;
      check_maturity Lang.Scala "scala" ".scala" GA;
      (* Beta *)
      check_maturity Lang.Hack "hack" ".hack" Beta;
      check_maturity Lang.Kotlin "kotlin" ".kt" Beta;
      check_maturity Lang.Rust "rust" ".rs" Beta;
      (* Terraform/HCL has too many NA, not worth it *)

      (* Experimental *)
      check_maturity Lang.Bash "bash" ".bash" Experimental;
      check_maturity Lang.C "c" ".c" Experimental;
      check_maturity Lang.Cpp "cpp" ".cpp" Experimental;
      (* TODO
         check_maturity Lang.Dockerfile "dockerfile" ".dockerfile" Experimental;
      *)
      check_maturity Lang.Lua "lua" ".lua" Experimental;
      check_maturity Lang.Ocaml "ocaml" ".ml" Experimental;
      check_maturity Lang.R "r" ".r" Experimental;
      check_maturity Lang.Solidity "solidity" ".sol" Experimental;
      check_maturity Lang.Elixir "elixir" ".ex" Experimental;
      check_maturity Lang.Swift "swift" ".swift" Experimental;
      check_maturity Lang.Julia "julia" ".jl" Experimental;
      (* YAML has too many NA, not worth it *)
      check_maturity Lang.Jsonnet "jsonnet" ".jsonnet" Experimental;
      check_maturity Lang.Clojure "clojure" ".clj" Experimental
      (* Not even experimental yet *)
      (* HTML, XML, Vue, Dart *);
    ]

(*****************************************************************************)
(* Language-specific tests *)
(*****************************************************************************)

let related_file_of_target ~polyglot_pattern_path ~ext ~file =
  let dirname, basename, _e = Common2.dbe_of_filename !!file in
  let candidate1 = Common2.filename_of_dbe (dirname, basename, ext) in
  if Sys.file_exists candidate1 then Ok (Fpath.v candidate1)
  else
    let candidate2 =
      Common2.filename_of_dbe (!!polyglot_pattern_path, basename, ext)
    in
    if Sys.file_exists candidate2 then Ok (Fpath.v candidate2)
    else
      let msg =
        spf "could not find %s file for test '%s' in either %s or %s" ext
          basename dirname !!polyglot_pattern_path
      in
      Error msg

(* Allows the  semgrep-core test runner that we use to test matches to also test
 * autofix. The format is pretty simple: add a `.fix` file with the fix pattern
 * and a `.fixed` file with the expected contents of the target after fixes are
 * applied.
 *
 * There are also end-to-end tests which test autofix on the CLI side.
 * Unfortunately, modifying them involves updating a large snapshot file, and
 * maintaining any large quantity of them would be onerous. Additionally, it's
 * not easy to adapt tests from our large existing test suite in semgrep-core to
 * also exercise autofix.
 *
 * Semgrep's `--test` flag can also test autofix
 * (https://github.com/returntocorp/semgrep/pull/5190), but it has the same
 * problems as the existing autofix e2e tests for these purposes. *)
let compare_fixes ~polyglot_pattern_path ~file matches =
  let expected_fixed_text =
    let expected_fixed_file =
      match
        related_file_of_target ~polyglot_pattern_path ~ext:"fixed" ~file
      with
      | Ok file -> file
      | Error msg -> failwith msg
    in
    File.read_file expected_fixed_file
  in
  let fixed_text = Autofix.apply_fixes_to_file matches ~file:!!file in
  Alcotest.(check string) "applied autofixes" expected_fixed_text fixed_text

let match_pattern ~lang ~hook ~file ~pattern ~fix_pattern =
  let pattern =
    try Parse_pattern.parse_pattern lang ~print_errors:true pattern with
    | exn ->
        failwith
          (spf "fail to parse pattern `%s` with lang = %s (exn = %s)" pattern
             (Lang.to_string lang) (Common.exn_to_s exn))
  in
  let rule =
    {
      MR.id = Rule.ID.of_string "unit-testing";
      pattern;
      inside = false;
      message = "";
      severity = R.Error;
      languages = [ lang ];
      pattern_string = "test: no need for pattern string";
      fix = fix_pattern;
    }
  in
  let ast =
    try Parse_target.parse_and_resolve_name_fail_if_partial lang !!file with
    | exn ->
        failwith
          (spf "fail to parse %s (exn = %s)" !!file (Common.exn_to_s exn))
  in
  let equiv = [] in
  Match_patterns.check ~hook
    (Rule_options.default_config, equiv)
    [ rule ] (!!file, lang, ast)

(*
   For each input file with the language's extension, locate a pattern file
   with the '.sgrep' extension.

   If foo/bar.sgrep is not found, POLYGLOT/bar.sgrep is used instead.
*)
let regression_tests_for_lang ~polyglot_pattern_path files lang =
  files
  |> Common.map (fun file ->
         ( Fpath.basename file,
           fun () ->
             let sgrep_file =
               match
                 related_file_of_target ~polyglot_pattern_path ~ext:"sgrep"
                   ~file
               with
               | Ok file -> file
               | Error msg -> failwith msg
             in
             let pattern = File.read_file sgrep_file in
             let fix_pattern =
               match
                 related_file_of_target ~polyglot_pattern_path ~ext:"fix" ~file
               with
               | Ok fix_file -> Some (File.read_file fix_file)
               | Error _ -> None
             in

             E.g_errors := [];

             (* old: semgrep-core used to support user-defined
              * equivalences, but the feature has been now deprecated.
              *
              * (* Python == is not the same than !(==) *)
              * if lang <> Lang.Python then
              *   Parse_equivalences.parse
              *     (Filename.concat data_path "basic_equivalences.yml")
              * else []
              *)
             let matches =
               match_pattern ~lang
                 ~hook:(fun { Pattern_match.range_loc; _ } ->
                   let start_loc, _end_loc = range_loc in
                   E.error
                     (Rule.ID.of_string "test-pattern")
                     start_loc "" Out.SemgrepMatchFound)
                 ~file ~pattern ~fix_pattern
             in
             (match fix_pattern with
             | Some _ -> compare_fixes ~polyglot_pattern_path ~file matches
             | None -> ());
             let actual = !E.g_errors in
             let expected = E.expected_error_lines_of_files [ !!file ] in
             E.compare_actual_to_expected_for_alcotest actual expected ))

let pack_regression_tests_for_lang ~test_pattern_path ~polyglot_pattern_path
    lang dir ext =
  pack_tests
    (spf "semgrep %s" (Lang.show lang))
    (let dir = test_pattern_path / dir in
     let files =
       Common2.glob (spf "%s/*%s" !!dir ext) |> File.Path.of_strings
     in
     regression_tests_for_lang ~polyglot_pattern_path files lang)

let pack_regression_tests lang_tests = pack_suites "lang testing" lang_tests

let make_lang_regression_tests ~test_pattern_path ~polyglot_pattern_path
    lang_data =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let lang_tests =
    lang_data
    |> Common.map (fun (lang, dir, ext) ->
           pack_regression_tests_for_lang ~test_pattern_path
             ~polyglot_pattern_path lang dir ext)
  in
  pack_regression_tests lang_tests

let lang_regression_tests ~polyglot_pattern_path =
  let test_pattern_path = tests_path_patterns in
  let regular_tests =
    make_lang_regression_tests ~test_pattern_path ~polyglot_pattern_path
      [
        (Lang.Bash, "bash", ".bash");
        (Lang.Dockerfile, "dockerfile", ".dockerfile");
        (Lang.Python, "python", ".py");
        (Lang.Js, "js", ".js");
        (Lang.Ts, "ts", ".ts");
        (Lang.Json, "json", ".json");
        (Lang.Java, "java", ".java");
        (Lang.C, "c", ".c");
        (Lang.Cpp, "cpp", ".cpp");
        (Lang.Go, "go", ".go");
        (Lang.Ocaml, "ocaml", ".ml");
        (Lang.Ruby, "ruby", ".rb");
        (Lang.Php, "php", ".php");
        (Lang.Hack, "hack", ".hack");
        (Lang.Csharp, "csharp", ".cs");
        (Lang.Lua, "lua", ".lua");
        (Lang.Rust, "rust", ".rs");
        (Lang.Cairo, "cairo", ".cairo");
        (Lang.Yaml, "yaml", ".yaml");
        (Lang.Scala, "scala", ".scala");
        (Lang.Swift, "swift", ".swift");
        (Lang.Html, "html", ".html");
        (Lang.Vue, "vue", ".vue");
        (Lang.Terraform, "terraform", ".tf");
        (Lang.Kotlin, "kotlin", ".kt");
        (Lang.Solidity, "solidity", ".sol");
        (Lang.Elixir, "elixir", ".ex");
        (Lang.R, "r", ".r");
        (* TODO: weird because the tests work in check_maturity above
         * but note here
         * (Lang.Julia, "julia", ".jl");
         *)
        (Lang.Jsonnet, "jsonnet", ".jsonnet");
        (Lang.Clojure, "clojure", ".clj");
        (Lang.Xml, "xml", ".xml");
      ]
  in
  let irregular_tests =
    pack_regression_tests
      [
        pack_tests "semgrep Typescript on Javascript (no JSX)"
          (let dir = test_pattern_path / "js" in
           let files = Common2.glob (spf "%s/*.js" !!dir) in
           let files =
             Common.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files
             |> File.Path.of_strings
           in
           let lang = Lang.Ts in
           regression_tests_for_lang ~polyglot_pattern_path files lang);
        pack_tests "semgrep C++ on C tests"
          (let dir = test_pattern_path / "c" in
           let files =
             Common2.glob (spf "%s/*.c" !!dir) |> File.Path.of_strings
           in
           let lang = Lang.Cpp in
           regression_tests_for_lang ~polyglot_pattern_path files lang);
      ]
  in
  regular_tests @ irregular_tests

(*****************************************************************************)
(* Eval_generic tests *)
(*****************************************************************************)

let eval_regression_tests () =
  [
    ( "eval regression testing",
      fun () ->
        let dir = tests_path / "eval" in
        let files = Common2.glob (spf "%s/*.json" !!dir) in
        files
        |> List.iter (fun file ->
               let env, code = Eval_generic.parse_json file in
               let res = Eval_generic.eval env code in
               Alcotest.(check bool)
                 (spf "%s should evaluate to true" file)
                 true
                 (Eval_generic.Bool true =*= res)) );
  ]

(*****************************************************************************)
(* Analyze_rule (filter irrelevant rules) tests *)
(*****************************************************************************)

let test_irrelevant_rule rule_file target_file =
  let rules = Parse_rule.parse rule_file in
  rules
  |> List.iter (fun rule ->
         match Analyze_rule.regexp_prefilter_of_rule rule with
         | None ->
             Alcotest.fail
               (spf "Rule %s: no regex prefilter formula"
                  (fst rule.id :> string))
         | Some (f, func) ->
             let content = File.read_file target_file in
             let s = Semgrep_prefilter_j.string_of_formula f in
             if func content then
               Alcotest.fail
                 (spf "Rule %s considered relevant by regex prefilter: %s"
                    (fst rule.id :> string)
                    s))

let test_irrelevant_rule_file target_file =
  ( Fpath.basename target_file,
    fun () ->
      let rules_file =
        let d, b, _e = Common2.dbe_of_filename !!target_file in
        let candidate1 = Common2.filename_of_dbe (d, b, "yaml") in
        if Sys.file_exists candidate1 then Fpath.v candidate1
        else
          failwith
            (spf "could not find target file for irrelevant rule %s"
               !!target_file)
      in
      test_irrelevant_rule rules_file target_file )

(* These tests test that semgrep with filter_irrelevant_rules correctly
   does not run files when they lack necessary strings.

   To test that filter_irrelevant_rules does not mistakenly filter out
   any files, place the rule/target pair in the rules folder but annotate
   in a comment that the test targets filter_irrelevant_rules to help
   future debuggers. *)
let filter_irrelevant_rules_tests () =
  pack_tests "filter irrelevant rules testing"
    (let dir = tests_path / "irrelevant_rules" in
     let target_files =
       Common2.glob (spf "%s/*" !!dir)
       |> File.Path.of_strings
       |> File_type.files_of_dirs_or_files (function
            | File_type.Config File_type.Yaml -> false
            | _ -> true (* TODO include .test.yaml*))
     in
     target_files
     |> Common.map (fun target_file -> test_irrelevant_rule_file target_file))

(*****************************************************************************)
(* Extract tests *)
(*****************************************************************************)

let get_extract_source_lang file rules =
  let _, _, erules,_, _ = R.partition_rules rules in
  let erule_langs =
    erules |> Common.map (fun r -> r.R.languages) |> List.sort_uniq compare
  in
  match erule_langs with
  | [] -> failwith (spf "no language for extract rule found in %s" !!file)
  | [ x ] -> x.target_analyzer
  | x :: _ ->
      let xlang = x.target_analyzer in
      pr2
        (spf
           "too many languages from extract rules found in %s, picking the \
            first one: %s"
           !!file (Xlang.show xlang));
      xlang

let extract_tests () =
  let path = tests_path / "extract" in
  pack_tests "extract mode"
    (let tests, _print_summary =
       Test_engine.make_tests ~unit_testing:true
         ~get_xlang:(Some get_extract_source_lang) [ path ]
     in
     tests)

(*****************************************************************************)
(* Tainting tests *)
(*****************************************************************************)

let tainting_test lang rules_file file =
  let rules =
    try Parse_rule.parse rules_file with
    | exn ->
        failwith
          (spf "fail to parse tainting rules %s (exn = %s)" !!rules_file
             (Common.exn_to_s exn))
  in
  let ast =
    try Parse_target.parse_and_resolve_name_warn_if_partial lang !!file with
    | exn ->
        failwith
          (spf "fail to parse %s (exn = %s)" !!file (Common.exn_to_s exn))
  in
  let rules =
    rules
    |> List.filter (fun r ->
           match r.Rule.languages.target_analyzer with
           | Xlang.L (x, xs) -> List.mem lang (x :: xs)
           | _ -> false)
  in
  let search_rules, taint_rules, extract_rules, secrets_rules, join_rules =
    Rule.partition_rules rules
  in
  assert (search_rules =*= []);
  assert (extract_rules =*= []);
  assert (secrets_rules =*= []);
  assert (join_rules =*= []);
  let xconf = Match_env.default_xconfig in

  let matches =
    taint_rules
    |> List.concat_map (fun rule ->
           let xtarget =
             {
               Xtarget.file;
               xlang = Xlang.L (lang, []);
               lazy_content = lazy (File.read_file file);
               lazy_ast_and_errors = lazy (ast, []);
             }
           in
           let results =
             Match_tainting_mode.check_rules
               ~match_hook:(fun _ _ -> ())
               ~per_rule_boilerplate_fn:(fun _rule f -> f ())
               [ rule ] xconf xtarget
           in
           match results with
           | [ res ] -> res.matches
           (* By construction, `check_rules` should only return the same number of results as rules it
              was initially given.
              So this case is impossible.
           *)
           | []
           | _ :: _ :: _ ->
               raise Impossible)
  in
  let actual =
    matches
    |> Common.map (fun m ->
           {
             rule_id = Some m.P.rule_id.id;
             E.typ = Out.SemgrepMatchFound;
             loc = fst m.range_loc;
             msg = m.P.rule_id.message;
             details = None;
           })
  in
  let expected = E.expected_error_lines_of_files [ !!file ] in
  E.compare_actual_to_expected_for_alcotest actual expected

let tainting_tests_for_lang files lang =
  files
  |> Common.map (fun file ->
         ( Fpath.basename file,
           fun () ->
             let rules_file =
               let d, b, _e = Common2.dbe_of_filename !!file in
               let candidate1 = Common2.filename_of_dbe (d, b, "yaml") in
               if Sys.file_exists candidate1 then Fpath.v candidate1
               else
                 failwith
                   (spf "could not find tainting rules file for %s" !!file)
             in
             tainting_test lang rules_file file ))

let lang_tainting_tests () =
  let taint_tests_path = tests_path / "tainting_rules" in
  pack_suites "lang tainting rules testing"
    [
      pack_tests "tainting Go"
        (let dir = taint_tests_path / "go" in
         let files =
           Common2.glob (spf "%s/*.go" !!dir) |> File.Path.of_strings
         in
         let lang = Lang.Go in
         tainting_tests_for_lang files lang);
      pack_tests "tainting PHP"
        (let dir = taint_tests_path / "php" in
         let files =
           Common2.glob (spf "%s/*.php" !!dir) |> File.Path.of_strings
         in
         let lang = Lang.Php in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Python"
        (let dir = taint_tests_path / "python" in
         let files =
           Common2.glob (spf "%s/*.py" !!dir) |> File.Path.of_strings
         in
         let lang = Lang.Python in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Java"
        (let dir = taint_tests_path / "java" in
         let files =
           Common2.glob (spf "%s/*.java" !!dir) |> File.Path.of_strings
         in
         let lang = Lang.Java in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Javascript"
        (let dir = taint_tests_path / "js" in
         let files =
           Common2.glob (spf "%s/*.js" !!dir) |> File.Path.of_strings
         in
         let lang = Lang.Js in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Typescript"
        (let dir = taint_tests_path / "ts" in
         let files =
           Common2.glob (spf "%s/*.ts" !!dir) |> File.Path.of_strings
         in
         let lang = Lang.Ts in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Scala"
        (let dir = taint_tests_path / "scala" in
         let files =
           Common2.glob (spf "%s/*.scala" !!dir) |> File.Path.of_strings
         in
         let lang = Lang.Scala in
         tainting_tests_for_lang files lang);
    ]

(*****************************************************************************)
(* Full rule tests *)
(*****************************************************************************)

(* TODO: For now we only have taint maturity tests for Beta, there are no specific
 * tests for GA. *)
(* TODO: We should also have here an explicit list of test filenames, like "taint_if",
 * that is then checked for every language, like we do for the search mode maturity
 * tests. *)
(* TODO: We could have a taint_maturity/POLYGLOT/ directory to put reusable rules
 * that can work for multiple languages (like we have for tests/patterns/POLYGLOT/ *)
let full_rule_taint_maturity_tests () =
  let path = tests_path / "taint_maturity" in
  pack_tests "taint maturity"
    (let tests, _print_summary =
       Test_engine.make_tests ~unit_testing:true [ path ]
     in
     tests)

let full_rule_regression_tests () =
  let path = tests_path / "rules" in
  pack_tests "full rule"
    (let tests, _print_summary =
       Test_engine.make_tests ~unit_testing:true [ path ]
     in
     tests)

(* quite similar to full_rule_regression_tests but prefer to pack_tests
 * with "full semgrep rule Java", so one can just run the Java tests
 * with ./test Java
 * alt: do like in deep-semgrep and call the toplevel engine
 * in a Unit_runner.ml instead of using Test_engine.make_tests
 *)
let full_rule_semgrep_rules_regression_tests () =
  let path = tests_path / "semgrep-rules" in
  let tests, _print_summary =
    Test_engine.make_tests ~unit_testing:true [ path ]
  in
  let groups =
    tests
    |> Common.map_filter (fun (name, ftest) ->
           let group_opt =
             match name with
             (* TODO: cleanup nodejsscan? "no target for" error *)
             | s
               when s =~ ".*/contrib/nodejsscan/xss_serialize_js.yaml"
                    || s =~ ".*/contrib/nodejsscan/xss_mustache_escape.yaml"
                    || s
                       =~ ".*/contrib/nodejsscan/xml_entity_expansion_dos.yaml"
                    || s =~ ".*/contrib/nodejsscan/timing_attack_node.yaml"
                    || s =~ ".*/contrib/nodejsscan/sql_injection.yaml"
                    || s =~ ".*/contrib/nodejsscan/security_electronjs.yaml"
                    || s =~ ".*/contrib/nodejsscan/resolve_path_traversal.yaml"
                    || s =~ ".*/contrib/nodejsscan/regex_injection.yaml"
                    || s =~ ".*/contrib/nodejsscan/logic_bypass.yaml"
                    || s =~ ".*/contrib/nodejsscan/jwt_hardcoded.yaml"
                    || s =~ ".*/contrib/nodejsscan/jwt_express_hardcoded.yaml"
                    || s =~ ".*/contrib/nodejsscan/good_ratelimiting.yaml"
                    || s =~ ".*/contrib/nodejsscan/good_helmet_checks.yaml"
                    || s =~ ".*/contrib/nodejsscan/good_anti_csrf.yaml"
                    || s =~ ".*/contrib/nodejsscan/eval_drpc_deserialize.yaml"
                    || s =~ ".*/contrib/nodejsscan/error_disclosure.yaml"
                    (* TODO: cleanup semgrep-rules: "no target for" error *)
                    || s =~ ".*/contrib/dlint/dlint-equivalent.yaml"
                    || s =~ ".*/fingerprints/fingerprints.yaml"
                    || s
                       =~ ".*/terraform/aws/security/aws-fsx-lustre-files-ystem.yaml"
                    (* TODO: Tests for tests/semgrep-rules/php/wordpress-plugins/security/audit/ are in
                     * a subfolder due to `paths:` constraints in the rule, perhaps Semgrep should ignore
                     * these constraints when in test mode. Note that `semgrep --test` simply ignores
                     * these files, but our test runner fails if it cannot find an example target file. *)
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-ajax-no-auth-and-auth-hooks-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-authorisation-checks-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-code-execution-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-command-execution-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-csrf-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-file-download-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-file-inclusion-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-file-manipulation-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-open-redirect-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-php-object-injection-audit.yaml"
                    || s
                       =~ ".*/php/wordpress-plugins/security/audit/wp-sql-injection-audit.yaml"
                    (* TODO: parse error, weird *)
                    || s =~ ".*/unicode/security/bidi.yml"
                    (* TODO many mismatches *)
                    || s =~ ".*/generic/ci/audit/changed-semgrepignore.*"
                    || s
                       =~ ".*/python/django/maintainability/duplicate-path-assignment.yaml"
                    (* ?? *)
                    || s =~ ".*/yaml/semgrep/consistency/.*" ->
                 Some "PB"
             (* not rule files *)
             | s when s =~ ".*.test.yml" -> None
             (* not languages tests *)
             | s when s =~ ".*/semgrep-rules/stats/" -> None
             | s when s =~ ".*/semgrep-rules/tests/" -> None
             (* ok let's keep all the other one with the appropriate group name *)
             | s when s =~ ".*/semgrep-rules/\\([a-zA-Z]+\\)/.*" ->
                 let s = Common.matched1 name in
                 Some (String.capitalize_ascii s)
             (* this skips the semgrep-rules/.github enrtries *)
             | _ ->
                 logger#info "skipping %s" name;
                 None
           in
           group_opt |> Option.map (fun groupname -> (groupname, (name, ftest))))
    |> Common.group_assoc_bykey_eff
  in
  pack_suites "full semgrep rule"
    (groups
    |> Common.map (fun (group, tests) ->
           pack_tests (spf "%s" group) tests
           |> Common.map (fun (name, ftest) ->
                  let test () =
                    match group with
                    | "PB" ->
                        let is_throwing =
                          try
                            ftest ();
                            false
                          with
                          | _exn -> true
                        in
                        if not is_throwing then
                          Alcotest.fail
                            "this used to raise an error (good news?)"
                    | _ -> ftest ()
                  in
                  (name, test))))

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests () =
  List.flatten
    [
      (* full testing for many languages *)
      lang_regression_tests ~polyglot_pattern_path;
      eval_regression_tests ();
      filter_irrelevant_rules_tests ();
      extract_tests ();
      lang_tainting_tests ();
      maturity_tests ();
      full_rule_taint_maturity_tests ();
      full_rule_regression_tests ();
      full_rule_semgrep_rules_regression_tests ();
    ]
