open Common
open Fpath_.Operators
module R = Rule
module MR = Mini_rule
module P = Pattern_match
module E = Core_error
module OutJ = Semgrep_output_v1_t

let logger = Logging.get_logger [ __MODULE__ ]
let t = Testo.create

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Tests exercising the engine.
 *
 * Some of those tests exercise just the semgrep patterns part (with the
 * .sgrep), and not the whole rule part (with the .yaml). We could move
 * them in matching/Unit_matcher.ml but matching/ does not depend
 * on parsing/, so it's simpler to put those tests here.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type fix_type =
  | Fix of string
  | FixRegex of (* regex *) string * int option * (* replacement *) string
  | NoFix

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* TODO: move these to the "main" for the test suite. *)
(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"
let tests_path_patterns = tests_path / "patterns"
let tests_path_autofix = tests_path / "autofix"
let polyglot_pattern_path = tests_path_patterns / "POLYGLOT"

(* TODO: infer dir and ext from lang using Lang helper functions *)
let full_lang_info =
  [
    (Lang.Bash, "bash", ".bash");
    (Lang.Dockerfile, "dockerfile", ".dockerfile");
    (Lang.Python, "python", ".py");
    (Lang.Promql, "promql", ".promql");
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
    (Lang.Julia, "julia", ".jl");
    (Lang.Jsonnet, "jsonnet", ".jsonnet");
    (Lang.Clojure, "clojure", ".clj");
    (Lang.Xml, "xml", ".xml");
    (Lang.Dart, "dart", ".dart");
  ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* lang_test_fn should:
   Generate a test suite for a list of languages.
   Two main folders are expected:
   - test_pattern_path: folder containing one subfolder per language
   - polyglot_pattern_path: folder containing patterns shared by multiple
     languages.

   Each language is specified as a triple:
   - language of type Lang.t e.g. Lang.Ruby
   - subdirectory containing the test cases e.g. "ruby"
   - language extension of the target files e.g. ".rb"

   Each language folder contains pairs of files:
   - foo.rb: target file for the test case "foo".
   - foo.sgrep: target file for the test case "foo". If missing, it must
     exist in the polyglot folder.
*)
let pack_tests_for_lang
    ~(lang_test_fn :
       polyglot_pattern_path:Fpath.t ->
       Fpath.t list ->
       Language.t ->
       Testo.test list) ~test_pattern_path ~polyglot_pattern_path lang dir ext =
  Testo.categorize
    (spf "semgrep %s" (Lang.show lang))
    (let dir = test_pattern_path / dir in
     let files = Common2.glob (spf "%s/*%s" !!dir ext) |> Fpath_.of_strings in

     lang_test_fn ~polyglot_pattern_path files lang)

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
    (* xxx_stmts is NA *)
    (Lang.Jsonnet, [ "dots_stmts"; "deep_exprstmt"; "dots_nested_stmts" ]);
    (* TODO *)
    (Lang.Clojure, [ "deep_exprstmt"; "dots_nested_stmts" ]);
  ]

(* TODO: infer dir and ext from lang using Lang helper functions *)
let make_maturity_tests ?(lang_exn = language_exceptions) lang dir ext maturity
    =
  Testo.categorize
    (spf "Maturity %s for %s" (show_maturity_level maturity) (Lang.show lang))
    (let dir = tests_path_patterns / dir in
     let features = assoc_maturity_level |> List.assoc maturity in
     let exns =
       try List.assoc lang lang_exn with
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
     |> List_.map (fun base ->
            Testo.create ~tags:(Test_tags.tags_of_lang lang) base (fun () ->
                let path = dir / (base ^ ext) in
                (* if it's a does-not-apply (NA) case, consider adding it
                 * to language_exceptions above
                 *)
                if not (Sys.file_exists !!path) then
                  failwith
                    (spf "missing test file %s for maturity %s" !!path
                       (show_maturity_level maturity)))))

let maturity_tests () =
  (* coupling: https://semgrep.dev/docs/language-support/ *)
  Testo.categorize_suites "Maturity level testing"
    [
      (* GA *)
      make_maturity_tests Lang.Csharp "csharp" ".cs" GA;
      make_maturity_tests Lang.Go "go" ".go" GA;
      make_maturity_tests Lang.Java "java" ".java" GA;
      make_maturity_tests Lang.Js "js" ".js" GA;
      (* JSON has too many NA, not worth it *)
      make_maturity_tests Lang.Php "php" ".php" GA;
      make_maturity_tests Lang.Python "python" ".py" GA;
      make_maturity_tests Lang.Ruby "ruby" ".rb" GA;
      make_maturity_tests Lang.Ts "ts" ".ts" GA;
      make_maturity_tests Lang.Scala "scala" ".scala" GA;
      (* Beta *)
      make_maturity_tests Lang.Hack "hack" ".hack" Beta;
      make_maturity_tests Lang.Kotlin "kotlin" ".kt" Beta;
      make_maturity_tests Lang.Rust "rust" ".rs" Beta;
      (* Terraform/HCL has too many NA, not worth it *)

      (* Experimental *)
      make_maturity_tests Lang.Bash "bash" ".bash" Experimental;
      make_maturity_tests Lang.C "c" ".c" Experimental;
      make_maturity_tests Lang.Cpp "cpp" ".cpp" Experimental;
      (* TODO
         make_maturity_tests Lang.Dockerfile "dockerfile" ".dockerfile" Experimental;
      *)
      make_maturity_tests Lang.Lua "lua" ".lua" Experimental;
      make_maturity_tests Lang.Ocaml "ocaml" ".ml" Experimental;
      make_maturity_tests Lang.R "r" ".r" Experimental;
      make_maturity_tests Lang.Solidity "solidity" ".sol" Experimental;
      make_maturity_tests Lang.Swift "swift" ".swift" Experimental;
      make_maturity_tests Lang.Julia "julia" ".jl" Experimental;
      (* YAML has too many NA, not worth it *)
      make_maturity_tests Lang.Jsonnet "jsonnet" ".jsonnet" Experimental;
      make_maturity_tests Lang.Clojure "clojure" ".clj" Experimental
      (* Not even experimental yet *)
      (* HTML, XML, Vue, Dart *);
    ]

(*****************************************************************************)
(* Language-specific tests *)
(*****************************************************************************)

let related_file_of_target ~polyglot_pattern_path ~ext ~file =
  let dirname, basename, _e = Filename_.dbe_of_filename !!file in
  let candidate1 = Filename_.filename_of_dbe (dirname, basename, ext) in
  if Sys.file_exists candidate1 then Ok (Fpath.v candidate1)
  else
    let candidate2 =
      Filename_.filename_of_dbe (!!polyglot_pattern_path, basename, ext)
    in
    if Sys.file_exists candidate2 then Ok (Fpath.v candidate2)
    else
      let msg =
        spf "could not find %s file for test '%s' in either %s or %s" ext
          basename dirname !!polyglot_pattern_path
      in
      Error msg

let match_pattern ~lang ~hook ~file ~pattern ~fix =
  let pattern =
    try Parse_pattern.parse_pattern lang ~print_errors:true pattern with
    | exn ->
        failwith
          (spf "fail to parse pattern `%s` with lang = %s (exn = %s)" pattern
             (Lang.to_string lang) (Common.exn_to_s exn))
  in
  let fix, fix_regexp =
    match fix with
    | NoFix -> (None, None)
    | Fix s -> (Some s, None)
    | FixRegex (regexp, count, replacement) ->
        (None, Some Rule.{ regexp; count; replacement })
  in
  let rule =
    {
      MR.id = Rule_ID.of_string "unit-testing";
      pattern;
      inside = false;
      message = "";
      severity = `Error;
      langs = [ lang ];
      pattern_string = "test: no need for pattern string";
      fix;
      fix_regexp;
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
    [ rule ] (file, lang, ast)

(*
   For each input file with the language's extension, locate a pattern file
   with the '.sgrep' extension.

   If foo/bar.sgrep is not found, POLYGLOT/bar.sgrep is used instead.
*)
let regression_tests_for_lang ~polyglot_pattern_path files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang) (Fpath.basename file)
           (fun () ->
             let sgrep_file =
               match
                 related_file_of_target ~polyglot_pattern_path ~ext:"sgrep"
                   ~file
               with
               | Ok file -> file
               | Error msg -> failwith msg
             in
             let pattern = UFile.read_file sgrep_file in

             (* old: semgrep-core used to support user-defined
                * equivalences, but the feature has been now deprecated.
                *
                * (* Python == is not the same than !(==) *)
                * if lang <> Lang.Python then
                *   Parse_equivalences.parse
                *     (Filename.concat data_path "basic_equivalences.yml")
                * else []
             *)
             match_pattern ~lang
               ~hook:(fun { Pattern_match.range_loc; _ } ->
                 let start_loc, _end_loc = range_loc in
                 E.push_error
                   (Rule_ID.of_string "test-pattern")
                   start_loc "" OutJ.SemgrepMatchFound)
               ~file ~pattern ~fix:NoFix
             |> ignore;
             let actual = !E.g_errors in
             E.g_errors := [];
             let expected = E.expected_error_lines_of_files [ file ] in
             E.compare_actual_to_expected_for_alcotest actual expected))

let make_lang_regression_tests ~test_pattern_path ~polyglot_pattern_path
    lang_data =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let lang_tests =
    lang_data
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:regression_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  Testo.categorize_suites "lang testing" lang_tests

let lang_regression_tests ~polyglot_pattern_path =
  let test_pattern_path = tests_path_patterns in
  let regular_tests =
    full_lang_info
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:regression_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  let irregular_tests =
    [
      Testo.categorize "semgrep Typescript on Javascript (no JSX)"
        (let dir = test_pattern_path / "js" in
         let files = Common2.glob (spf "%s/*.js" !!dir) in
         let files =
           List_.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files
           |> Fpath_.of_strings
         in

         let lang = Lang.Ts in
         regression_tests_for_lang ~polyglot_pattern_path files lang);
      Testo.categorize "semgrep C++ on C tests"
        (let dir = test_pattern_path / "c" in
         let files = Common2.glob (spf "%s/*.c" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Cpp in
         regression_tests_for_lang ~polyglot_pattern_path files lang);
    ]
  in
  Testo.categorize_suites "lang testing" (regular_tests @ irregular_tests)

(*****************************************************************************)
(* Autofix tests *)
(*****************************************************************************)

(* Allows the semgrep-core test runner that we use to test matches to also test
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
    UFile.read_file expected_fixed_file
  in
  let processed_matches =
    Autofix.produce_autofixes (List_.map Core_result.mk_processed_match matches)
  in
  let file = Fpath.to_string file in
  let fixed_text =
    processed_matches
    |> List_.map_filter (fun (m : Core_result.processed_match) ->
           m.autofix_edit)
    |> Autofix.apply_fixes_to_file ~file
  in
  Alcotest.(check string) "applied autofixes" expected_fixed_text fixed_text

let autofix_tests_for_lang ~polyglot_pattern_path files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang) (Fpath.basename file)
           (fun () ->
             let sgrep_file =
               match
                 related_file_of_target ~polyglot_pattern_path ~ext:"sgrep"
                   ~file
               with
               | Ok file -> file
               | Error msg -> failwith msg
             in
             let pattern = UFile.read_file sgrep_file in
             let fix =
               match
                 related_file_of_target ~polyglot_pattern_path ~ext:"fix" ~file
               with
               | Ok fix_file -> Fix (UFile.read_file fix_file)
               | Error _ -> (
                   (* A poor man's configuration format.
                      Either two or three lines.
                      regex-replacement or regex-count-replacement.
                   *)
                   match
                     related_file_of_target ~polyglot_pattern_path
                       ~ext:"fix-regex" ~file
                   with
                   | Ok fix_regex_file -> (
                       match UFile.cat fix_regex_file with
                       | [ l1; l2 ] -> FixRegex (l1, None, l2)
                       | [ l1; l2; l3 ] ->
                           FixRegex (l1, Some (int_of_string l2), l3)
                       | _ ->
                           failwith
                             (Common.spf
                                "found fix-regex file %s with <> 2 lines"
                                (Fpath.to_string fix_regex_file)))
                   | Error _ ->
                       failwith
                         (Common.spf "no fix file found for autofix test %s"
                            (Fpath.to_string file)))
             in

             let matches =
               match_pattern ~lang
                 ~hook:(fun { Pattern_match.range_loc; _ } ->
                   let start_loc, _end_loc = range_loc in
                   (* TODO? needed? we don't seem to use it,
                    * maybe left because of copy-pasta?
                    *)
                   E.push_error
                     (Rule_ID.of_string "test-pattern")
                     start_loc "" OutJ.SemgrepMatchFound)
                 ~file ~pattern ~fix
             in
             E.g_errors := [];
             match fix with
             | NoFix -> ()
             | _ -> compare_fixes ~polyglot_pattern_path ~file matches))

let lang_autofix_tests ~polyglot_pattern_path =
  let test_pattern_path = tests_path_autofix in
  let lang_tests =
    full_lang_info
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:autofix_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  Testo.categorize_suites "autofix testing" lang_tests

(*****************************************************************************)
(* Eval_generic tests *)
(*****************************************************************************)

let eval_regression_tests () =
  [
    t "eval regression testing" (fun () ->
        let dir = tests_path / "eval" in
        let files = Common2.glob (spf "%s/*.json" !!dir) in
        files
        |> List.iter (fun file ->
               let env, code = Eval_generic.parse_json file in
               let res = Eval_generic.eval env code in
               Alcotest.(check bool)
                 (spf "%s should evaluate to true" file)
                 true
                 (Eval_generic.Bool true =*= res)));
  ]

(*****************************************************************************)
(* Analyze_rule (filter irrelevant rules) tests *)
(*****************************************************************************)

let test_irrelevant_rule rule_file target_file =
  let cache = Some (Hashtbl.create 101) in
  let rules = Parse_rule.parse rule_file in
  rules
  |> List.iter (fun rule ->
         match Analyze_rule.regexp_prefilter_of_rule ~cache rule with
         | None ->
             Alcotest.fail
               (spf "Rule %s: no regex prefilter formula"
                  (Rule_ID.to_string (fst rule.id)))
         | Some (f, func) ->
             let content = UFile.read_file target_file in
             let s = Semgrep_prefilter_j.string_of_formula f in
             if func content then
               Alcotest.fail
                 (spf "Rule %s considered relevant by regex prefilter: %s"
                    (Rule_ID.to_string (fst rule.id))
                    s))

let test_irrelevant_rule_file target_file =
  t (Fpath.basename target_file) (fun () ->
      let rules_file =
        let d, b, _e = Filename_.dbe_of_filename !!target_file in
        let candidate1 = Filename_.filename_of_dbe (d, b, "yaml") in
        if Sys.file_exists candidate1 then Fpath.v candidate1
        else
          failwith
            (spf "could not find target file for irrelevant rule %s"
               !!target_file)
      in
      test_irrelevant_rule rules_file target_file)

(* These tests test that semgrep with filter_irrelevant_rules correctly
   does not run files when they lack necessary strings.

   To test that filter_irrelevant_rules does not mistakenly filter out
   any files, place the rule/target pair in the rules folder but annotate
   in a comment that the test targets filter_irrelevant_rules to help
   future debuggers. *)
let filter_irrelevant_rules_tests () =
  Testo.categorize "filter irrelevant rules testing"
    (let dir = tests_path / "irrelevant_rules" in
     let target_files =
       Common2.glob (spf "%s/*" !!dir)
       |> Fpath_.of_strings
       |> File_type.files_of_dirs_or_files (function
            | File_type.Config File_type.Yaml -> false
            | _ -> true (* TODO include .test.yaml*))
     in
     target_files
     |> List_.map (fun target_file -> test_irrelevant_rule_file target_file))

(*****************************************************************************)
(* Extract tests *)
(*****************************************************************************)

let get_extract_source_lang file rules =
  let _, _, erules, _, _ = R.partition_rules rules in
  let erule_langs =
    erules |> List_.map (fun r -> r.R.target_analyzer) |> List.sort_uniq compare
  in
  match erule_langs with
  | [] -> failwith (spf "no language for extract rule found in %s" !!file)
  | [ x ] -> x
  | xlang :: _ ->
      UCommon.pr2
        (spf
           "too many languages from extract rules found in %s, picking the \
            first one: %s"
           !!file (Xlang.show xlang));
      xlang

let extract_tests () =
  let path = tests_path / "extract" in
  Testo.categorize "extract mode"
    (Test_engine.make_tests ~get_xlang:get_extract_source_lang [ path ])

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
           match r.Rule.target_analyzer with
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
               lazy_content = lazy (UFile.read_file file);
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
    |> List_.map (fun m ->
           {
             rule_id = Some m.P.rule_id.id;
             E.typ = OutJ.SemgrepMatchFound;
             loc = fst m.range_loc;
             msg = m.P.rule_id.message;
             details = None;
           })
  in
  let expected = E.expected_error_lines_of_files [ file ] in
  E.compare_actual_to_expected_for_alcotest actual expected

let tainting_tests_for_lang files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang) (Fpath.basename file)
           (fun () ->
             let rules_file =
               let d, b, _e = Filename_.dbe_of_filename !!file in
               let candidate1 = Filename_.filename_of_dbe (d, b, "yaml") in
               if Sys.file_exists candidate1 then Fpath.v candidate1
               else
                 failwith
                   (spf "could not find tainting rules file for %s" !!file)
             in
             tainting_test lang rules_file file))

let lang_tainting_tests () =
  let taint_tests_path = tests_path / "tainting_rules" in
  Testo.categorize_suites "lang tainting rules testing"
    [
      Testo.categorize "tainting Go"
        (let dir = taint_tests_path / "go" in
         let files = Common2.glob (spf "%s/*.go" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Go in
         tainting_tests_for_lang files lang);
      Testo.categorize "tainting PHP"
        (let dir = taint_tests_path / "php" in
         let files = Common2.glob (spf "%s/*.php" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Php in
         tainting_tests_for_lang files lang);
      Testo.categorize "tainting Python"
        (let dir = taint_tests_path / "python" in
         let files = Common2.glob (spf "%s/*.py" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Python in
         tainting_tests_for_lang files lang);
      Testo.categorize "tainting Java"
        (let dir = taint_tests_path / "java" in
         let files =
           Common2.glob (spf "%s/*.java" !!dir) |> Fpath_.of_strings
         in

         let lang = Lang.Java in
         tainting_tests_for_lang files lang);
      Testo.categorize "tainting Javascript"
        (let dir = taint_tests_path / "js" in
         let files = Common2.glob (spf "%s/*.js" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Js in
         tainting_tests_for_lang files lang);
      Testo.categorize "tainting Ruby"
        (let dir = taint_tests_path / "ruby" in
         let files = Common2.glob (spf "%s/*.rb" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Ruby in
         tainting_tests_for_lang files lang);
      Testo.categorize "tainting Typescript"
        (let dir = taint_tests_path / "ts" in
         let files = Common2.glob (spf "%s/*.ts" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Ts in
         tainting_tests_for_lang files lang);
      Testo.categorize "tainting Scala"
        (let dir = taint_tests_path / "scala" in
         let files =
           Common2.glob (spf "%s/*.scala" !!dir) |> Fpath_.of_strings
         in

         let lang = Lang.Scala in
         tainting_tests_for_lang files lang);
    ]

(*****************************************************************************)
(* Full rule tests *)
(*****************************************************************************)

let full_rule_regression_tests () =
  let path = tests_path / "rules" in
  let tests1 = Test_engine.make_tests ~prepend_lang:true [ path ] in
  let path = tests_path / "rules_v2" in
  let tests2 = Test_engine.make_tests ~prepend_lang:true [ path ] in
  let tests = tests1 @ tests2 in
  let groups =
    tests
    |> List_.map (fun (test : Testo.test) ->
           let group =
             match String.split_on_char ' ' test.name with
             | lang :: _ -> lang
             | _ -> test.name
           in
           (group, test))
    |> Assoc.group_assoc_bykey_eff
  in

  Testo.categorize_suites "full rule"
    (groups |> List_.map (fun (group, tests) -> Testo.categorize group tests))

(* TODO: For now we only have taint maturity tests for Beta, there are no
 * specific tests for GA.
 * TODO: We should also have here an explicit list of test filenames, like
 * "taint_if", that is then checked for every language, like we do for the
 * search mode maturity tests.
 * TODO: We could have a taint_maturity/POLYGLOT/ directory to put reusable
 * rules that can work for multiple languages (like we have
 * for tests/patterns/POLYGLOT/
 *)
let full_rule_taint_maturity_tests () =
  let path = tests_path / "taint_maturity" in
  Testo.categorize "taint maturity" (Test_engine.make_tests [ path ])

(*
   Special exclusions for Semgrep JS
*)
let mark_todo_js (test : Testo.test) =
  match test.name with
  | s
    when (* The target file has an unsupported .erb extension, making it excluded
            correctly by the OCaml test suite but not by the JS test suite
            (or something close to this). *)
         s =~ ".*/ruby/rails/security/brakeman/check-reverse-tabnabbing.yaml" ->
      Testo.update test ~tags:(Test_tags.todo_js :: test.tags)
  | _ -> test

(* quite similar to full_rule_regression_tests but prefer to pack_tests
 * with "full semgrep rule Java", so one can just run the Java tests
 * with ./test Java
 * alt: do like in deep-semgrep and call the toplevel engine
 * in a Unit_runner.ml instead of using Test_engine.make_tests
 *)
let full_rule_semgrep_rules_regression_tests () =
  let path = tests_path / "semgrep-rules" in
  let tests = Test_engine.make_tests [ path ] in
  let groups =
    tests
    |> List_.map_filter (fun (test : Testo.test) ->
           let test = mark_todo_js test in
           let group_opt =
             match test.name with
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
                    (* TODO: do this filtering before Test_engine.make_tests
                       because it already requires the target files. *)
                    || s =~ ".*/contrib/dlint/dlint-equivalent.yaml"
                    || s =~ ".*/fingerprints/fingerprints.yaml"
                    || s
                       =~ ".*/terraform/aws/security/aws-fsx-lustre-files-ystem.yaml"
                    || s =~ ".*/generic/ci/audit/changed-semgrepignore.*"
                    (* TODO: parse error, weird *)
                    || s =~ ".*/unicode/security/bidi.yml"
                    || s
                       =~ ".*/python/django/maintainability/duplicate-path-assignment.yaml"
                    (* ?? *)
                    || s =~ ".*/yaml/semgrep/consistency/.*" ->
                 Some "XFAIL"
             (* not rule files *)
             | s when s =~ ".*.test.yml" -> None
             (* not languages tests *)
             | s when s =~ ".*/semgrep-rules/stats/" -> None
             | s when s =~ ".*/semgrep-rules/tests/" -> None
             (* ok let's keep all the other one with the appropriate group name *)
             | s when s =~ ".*/semgrep-rules/\\([a-zA-Z]+\\)/.*" ->
                 (* This is confusing because it looks like a programming
                    language from Lang.t but there's no guarantee that
                    it's a valid one.
                    TODO: don't capitalize? leave a slash? *)
                 let s = Common.matched1 test.name in
                 Some (String.capitalize_ascii s)
             (* this skips the semgrep-rules/.github entries *)
             | _ ->
                 logger#info "skipping %s" test.name;
                 None
           in
           group_opt |> Option.map (fun groupname -> (groupname, test)))
    |> Assoc.group_assoc_bykey_eff
  in

  Testo.categorize_suites "full semgrep rule"
    (groups
    |> List_.map (fun (group, tests) ->
           tests
           |> List_.map (fun (test : Testo.test) ->
                  match group with
                  | "XFAIL" ->
                      (* TODO: populate the excuse below with the exact reason
                         found in the comments above *)
                      Testo.update test
                        ~expected_outcome:
                          (Should_fail
                             "excluded semgrep-rule (see OCaml source file for \
                              details)")
                  | _ -> test)
           |> Testo.categorize group))

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests () =
  List.flatten
    [
      (* full testing for many languages *)
      lang_regression_tests ~polyglot_pattern_path;
      lang_autofix_tests ~polyglot_pattern_path;
      eval_regression_tests ();
      filter_irrelevant_rules_tests ();
      extract_tests ();
      lang_tainting_tests ();
      maturity_tests ();
      full_rule_taint_maturity_tests ();
      full_rule_regression_tests ();
      full_rule_semgrep_rules_regression_tests ();
    ]
