open Common
open Fpath_.Operators
module R = Rule
module MR = Mini_rule
module PM = Core_match
module E = Core_error
module Out = Semgrep_output_v1_t
module TCM = Test_compare_matches

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
    (Lang.Ql, "ql", ".ql");
    (Lang.Move_on_sui, "move_on_sui", ".move");
    (Lang.Move_on_aptos, "move_on_aptos", ".move");
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
       Testo.t list) ~test_pattern_path ~polyglot_pattern_path lang dir ext =
  Testo.categorize
    (spf "%s" (Lang.show lang))
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
    "metavar_equality_var";
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
      "dots_params";
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
    ( Lang.Csharp,
      [ "equivalence_naming_import"; "regexp_string"; "dots_params" ] );
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
      [
        "equivalence_naming_import";
        "metavar_key_value";
        "metavar_typed";
        "dots_params";
      ] );
    (* good boy, metavar_typed is working just for constants though *)
    (Lang.Python, []);
    (* metavar_typed is NA (dynamic language), metavar_anno also NA? *)
    ( Lang.Ruby,
      [
        "equivalence_naming_import";
        "metavar_typed";
        "metavar_anno";
        "dots_params";
      ] );
    (* regexp_string feature has been deprecated *)
    (Lang.Scala, [ "regexp_string"; "metavar_ellipsis_args"; "dots_params" ]);
    (* Beta languages *)

    (* TODO: to fix *)
    (Lang.Kotlin, [ "dots_stmts"; "metavar_equality_var" ]);
    (* good boy *)
    (Lang.Rust, []);
    (Lang.Move_on_sui, [ "metavar_key_value"; "regexp_string" ]);
    (Lang.Move_on_aptos, [ "metavar_key_value"; "regexp_string" ]);
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
  Testo.categorize_suites "Maturity levels"
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
      make_maturity_tests Lang.Move_on_sui "move_on_sui" ".move" Beta;
      make_maturity_tests Lang.Move_on_aptos "move_on_aptos" ".move" Beta;
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
      (* HTML, XML, Dart *);
    ]

(*****************************************************************************)
(* Language-specific tests *)
(*****************************************************************************)

let match_pattern ~lang ~hook ~file ~pattern ~fix =
  (* TODO? enable the "semgrep.parsing" src level maybe here *)
  let pattern =
    match Parse_pattern.parse_pattern lang pattern with
    | Ok pat -> pat
    | Error s ->
        failwith
          (spf "fail to parse pattern `%s` with lang = %s: %s" pattern
             (Lang.to_string lang) s)
    | exception exn ->
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
      MR.id = Rule_ID.of_string_exn "unit-testing";
      pattern;
      inside = false;
      message = "";
      metadata = None;
      severity = `Error;
      langs = [ lang ];
      pattern_string = "test: no need for pattern string";
      fix;
      fix_regexp;
    }
  in
  let ast =
    try Parse_target.parse_and_resolve_name_fail_if_partial lang file with
    | exn ->
        failwith
          (spf "fail to parse %s (exn = %s)" !!file (Common.exn_to_s exn))
  in
  let equiv = [] in
  Match_patterns.check ~hook
    (Rule_options.default, equiv)
    [ rule ]
    (file, File file, lang, ast)

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
                 Test_utils.related_file_of_target ~polyglot_pattern_path
                   ~ext:"sgrep" file
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
             let matches = ref [] in
             match_pattern ~lang
               ~hook:(fun pm -> Stack_.push (TCM.location_of_pm pm) matches)
               ~file ~pattern ~fix:NoFix
             |> ignore;
             let actual = !matches in
             let expected = TCM.expected_error_lines_of_files [ file ] in
             TCM.compare_actual_to_expected_for_alcotest ~to_location:Fun.id
               actual expected))

(* used in Unit_pro_languages.ml *)
let make_lang_regression_tests ~test_pattern_path ~polyglot_pattern_path
    lang_data =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let lang_tests =
    lang_data
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:regression_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  Testo.categorize_suites "sgrep patterns" lang_tests

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
      Testo.categorize "Typescript on Javascript (no JSX)"
        (let dir = test_pattern_path / "js" in
         let files = Common2.glob (spf "%s/*.js" !!dir) in
         let files =
           List_.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files
           |> Fpath_.of_strings
         in

         let lang = Lang.Ts in
         regression_tests_for_lang ~polyglot_pattern_path files lang);
      Testo.categorize "C++ on C tests"
        (let dir = test_pattern_path / "c" in
         let files = Common2.glob (spf "%s/*.c" !!dir) |> Fpath_.of_strings in

         let lang = Lang.Cpp in
         regression_tests_for_lang ~polyglot_pattern_path files lang);
    ]
  in
  Testo.categorize_suites "sgrep patterns" (regular_tests @ irregular_tests)

(*****************************************************************************)
(* Autofix tests *)
(*****************************************************************************)

let autofix_tests_for_lang ~polyglot_pattern_path files lang =
  files
  |> List_.map (fun file ->
         Testo.create ~tags:(Test_tags.tags_of_lang lang) (Fpath.basename file)
           (fun () ->
             let sgrep_file =
               match
                 Test_utils.related_file_of_target ~polyglot_pattern_path
                   ~ext:"sgrep" file
               with
               | Ok file -> file
               | Error msg -> failwith msg
             in
             let pattern = UFile.read_file sgrep_file in
             let fix =
               match
                 Test_utils.related_file_of_target ~polyglot_pattern_path
                   ~ext:"fix" file
               with
               | Ok fix_file -> Fix (UFile.read_file fix_file)
               | Error _ -> (
                   (* A poor man's configuration format.
                      This can either be two lines, the regex to match
                      and the replacement content (one line),
                      or 3+ lines, the regex to match, the number of matches
                      to replace, and the replacement text (possibly multiline)
                   *)
                   match
                     Test_utils.related_file_of_target ~polyglot_pattern_path
                       ~ext:"fix-regex" file
                   with
                   | Ok fix_regex_file -> (
                       match UFile.cat fix_regex_file with
                       | [ l1; l2 ] -> FixRegex (l1, None, l2)
                       | l1 :: l2 :: l3 :: rest ->
                           FixRegex
                             ( l1,
                               Some (int_of_string l2),
                               String.concat "\n" (l3 :: rest) )
                       | _ ->
                           failwith
                             (Common.spf
                                "found fix-regex file %s with invalid number \
                                 of lines"
                                (Fpath.to_string fix_regex_file)))
                   | Error _ ->
                       failwith
                         (Common.spf "no fix file found for autofix test %s"
                            (Fpath.to_string file)))
             in

             let matches =
               match_pattern ~lang ~hook:(fun _ -> ()) ~file ~pattern ~fix
             in
             match fix with
             | NoFix -> ()
             | _ ->
                 Test_utils.compare_fixes ~polyglot_pattern_path ~file matches))

let lang_autofix_tests ~polyglot_pattern_path =
  let test_pattern_path = tests_path_autofix in
  let lang_tests =
    full_lang_info
    |> List_.map (fun (lang, dir, ext) ->
           pack_tests_for_lang ~lang_test_fn:autofix_tests_for_lang
             ~test_pattern_path ~polyglot_pattern_path lang dir ext)
  in
  Testo.categorize_suites "autofix" lang_tests

(*****************************************************************************)
(* Eval_generic tests *)
(*****************************************************************************)

let eval_regression_tests () =
  [
    t "Eval_generic" (fun () ->
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
  (* TODO: fail more gracefully for invalid rules? *)
  let rules = Parse_rule.parse rule_file |> Result.get_ok in
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
  Testo.categorize "filter irrelevant rules"
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
(* Tainting tests *)
(*****************************************************************************)

let tainting_test (lang : Lang.t) (rules_file : Fpath.t) (file : Fpath.t) =
  let rules =
    match Parse_rule.parse rules_file with
    | Ok rules -> rules
    | Error e ->
        failwith
          (spf "fail to parse tainting rules %s (error = %s)" !!rules_file
             (Rule_error.string_of_error e))
  in
  let ast =
    try Parse_target.parse_and_resolve_name_warn_if_partial lang file with
    | exn ->
        failwith
          (spf "fail to parse %s (exn = %s)" !!file (Common.exn_to_s exn))
  in
  let rules =
    rules
    |> List.filter (fun r ->
           match r.Rule.target_analyzer with
           | Analyzer.L (x, xs) -> List.mem lang (x :: xs)
           | _ -> false)
  in
  let search_rules, taint_rules, extract_rules, join_rules =
    Rule.partition_rules rules
  in
  assert (search_rules =*= []);
  assert (extract_rules =*= []);
  assert (join_rules =*= []);
  let xconf = Match_env.default_xconfig in

  let matches =
    taint_rules
    |> List.concat_map (fun rule ->
           let xtarget : Xtarget.t =
             {
               path = { origin = File file; internal_path_to_content = file };
               analyzer = Analyzer.L (lang, []);
               lazy_content = lazy (UFile.read_file file);
               lazy_ast_and_errors = lazy (ast, []);
             }
           in
           let results =
             Match_tainting_mode.check_rules ~match_hook:Fun.id
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
    |> List_.map (fun (m : PM.t) ->
           E.
             {
               rule_id = Some m.rule_id.id;
               typ = Out.SemgrepMatchFound;
               loc = Some (fst m.range_loc);
               msg = m.rule_id.message;
               details = None;
             })
  in
  let regexp = ".*\\b\\(ruleid\\|todook\\):.*" in
  let expected = TCM.expected_error_lines_of_files ~regexp [ file ] in
  TCM.compare_actual_to_expected_for_alcotest
    ~to_location:TCM.location_of_core_error actual expected

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

(* DEPRECATED: this is redundant because we now have 'make rules-test'
 * which calls 'osemgrep-pro test --pro tests/tainting_rules'
 *)
let lang_tainting_tests () =
  let taint_tests_path = tests_path / "tainting_rules" in
  Testo.categorize_suites "lang tainting rules"
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

(* DEPRECATED: this is redundant because we now have 'make rules-test'
 * which calls 'osemgrep-pro test --pro tests/rules tests/rules_v2'
 *)
let full_rule_regression_tests () =
  let path = tests_path / "rules" in
  let tests1 = Test_engine.make_tests ~prepend_lang:true [ path ] in
  let path = tests_path / "rules_v2" in
  let tests2 = Test_engine.make_tests ~prepend_lang:true [ path ] in
  let tests = tests1 @ tests2 in
  let groups =
    tests
    |> List_.map (fun (test : Testo.t) ->
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
 * DEPRECATED: this is redundant because we now have 'make rules-test'
 * which calls 'osemgrep-pro test --pro tests/taint_maturity'
 *)
let full_rule_taint_maturity_tests () =
  let path = tests_path / "taint_maturity" in
  Testo.categorize "taint maturity" (Test_engine.make_tests [ path ])

(*
   Special exclusions for Semgrep JS
*)
let mark_todo_js (test : Testo.t) =
  match test.name with
  | s
    when (* The target file has an unsupported .erb extension, making it excluded
            correctly by the OCaml test suite but not by the JS test suite
            (or something close to this). *)
         s =~ ".*/ruby/rails/security/brakeman/check-reverse-tabnabbing.yaml"
         || (* Not sure why this fails *)
         s =~ ".*/ruby/lang/security/divide-by-zero.yaml" ->
      Testo.update test ~tags:(Test_tags.todo_js :: test.tags)
  | _ -> test

(* quite similar to full_rule_regression_tests but prefer to pack_tests
 * with "semgrep-rules repo Java", so one can just run the Java tests
 * with ./test Java
 * alt: do like in semgrep-pro and call the toplevel engine
 * in a Unit_runner.ml instead of using Test_engine.make_tests
 * TODO: get rid of this and rely on `osemgrep test` code instead of
 * Test_engine.make_tests as they differ sligltly and we're using
 * osemgrep test in the semgrep-rules repo CI, not -test_rules.
 * DEPRECATED: this is redundant because we now have 'make rules-test'
 * which calls 'osemgrep-pro test --pro tests/semgrep-rules'
 *)
let semgrep_rules_repo_tests () : Testo.t list =
  let path = tests_path / "semgrep-rules" in
  let tests = Test_engine.make_tests [ path ] in
  let groups =
    tests
    |> List_.filter_map (fun (test : Testo.t) ->
           let test = mark_todo_js test in
           let group_opt =
             match test.name with
             (* note that there is no need to filter rules without targets; This
              * is now handled in Test_engine.make_tests which will generate
              * an XFAIL Testo test for those.
              *)
             | s
               when (* TODO: we're skipping those rules because e.g. for bidy.yml
                       we're using a languages: [bash,c, ..., python] and a
                       bidy.py target file which cause Test_engine.make_test to
                       parse bidy.py with a bash parser (the first one in the
                       list) which then cause a parse error. (py|o)semgrep test
                       do not have the issue because they use the extension of
                       the file to decide which language to use instead of what
                       is in the rule
                    *)
                    s =~ ".*/unicode/security/bidi.yml"
                    || s =~ ".*/dockerfile/security/dockerd-socket-mount.yaml"
                    (* Elixir requires Pro *)
                    || s =~ ".*/elixir/lang/.*"
                    (* Apex requires Pro *)
                    || s =~ ".*/apex/lang/.*"
                       (* but the following are generic rules ... *)
                       && s
                          <> "tests/semgrep-rules/apex/lang/best-practice/ncino/tests/UseAssertClass.yaml"
                       && s
                          <> "tests/semgrep-rules/apex/lang/performance/ncino/operationsInLoops/AvoidNativeDmlInLoops.yaml"
                       && s
                          <> "tests/semgrep-rules/apex/lang/performance/ncino/operationsInLoops/AvoidSoqlInLoops.yaml"
                       && s
                          <> "tests/semgrep-rules/apex/lang/performance/ncino/operationsInLoops/AvoidSoslInLoops.yaml"
                       && s
                          <> "tests/semgrep-rules/apex/lang/performance/ncino/operationsInLoops/AvoidOperationsWithLimitsInLoops.yaml"
                       && s
                          <> "tests/semgrep-rules/apex/lang/security/ncino/dml/ApexCSRFStaticConstructor.yaml"
                    (* ?? *)
                    || s =~ ".*/yaml/semgrep/consistency/.*" ->
                 Some "XFAIL"
             (* not rule files *)
             | s when s =~ ".*.test.yml" -> None
             (* not languages tests *)
             | s when s =~ ".*/semgrep-rules/stats/" -> None
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
                 Logs.info (fun m -> m "skipping %s" test.name);
                 None
           in
           group_opt |> Option.map (fun groupname -> (groupname, test)))
    |> Assoc.group_assoc_bykey_eff
  in

  Testo.categorize_suites "semgrep-rules repo"
    (groups
    |> List_.map (fun (group, tests) ->
           tests
           |> List_.map (fun (test : Testo.t) ->
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
  List_.flatten
    [
      (* full testing for many languages *)
      lang_regression_tests ~polyglot_pattern_path;
      lang_autofix_tests ~polyglot_pattern_path;
      eval_regression_tests ();
      filter_irrelevant_rules_tests ();
      lang_tainting_tests ();
      maturity_tests ();
      full_rule_taint_maturity_tests ();
      full_rule_regression_tests ();
      semgrep_rules_repo_tests ();
    ]
