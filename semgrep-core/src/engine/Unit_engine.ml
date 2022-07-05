open Common
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

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../tests"

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
      [ "equivalence_naming_import"; "metavar_ellipsis_args"; "regexp_string" ]
    );
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
  ]

let maturity_tests () =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let check_maturity lang dir ext maturity =
    pack_tests
      (spf "Maturity %s for %s" (show_maturity_level maturity) (Lang.show lang))
      (let dir = Filename.concat tests_path dir in
       let features = assoc_maturity_level |> List.assoc maturity in
       let exns =
         try List.assoc lang language_exceptions with
         | Not_found -> []
       in
       (* sanity check exns *)
       exns
       |> List.iter (fun base ->
              let path = Filename.concat dir (base ^ ext) in
              if Sys.file_exists path then
                failwith
                  (spf "%s actually exist! remove it from exceptions" path));
       let features = Common2.minus_set features exns in
       features
       |> Common.map (fun base ->
              ( base,
                fun () ->
                  let path = Filename.concat dir (base ^ ext) in
                  (* if it's a does-not-apply (NA) case, consider adding it
                   * to language_exceptions above
                   *)
                  if not (Sys.file_exists path) then
                    failwith
                      (spf "missing test file %s for maturity %s" path
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
      (* Terraform/HCL has too many NA, not worth it *)

      (* Experimental *)
      check_maturity Lang.Bash "bash" ".bash" Experimental;
      check_maturity Lang.C "c" ".c" Experimental;
      check_maturity Lang.Cpp "cpp" ".cpp" Experimental;
      (* TODO dockerfile
          check_maturity Lang.Dockerfile "dockerfile" ".dockerfile" Experimental;
      *)
      check_maturity Lang.Lua "lua" ".lua" Experimental;
      check_maturity Lang.Ocaml "ocaml" ".ml" Experimental;
      (* TODO we say we support R, but not really actually *)
      (* TODO: too many exns, we need to write tests!
         check_maturity Lang.Rust "rust" ".rust" Experimental;
      *)
      check_maturity Lang.Solidity "solidity" ".sol" Experimental;
      (* YAML has too many NA, not worth it *)
      check_maturity Lang.R "r" ".r" Experimental
      (* Not even experimental *)
      (* HTML, Vue *);
    ]

(*****************************************************************************)
(* Language-specific tests *)
(*****************************************************************************)

let sgrep_file_of_target file =
  let d, b, _e = Common2.dbe_of_filename file in
  let candidate1 = Common2.filename_of_dbe (d, b, "sgrep") in
  if Sys.file_exists candidate1 then candidate1
  else
    let d = Filename.concat tests_path "POLYGLOT" in
    let candidate2 = Common2.filename_of_dbe (d, b, "sgrep") in
    if Sys.file_exists candidate2 then candidate2
    else failwith (spf "could not find sgrep file for %s" file)

(*
   For each input file with the language's extension, locate a pattern file
   with the '.sgrep' extension.

   If foo/bar.sgrep is not found, POLYGLOT/bar.sgrep is used instead.
*)
let regression_tests_for_lang ~with_caching files lang =
  files
  |> Common.map (fun file ->
         ( Filename.basename file,
           fun () ->
             let sgrep_file = sgrep_file_of_target file in
             let ast =
               try
                 Parse_target.parse_and_resolve_name_fail_if_partial lang file
               with
               | exn ->
                   failwith
                     (spf "fail to parse %s (exn = %s)" file
                        (Common.exn_to_s exn))
             in
             let pattern =
               try
                 Parse_pattern.parse_pattern lang ~print_errors:true
                   (Common.read_file sgrep_file)
               with
               | exn ->
                   failwith
                     (spf "fail to parse pattern %s with lang = %s (exn = %s)"
                        sgrep_file (Lang.to_string lang) (Common.exn_to_s exn))
             in
             E.g_errors := [];

             let rule =
               {
                 MR.id = "unit testing";
                 pattern;
                 inside = false;
                 message = "";
                 severity = R.Error;
                 languages = [ lang ];
                 pattern_string = "test: no need for pattern string";
               }
             in
             (* old: semgrep-core used to support user-defined
              * equivalences, but the feature has been now deprecated.
              *
              * (* Python == is not the same than !(==) *)
              * if lang <> Lang.Python then
              *   Parse_equivalences.parse
              *     (Filename.concat data_path "basic_equivalences.yml")
              * else []
              *)
             let equiv = [] in
             Common.save_excursion Flag_semgrep.with_opt_cache with_caching
               (fun () ->
                 Match_patterns.check
                   ~hook:(fun { Pattern_match.tokens = (lazy xs); _ } ->
                     (* there are a few fake tokens in the generic ASTs now (e.g.,
                      * for DotAccess generated outside the grammar) *)
                     let toks = xs |> List.filter Parse_info.is_origintok in
                     let minii, _maxii = Parse_info.min_max_ii_by_pos toks in
                     let minii_loc =
                       Parse_info.unsafe_token_location_of_info minii
                     in
                     E.error "test pattern" minii_loc "" Out.SemgrepMatchFound)
                   (Config_semgrep.default_config, equiv)
                   [ rule ] (file, lang, ast)
                 |> ignore;
                 let actual = !E.g_errors in
                 let expected = E.expected_error_lines_of_files [ file ] in
                 E.compare_actual_to_expected_for_alcotest actual expected) ))

let lang_regression_tests ~with_caching =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let pack_regression_tests_for_lang lang dir ext =
    pack_tests
      (spf "semgrep %s" (Lang.show lang))
      (let dir = Filename.concat tests_path dir in
       let files = Common2.glob (spf "%s/*%s" dir ext) in
       regression_tests_for_lang ~with_caching files lang)
  in
  let regression_tests_for_lang files lang =
    regression_tests_for_lang ~with_caching files lang
  in
  let name_suffix = if with_caching then " with caching" else " no caching" in
  pack_suites
    ("lang testing" ^ name_suffix)
    [
      pack_regression_tests_for_lang Lang.Bash "bash" ".bash";
      pack_regression_tests_for_lang Lang.Dockerfile "dockerfile" ".dockerfile";
      pack_regression_tests_for_lang Lang.Python "python" ".py";
      pack_regression_tests_for_lang Lang.Js "js" ".js";
      pack_regression_tests_for_lang Lang.Ts "ts" ".ts";
      pack_tests "semgrep Typescript on Javascript (no JSX)"
        (let dir = Filename.concat tests_path "js" in
         let files = Common2.glob (spf "%s/*.js" dir) in
         let files =
           Common.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files
         in
         let lang = Lang.Ts in
         regression_tests_for_lang files lang);
      pack_regression_tests_for_lang Lang.Json "json" ".json";
      pack_regression_tests_for_lang Lang.Java "java" ".java";
      pack_regression_tests_for_lang Lang.C "c" ".c";
      pack_regression_tests_for_lang Lang.Cpp "cpp" ".cpp";
      pack_tests "semgrep C++ on C tests"
        (let dir = Filename.concat tests_path "c" in
         let files = Common2.glob (spf "%s/*.c" dir) in
         let lang = Lang.Cpp in
         regression_tests_for_lang files lang);
      pack_regression_tests_for_lang Lang.Go "go" ".go";
      pack_regression_tests_for_lang Lang.Ocaml "ocaml" ".ml";
      pack_regression_tests_for_lang Lang.Ruby "ruby" ".rb";
      pack_regression_tests_for_lang Lang.Php "php" ".php";
      pack_regression_tests_for_lang Lang.Hack "hack" ".hack";
      pack_regression_tests_for_lang Lang.Csharp "csharp" ".cs";
      pack_regression_tests_for_lang Lang.Lua "lua" ".lua";
      pack_regression_tests_for_lang Lang.Rust "rust" ".rs";
      pack_regression_tests_for_lang Lang.Yaml "yaml" ".yaml";
      pack_regression_tests_for_lang Lang.Scala "scala" ".scala";
      pack_regression_tests_for_lang Lang.Swift "swift" ".swift";
      pack_regression_tests_for_lang Lang.Html "html" ".html";
      pack_regression_tests_for_lang Lang.Vue "vue" ".vue";
      pack_regression_tests_for_lang Lang.Hcl "hcl" ".tf";
      pack_regression_tests_for_lang Lang.Kotlin "kotlin" ".kt";
      pack_regression_tests_for_lang Lang.Solidity "solidity" ".sol";
      pack_regression_tests_for_lang Lang.R "r" ".r";
    ]

(*****************************************************************************)
(* Eval_generic tests *)
(*****************************************************************************)

let eval_regression_tests () =
  [
    ( "eval regression testing",
      fun () ->
        let dir = Filename.concat tests_path "OTHER/eval" in
        let files = Common2.glob (spf "%s/*.json" dir) in
        files
        |> List.iter (fun file ->
               let env, code = Eval_generic.parse_json file in
               let res = Eval_generic.eval env code in
               Alcotest.(check bool)
                 (spf "%s should evaluate to true" file)
                 true
                 (Eval_generic.Bool true = res)) );
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
               (spf "Rule %s: no regex prefilter formula" (fst rule.id))
         | Some (f, func) ->
             let content = read_file target_file in
             let s = Semgrep_prefilter_j.string_of_formula f in
             if func content then
               Alcotest.fail
                 (spf "Rule %s considered relevant by regex prefilter: %s"
                    (fst rule.id) s))

let test_irrelevant_rule_file target_file =
  ( Filename.basename target_file,
    fun () ->
      let rules_file =
        let d, b, _e = Common2.dbe_of_filename target_file in
        let candidate1 = Common2.filename_of_dbe (d, b, "yaml") in
        if Sys.file_exists candidate1 then candidate1
        else
          failwith
            (spf "could not find target file for irrelevant rule %s" target_file)
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
    (let dir = Filename.concat tests_path "OTHER/irrelevant_rules" in
     let target_files =
       Common2.glob (spf "%s/*" dir)
       |> File_type.files_of_dirs_or_files (function
            | File_type.Config File_type.Yaml -> false
            | _ -> true (* TODO include .test.yaml*))
     in
     target_files
     |> Common.map (fun target_file -> test_irrelevant_rule_file target_file))

(*****************************************************************************)
(* Tainting tests *)
(*****************************************************************************)

let tainting_test lang rules_file file =
  let rules =
    try Parse_rule.parse rules_file with
    | exn ->
        failwith
          (spf "fail to parse tainting rules %s (exn = %s)" rules_file
             (Common.exn_to_s exn))
  in
  let ast =
    try Parse_target.parse_and_resolve_name_warn_if_partial lang file with
    | exn ->
        failwith (spf "fail to parse %s (exn = %s)" file (Common.exn_to_s exn))
  in
  let rules =
    rules
    |> List.filter (fun r ->
           match r.Rule.languages with
           | Xlang.L (x, xs) -> List.mem lang (x :: xs)
           | _ -> false)
  in
  let search_rules, taint_rules = Rule.partition_rules rules in
  assert (search_rules = []);
  let matches =
    taint_rules
    |> Common.map (fun rule ->
           let equivs = [] in
           let xtarget =
             {
               Xtarget.file;
               xlang = Xlang.L (lang, []);
               lazy_content = lazy (Common.read_file file);
               lazy_ast_and_errors = lazy (ast, []);
             }
           in
           let res, _debug =
             Match_tainting_mode.check_rule rule
               (fun _ _ -> ())
               (Config_semgrep.default_config, equivs)
               xtarget
           in
           res.matches)
    |> List.flatten
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
  let expected = E.expected_error_lines_of_files [ file ] in
  E.compare_actual_to_expected_for_alcotest actual expected

let tainting_tests_for_lang files lang =
  files
  |> Common.map (fun file ->
         ( Filename.basename file,
           fun () ->
             let rules_file =
               let d, b, _e = Common2.dbe_of_filename file in
               let candidate1 = Common2.filename_of_dbe (d, b, "yaml") in
               if Sys.file_exists candidate1 then candidate1
               else
                 failwith (spf "could not find tainting rules file for %s" file)
             in
             tainting_test lang rules_file file ))

let lang_tainting_tests () =
  let taint_tests_path = Filename.concat tests_path "tainting_rules" in
  pack_suites "lang tainting rules testing"
    [
      pack_tests "tainting Go"
        (let dir = Filename.concat taint_tests_path "go" in
         let files = Common2.glob (spf "%s/*.go" dir) in
         let lang = Lang.Go in
         tainting_tests_for_lang files lang);
      pack_tests "tainting PHP"
        (let dir = Filename.concat taint_tests_path "php" in
         let files = Common2.glob (spf "%s/*.php" dir) in
         let lang = Lang.Php in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Python"
        (let dir = Filename.concat taint_tests_path "python" in
         let files = Common2.glob (spf "%s/*.py" dir) in
         let lang = Lang.Python in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Java"
        (let dir = Filename.concat taint_tests_path "java" in
         let files = Common2.glob (spf "%s/*.java" dir) in
         let lang = Lang.Java in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Javascript"
        (let dir = Filename.concat taint_tests_path "js" in
         let files = Common2.glob (spf "%s/*.js" dir) in
         let lang = Lang.Js in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Typescript"
        (let dir = Filename.concat taint_tests_path "ts" in
         let files = Common2.glob (spf "%s/*.ts" dir) in
         let lang = Lang.Ts in
         tainting_tests_for_lang files lang);
      pack_tests "tainting Scala"
        (let dir = Filename.concat taint_tests_path "scala" in
         let files = Common2.glob (spf "%s/*.scala" dir) in
         let lang = Lang.Scala in
         tainting_tests_for_lang files lang);
    ]

(*****************************************************************************)
(* Full rule tests *)
(*****************************************************************************)

let full_rule_regression_tests () =
  let path = Filename.concat tests_path "OTHER/rules" in
  pack_tests "full rule"
    (let tests, _print_summary =
       Test_engine.make_tests ~unit_testing:true [ path ]
     in
     tests
     |> Common.map (fun (name, ftest) ->
            let test () =
              Common2.save_excursion_and_enable
                Flag_semgrep.filter_irrelevant_rules (fun () ->
                  logger#info "running with -filter_irrelevant_rules";
                  ftest ())
            in
            (name, test)))

(* quite similar to full_rule_regression_tests but prefer to pack_tests
 * with "full semgrep rule Java", so one can just run the Java tests
 * with ./test Java
 * alt: do like in deep-semgrep and call the toplevel engine
 * in a Unit_runner.ml instead of using Test_engine.make_tests
 *)
let full_rule_semgrep_rules_regression_tests () =
  let path = Filename.concat tests_path "semgrep-rules" in
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
                    (* TODO: parse error, weird *)
                    || s =~ ".*/unicode/security/bidi.yml"
                    || s
                       =~ ".*/javascript/audit/detect-replaceall-sanitization.yaml"
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
                    | _ ->
                        Common2.save_excursion_and_enable
                          Flag_semgrep.filter_irrelevant_rules (fun () ->
                            logger#info "running with -filter_irrelevant_rules";
                            ftest ())
                  in
                  (name, test))))

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests () =
  List.flatten
    [
      (* full testing for many languages *)
      lang_regression_tests ~with_caching:false;
      lang_regression_tests ~with_caching:true;
      eval_regression_tests ();
      filter_irrelevant_rules_tests ();
      lang_tainting_tests ();
      maturity_tests ();
      full_rule_regression_tests ();
      full_rule_semgrep_rules_regression_tests ();
    ]
