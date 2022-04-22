open Common
open Testutil
module E = Semgrep_error_code
module P = Pattern_match
module R = Rule
module MR = Mini_rule

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests entry point.
 *
 * From semgrep-core you can do
 *
 *   $./test test foo
 *
 * to run all the tests containing foo in their description.
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../tests"
let data_path = "../../../data"

(* coupling: https://semgrep.dev/docs/language-support/
 * See also https://r2c.quip.com/FOAuA4ThzULc/How-to-promote-a-language-
*)
type maturity_level = GA | Beta | Experimental
[@@deriving show { with_path = false }]

(* coupling: 
 * https://semgrep.dev/docs/language-support/#maturity-definitions 
 * ../../scripts/generate-cheatsheet.py
*)
let experimental_features = [
    "concrete_syntax";
    "deep_exprstmt";
    "dots_args";
    "dots_nested_stmts";
    "dots_stmts";
    "dots_string";
    "metavar_arg";
    "metavar_call";
    "metavar_equality_var"
    (* TODO: add dots_params? *)
]

let beta_features = experimental_features @ [
    "metavar_class_def";
    "metavar_func_def";
    "metavar_cond";
    "metavar_equality_expr";
    "metavar_equality_stmt";
    "metavar_import";
    "metavar_stmt";
]

let ga_features = beta_features @ [
    "deep_expr_operator";
    "dots_method_chaining";
    "equivalence_constant_propagation";
    "equivalence_eq";
    "equivalence_naming_import";
    "metavar_anno";
    "metavar_key_value";
    "metavar_typed";
    "regexp_string";
]

let assoc_maturity_level = [
    GA, ga_features;
    Beta, beta_features;
    Experimental, experimental_features
]

(* coupling: ../../scripts/generate_cheatsheet.py LANGUAGE_EXCEPTIONS
 * Note that for some languages, e.g., JSON, certain tests do not
 * apply (NA), hence the exceptions listed above.
 * For others, we should really add the test and/or corresponding feature.
*)
let language_exceptions = [
    (* GA languages *)

    (* TODO: NA for Java? *)
    Lang.Java, 
    ["equivalence_naming_import"; "metavar_key_value"];
    (* TODO: why not metavar_typed? regexp_string? NA for naming_import? *)
    Lang.Csharp, 
    ["equivalence_naming_import"; "metavar_typed"; "regexp_string"];
    (* TODO: metavar_anno sounds like an NA, but the other?? *)
    Lang.Go, 
    ["metavar_class_def"; "metavar_import"; "metavar_anno"];
    (* metavar_typed is NA (dynamic language) *)
    Lang.Js,
    ["equivalence_naming_import"; "metavar_typed";];
    Lang.Ts,
    ["equivalence_naming_import"; "metavar_typed";"metavar_anno";"metavar_class_def"];
    (* good boy, metavar_typed is working just for constants though *)
    Lang.Python, [];
    (* metavar_typed is NA (dynamic language), metavar_anno also NA? *)
    Lang.Ruby, 
    ["equivalence_naming_import"; "metavar_typed";"metavar_anno"];

    (* Beta languages *)

    (* TODO: to fix *)
    Lang.Kotlin, 
    ["dots_stmts"; "metavar_equality_var"];

    (* Experimental languages *)

    (* TODO: dots_nested_stmts to fix for C and C++ *)
    Lang.C,
    ["dots_nested_stmts"];
    Lang.Cpp,
    ["dots_nested_stmts"];
    (* good boy *)
    Lang.Lua, [];
    (* dots_stmts is maybe NA, same with deep_exprstmt *)
    Lang.Ocaml,
    ["deep_exprstmt";"dots_stmts"];
    (* good boy *)
    Lang.Php, [];

    (* good boy, this feature has been deprecated *)
    Lang.Scala, ["regexp_string"]
]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let any_gen_of_string str =
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any


let parsing_tests_for_lang files lang =
  files |> List.map (fun file ->
    Filename.basename file, (fun () ->
      let {Parse_target. errors = errs; _ } = 
        Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file in
      if errs <> []
      then failwith (String.concat ";" (List.map E.string_of_error errs));
    )
  )

let partial_parsing_tests_for_lang files lang =
  files |> List.map (fun file ->
    Filename.basename file, (fun () ->
      let {Parse_target. errors = errs; _ }  = 
        Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file in
      if errs = []
      then failwith "it should parse partially the file (with some errors)"
    )
  )

let compare_actual_to_expected actual expected =
  match E.compare_actual_to_expected actual expected with
  | Ok () -> ()
  | Error (_num_errors, msg) -> Alcotest.fail msg


let sgrep_file_of_target file =
  let (d,b,_e) = Common2.dbe_of_filename file in
  let candidate1 = Common2.filename_of_dbe (d,b,"sgrep") in
  if Sys.file_exists candidate1
  then candidate1
  else
    let d = Filename.concat tests_path "POLYGLOT" in
    let candidate2 = Common2.filename_of_dbe (d,b,"sgrep") in
    if Sys.file_exists candidate2
    then candidate2
    else failwith (spf "could not find sgrep file for %s" file)

(*
   For each input file with the language's extension, locate a pattern file
   with the '.sgrep' extension.

   If foo/bar.sgrep is not found, POLYGLOT/bar.sgrep is used instead.
*)
let regression_tests_for_lang ~with_caching files lang =
  files |> List.map (fun file ->
    Filename.basename file, (fun () -> 
    let sgrep_file = sgrep_file_of_target file in
    let ast = 
      try 
        let { Parse_target. ast; errors; _ } = 
          Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file 
        in
        if errors <> []
        then pr2 (spf "WARNING: fail to fully parse %s" file);
        ast
      with exn ->
        failwith (spf "fail to parse %s (exn = %s)" file 
                    (Common.exn_to_s exn))
    in
    let pattern = 
        try 
          Parse_pattern.parse_pattern lang ~print_errors:true (Common.read_file sgrep_file)
        with exn ->
          failwith (spf "fail to parse pattern %s with lang = %s (exn = %s)" 
                      sgrep_file 
                      (Lang.to_string lang)
                      (Common.exn_to_s exn))
    in
    E.g_errors := [];

    let rule = { MR.
                 id = "unit testing"; pattern; inside=false; message = ""; 
                 severity = R.Error; languages = [lang];
                 pattern_string = "test: no need for pattern string";
               } in
    let equiv = 
      (* Python == is not the same than !(==) *)
      if lang <> Lang.Python
      then Parse_equivalences.parse 
          (Filename.concat data_path "basic_equivalences.yml")
      else []
    in
    Common.save_excursion Flag_semgrep.with_opt_cache with_caching (fun() ->
      Match_patterns.check
        ~hook:(fun _env matched_tokens ->
          (* there are a few fake tokens in the generic ASTs now (e.g., 
           * for DotAccess generated outside the grammar) *)
          let xs = Lazy.force matched_tokens in
          let toks = xs |> List.filter Parse_info.is_origintok in
          let (minii, _maxii) = Parse_info.min_max_ii_by_pos toks in
          let minii_loc = Parse_info.unsafe_token_location_of_info minii in
          E.error "test pattern" minii_loc "" (E.SemgrepMatchFound "")
        )
        (Config_semgrep.default_config, equiv)
        [rule] (file, lang, ast) 
      |> ignore;
      let actual = !E.g_errors in
      let expected = E.expected_error_lines_of_files [file] in
      compare_actual_to_expected actual expected; 
    )
    )
  )

let tainting_test lang rules_file file =
  let rules =
    try
      Parse_rule.parse rules_file
    with exn ->
      failwith (spf "fail to parse tainting rules %s (exn = %s)"
                  rules_file
                  (Common.exn_to_s exn))
  in
  let ast = 
    try 
      let { Parse_target. ast; errors; _ } = 
        Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file 
      in
      if errors <> []
      then pr2 (spf "WARNING: fail to fully parse %s" file);
      ast
    with exn ->
      failwith (spf "fail to parse %s (exn = %s)" file 
                  (Common.exn_to_s exn))
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
  let matches = taint_rules |> List.map (fun (rule, taint_spec) ->
    let equivs = [] in
    let xtarget = { Xtarget.file;
        xlang = Xlang.L (lang, []);
        lazy_content = lazy (Common.read_file file);
        lazy_ast_and_errors = lazy (ast, []);
    } in
    let res, _debug = Match_tainting_rules.check_rule rule
        (fun _ _ _ -> ())
        (Config_semgrep.default_config, equivs)
        taint_spec xtarget
     in
     res.matches
     ) |> List.flatten
  in
  let actual =
    matches |> List.map (fun m ->
      { rule_id = Some m.P.rule_id.id;
        E.typ = SemgrepMatchFound m.P.rule_id.id;
        loc   = fst m.range_loc;
        msg   = m.P.rule_id.message;
        details = None;
        yaml_path = None }
    )
  in
  let expected = E.expected_error_lines_of_files [file] in
  compare_actual_to_expected actual expected

let tainting_tests_for_lang files lang =
  files |> List.map (fun file ->
    Filename.basename file, (fun () ->
      let rules_file =
        let (d,b,_e) = Common2.dbe_of_filename file in
        let candidate1 = Common2.filename_of_dbe (d,b,"yaml") in
        if Sys.file_exists candidate1
        then candidate1
        else failwith (spf "could not find tainting rules file for %s" file)
      in
      tainting_test lang rules_file file
    ))

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* This differs from pfff/tests/<lang>/parsing because here we also use
 * tree-sitter to parse; certain files do not parse with pfff but parses here
 *)
let lang_parsing_tests =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let pack_parsing_tests_for_lang lang dir ext =
    let slang = Lang.show lang in
    pack_tests slang (
      let dir = Filename.concat (Filename.concat tests_path dir) "parsing" in
      let files = Common2.glob (spf "%s/*%s" dir ext) in
      parsing_tests_for_lang files lang
      )
  in
  pack_suites "lang parsing testing" [
   (* languages with only a tree-sitter parser *)
    pack_parsing_tests_for_lang Lang.Bash "bash" ".bash";
    pack_parsing_tests_for_lang Lang.Csharp "csharp" ".cs";
    pack_parsing_tests_for_lang Lang.Dockerfile "dockerfile" ".dockerfile";
    pack_parsing_tests_for_lang Lang.Lua "lua" ".lua";
    pack_parsing_tests_for_lang Lang.Rust "rust" ".rs";
    pack_parsing_tests_for_lang Lang.Swift "swift" ".swift";
    pack_parsing_tests_for_lang Lang.Kotlin "kotlin" ".kt";
    pack_parsing_tests_for_lang Lang.Hack "hack" ".hack";
    (* here we have both a Pfff and tree-sitter parser *)
    pack_parsing_tests_for_lang Lang.Java "java" ".java";
    pack_parsing_tests_for_lang Lang.Go "go" ".go";
    pack_parsing_tests_for_lang Lang.Ruby "ruby" ".rb";
    pack_parsing_tests_for_lang Lang.Js "js" ".js";
    pack_parsing_tests_for_lang Lang.Scala "scala" ".scala";
    pack_parsing_tests_for_lang Lang.Html "html" ".html";
    pack_parsing_tests_for_lang Lang.Vue "vue" ".vue";
    pack_parsing_tests_for_lang Lang.Cpp "cpp" ".cpp";
    (* a few parsing tests where we expect some partials
     * See cpp/parsing_partial/
     *)
    pack_tests "C++ partial parsing" (
      let dir = Filename.concat tests_path "cpp/parsing_partial" in
      let files = Common2.glob (spf "%s/*.cpp" dir) in
      let lang = Lang.Cpp in
      partial_parsing_tests_for_lang files lang
    );
  ]

let lang_regression_tests ~with_caching =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let pack_regression_tests_for_lang lang dir ext =
    pack_tests (spf "semgrep %s" (Lang.show lang)) (
    let dir = Filename.concat tests_path dir in
    let files = Common2.glob (spf "%s/*%s" dir ext) in
    regression_tests_for_lang ~with_caching files lang
    )
  in
  let regression_tests_for_lang files lang =
    regression_tests_for_lang ~with_caching files lang
  in
  let name_suffix =
    if with_caching then " with caching"
    else " no caching"
  in
  pack_suites ("lang testing" ^ name_suffix) [
    pack_regression_tests_for_lang Lang.Bash "bash" ".bash";
    pack_regression_tests_for_lang Lang.Dockerfile "dockerfile" ".dockerfile";
    pack_regression_tests_for_lang Lang.Python "python" ".py";
    pack_regression_tests_for_lang Lang.Js "js" ".js";
    pack_regression_tests_for_lang Lang.Ts "ts" ".ts";
    pack_tests "semgrep Typescript on Javascript (no JSX)" (
      let dir = Filename.concat tests_path "js" in
      let files = Common2.glob (spf "%s/*.js" dir) in
      let files = Common.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files in
      let lang = Lang.Ts in
      regression_tests_for_lang files lang
    );
    pack_regression_tests_for_lang Lang.Json "json" ".json";
    pack_regression_tests_for_lang Lang.Java "java" ".java";
    pack_regression_tests_for_lang Lang.C "c" ".c";
    pack_regression_tests_for_lang Lang.Cpp "cpp" ".cpp";
    pack_tests "semgrep C++ on C tests" (
      let dir = Filename.concat tests_path "c" in
      let files = Common2.glob (spf "%s/*.c" dir) in
      let lang = Lang.Cpp in
      regression_tests_for_lang files lang
    );
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
    pack_regression_tests_for_lang Lang.Html "html" ".html";
    pack_regression_tests_for_lang Lang.Vue "vue" ".vue";
    pack_regression_tests_for_lang Lang.Hcl "hcl" ".tf";
    pack_regression_tests_for_lang Lang.Kotlin "kotlin" ".kt";
    pack_regression_tests_for_lang Lang.Solidity "solidity" ".sol";
 ]

let full_rule_regression_tests =
  let path = Filename.concat tests_path "OTHER/rules" in
  pack_tests "full rule" (
    let tests, _print_summary =
      Test_engine.make_tests ~unit_testing:true [path]
    in
    tests |> Common.map (fun (name, test) ->
      let test () =
        Common2.save_excursion_and_enable Flag_semgrep.filter_irrelevant_rules
          (fun () ->
             logger#info "running with -filter_irrelevant_rules";
             test ()
          )
      in
      name, test
    )
  )

let lang_tainting_tests =
  let taint_tests_path = Filename.concat tests_path "tainting_rules" in
  pack_suites "lang tainting rules testing" [
    pack_tests "tainting Go" (
      let dir = Filename.concat taint_tests_path "go" in
      let files = Common2.glob (spf "%s/*.go" dir) in
      let lang = Lang.Go in
      tainting_tests_for_lang files lang
    );
    pack_tests "tainting PHP" (
      let dir = Filename.concat taint_tests_path "php" in
      let files = Common2.glob (spf "%s/*.php" dir) in
      let lang = Lang.Php in
      tainting_tests_for_lang files lang
    );
    pack_tests "tainting Python" (
      let dir = Filename.concat taint_tests_path "python" in
      let files = Common2.glob (spf "%s/*.py" dir) in
      let lang = Lang.Python in
      tainting_tests_for_lang files lang
    );
    pack_tests "tainting Java" (
      let dir = Filename.concat taint_tests_path "java" in
      let files = Common2.glob (spf "%s/*.java" dir) in
      let lang = Lang.Java in
      tainting_tests_for_lang files lang
    );
    pack_tests "tainting Javascript" (
      let dir = Filename.concat taint_tests_path "js" in
      let files = Common2.glob (spf "%s/*.js" dir) in
      let lang = Lang.Js in
      tainting_tests_for_lang files lang
    );
    pack_tests "tainting Typescript" (
      let dir = Filename.concat taint_tests_path "ts" in
      let files = Common2.glob (spf "%s/*.ts" dir) in
      let lang = Lang.Ts in
      tainting_tests_for_lang files lang
    );
    pack_tests "tainting Scala" (
      let dir = Filename.concat taint_tests_path "scala" in
      let files = Common2.glob (spf "%s/*.scala" dir) in
      let lang = Lang.Scala in
      tainting_tests_for_lang files lang
    );
  ]

let eval_regression_tests = [
  "eval regression testing", (fun () ->
    let dir = Filename.concat tests_path "OTHER/eval" in
    let files = Common2.glob (spf "%s/*.json" dir) in
    files |> List.iter (fun file ->
      let (env, code) = Eval_generic.parse_json file in
      let res = Eval_generic.eval env code in
      Alcotest.(check bool) (spf "%s should evaluate to true" file)
        true (Eval_generic.Bool true = res)
    )
  )
]

(* alt: we could split the purely-syntactical parsing error checks
 * from the metachecker checks, but simpler to consider all of that
 * as just errors.
 *)
let metachecker_checks_tests =
  pack_tests "metachecker checks testing" (
    let dir = Filename.concat tests_path "OTHER/errors" in
    let files = Common2.glob (spf "%s/*.yaml" dir) in
    files |> List.map (fun file ->
      Filename.basename file, (fun () ->
        E.g_errors := [];
        E.try_with_exn_to_error file (fun () ->
            let rules = Parse_rule.parse file in
            rules |> List.iter (fun rule ->
              let errs = Check_rule.check rule in
              E.g_errors := errs @ !E.g_errors
            )
        );
        let actual = !E.g_errors in
        let expected = E.expected_error_lines_of_files [file] in
        compare_actual_to_expected actual expected;
      )
    )
  )

(* Test the entire `-test_check` path *)
  let metachecker_regression_tests = [
  "metachecker regresion testing", (fun () ->
    let path = Filename.concat tests_path "OTHER/metachecks" in
    Common2.save_excursion_and_enable Flag_semgrep.filter_irrelevant_rules (fun () ->
    Test_metachecking.test_rules ~unit_testing:true [path])
  )
]

let test_irrelevant_rule rule_file target_file =
  let rules = Parse_rule.parse rule_file in
  rules |> List.iter (fun rule ->
    match Analyze_rule.regexp_prefilter_of_rule rule with
    | None -> Alcotest.fail (spf "Rule %s: no regex prefilter formula" (fst rule.id))
    | Some (re, f) ->
      let content = read_file target_file in
      if f content then
        Alcotest.fail (spf "Rule %s considered relevant by regex prefilter: %s" (fst rule.id) re)
  )
  
let test_irrelevant_rule_file target_file =
  Filename.basename target_file, (fun () ->
    let rules_file =
      let (d,b,_e) = Common2.dbe_of_filename target_file in
      let candidate1 = Common2.filename_of_dbe (d,b,"yaml") in
      if Sys.file_exists candidate1
      then candidate1
      else failwith (spf "could not find target file for irrelevant rule %s" target_file)
    in
    test_irrelevant_rule rules_file target_file
  )

(* These tests test that semgrep with filter_irrelevant_rules correctly 
   does not run files when they lack necessary strings.

   To test that filter_irrelevant_rules does not mistakenly filter out 
   any files, place the rule/target pair in the rules folder but annotate 
   in a comment that the test targets filter_irrelevant_rules to help
   future debuggers. *)
let filter_irrelevant_rules_tests =
  pack_tests "filter irrelevant rules testing" (
    let dir = Filename.concat tests_path "OTHER/irrelevant_rules" in
    let target_files = 
    Common2.glob (spf "%s/*" dir)
    |> File_type.files_of_dirs_or_files (function
         | File_type.Config File_type.Yaml -> false
         | _ -> true (* TODO include .test.yaml*))
    in
    target_files |> List.map (fun target_file ->
      test_irrelevant_rule_file target_file
    )
  )

let maturity_tests =
  (* TODO: infer dir and ext from lang using Lang helper functions *)
  let check_maturity lang dir ext maturity =
    pack_tests (spf "Maturity %s for %s" 
        (show_maturity_level maturity)
        (Lang.show lang)) (
      let dir = Filename.concat tests_path dir in
      let features = 
        assoc_maturity_level |> List.assoc maturity in
      let exns = 
        try
          List.assoc lang language_exceptions 
        with Not_found -> []
      in
      let features = Common2.minus_set features exns in
      features |> List.map (fun base ->
         base, (fun () ->
           let path = Filename.concat dir (base ^ ext) in
           (* if it's a does-not-apply (NA) case, consider adding it
            * to language_exceptions above
            *)
           if not (Sys.file_exists path)
           then failwith (spf "missing test file %s for maturity %s" 
                  path (show_maturity_level maturity))
         )
      )
     )
  in
  (* coupling: https://semgrep.dev/docs/language-support/ *)
  pack_suites "Maturity level testing" [
    (* GA *)
    check_maturity Lang.Csharp "csharp" ".cs" GA;
    check_maturity Lang.Go "go" ".go" GA;
    check_maturity Lang.Java "java" ".java" GA;
    check_maturity Lang.Js "js" ".js" GA;
    (* JSON has too many NA, not worth it *)
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
    check_maturity Lang.Php "php" ".php" Experimental;
    (* TODO we say we support R, but not really actually *)
    (* TODO: too many exns, we need to write tests!
     check_maturity Lang.Rust "rust" ".rust" Experimental;
    *)
    check_maturity Lang.Solidity "solidity" ".sol" Experimental;
    (* YAML has too many NA, not worth it *)

    (* Not even experimental *)
    (* R, HTML, Vue *)
  ]

(* It's important that our parsers generate classic parsing errors
 * exns (e.g., Parsing_error, Lexical_error), otherwise semgrep
 * will report some "Fatal error" and abort.
 *)
let parsing_error_tests =
  let dir = Filename.concat tests_path "OTHER/parsing_errors" in
  pack_tests "Parsing error detection" (
    let tests = Common2.glob (spf "%s/*" dir) in
    tests |> List.map (fun file ->
      Filename.basename file, (fun () ->
         try
          let lang = List.hd (Lang.langs_of_filename file) in
          let res = Parse_target.just_parse_with_lang lang file in
          if res.Parse_target.errors = []
          then 
           failwith "it should raise a standard parsing error exn or return partial errors "
         with
         | Parse_info.Lexical_error _
         | Parse_info.Parsing_error _
           -> ()
     )
   )
  )

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

let tests = List.flatten [
  (* just expression vs expression testing for one language (Python) *)
  Unit_matcher.tests ~any_gen_of_string;
  Unit_entropy.tests;
  Unit_ReDoS.tests;
  Unit_synthesizer.tests;
  Unit_synthesizer_targets.tests;
  Unit_dataflow.tests Parse_target.parse_program;
  Unit_typing_generic.tests
    Parse_target.parse_program
    (fun lang file -> Parse_pattern.parse_pattern lang file);
  Unit_naming_generic.tests Parse_target.parse_program;
  Unit_guess_lang.tests;
  Unit_memory_limit.tests;
  Unit_pcre_settings.tests;

  lang_parsing_tests;
  parsing_error_tests;
  (* full testing for many languages *)
  lang_regression_tests ~with_caching:false;
  lang_regression_tests ~with_caching:true;
  (* TODO Unit_matcher.spatch_unittest ~xxx *)
  (* TODO Unit_matcher_php.unittest; (* sgrep, spatch, refactoring, unparsing *) *)
  eval_regression_tests;
  full_rule_regression_tests;
  lang_tainting_tests;
  metachecker_checks_tests;
  metachecker_regression_tests;
  filter_irrelevant_rules_tests;
  maturity_tests;
]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main () =
  let alcotest_tests = Testutil.to_alcotest tests in
  Alcotest.run "semgrep-core" alcotest_tests

let () = main ()
