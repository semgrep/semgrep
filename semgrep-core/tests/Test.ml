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
(* Unit tests runner (and a few dumpers) *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../tests"
let data_path = "../../../data"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)


let any_gen_of_string str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any
  )


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

(*
   For each input file with the language's extension, locate a pattern file
   with the '.sgrep' extension.

   If foo/bar.sgrep is not found, POLYGLOT/bar.sgrep is used instead.
*)
let regression_tests_for_lang ~with_caching files lang =
  files |> List.map (fun file ->
    Filename.basename file, (fun () ->
      let sgrep_file =
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
    let pattern = 
      Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
        try 
          Parse_pattern.parse_pattern lang ~print_errors:true (Common.read_file sgrep_file)
        with exn ->
          failwith (spf "fail to parse pattern %s with lang = %s (exn = %s)" 
                      sgrep_file 
                      (Lang.string_of_lang lang)
                      (Common.exn_to_s exn))
      )
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
        Config_semgrep.default_config
        [rule] equiv (file, lang, ast) 
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
  let matches =
    let equivs = [] in
    Tainting_generic.check
      (fun _ _ _ -> ())
      Config_semgrep.default_config
      taint_rules equivs file lang ast
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
  pack_suites "lang parsing testing" [
   (* languages with only a tree-sitter parser *)
    pack_tests "Bash" (
      let dir = Filename.concat (Filename.concat tests_path "bash") "parsing" in
      let files = Common2.glob (spf "%s/*.bash" dir) in
      let lang = Lang.Bash in
      parsing_tests_for_lang files lang
    );
    pack_tests "C#" (
      let dir = Filename.concat (Filename.concat tests_path "csharp") "parsing" in
      let files = Common2.glob (spf "%s/*.cs" dir) in
      let lang = Lang.Csharp in
      parsing_tests_for_lang files lang
    );
    pack_tests "Lua" (
      let dir = Filename.concat (Filename.concat tests_path "lua") "parsing" in
      let files = Common2.glob (spf "%s/*.lua" dir) in
      let lang = Lang.Lua in
      parsing_tests_for_lang files lang
    );
    pack_tests "Rust" (
      let dir = Filename.concat (Filename.concat tests_path "rust") "parsing" in
      let files = Common2.glob (spf "%s/*.rs" dir) in
      let lang = Lang.Rust in
      parsing_tests_for_lang files lang
    );
    pack_tests "Kotlin" (
      let dir = Filename.concat (Filename.concat tests_path "kotlin") "parsing" in
      let files = Common2.glob (spf "%s/*.kt" dir) in
      let lang = Lang.Kotlin in
      parsing_tests_for_lang files lang
    );
    pack_tests "Hack" (
      let dir = Filename.concat (Filename.concat tests_path "hack") "parsing" in
      let files = Common2.glob (spf "%s/*.hack" dir) in
      let lang = Lang.Hack in
      parsing_tests_for_lang files lang
    );
    (* here we have both a Pfff and tree-sitter parser *)
    pack_tests "Java" (
      let dir= Filename.concat (Filename.concat tests_path "java") "parsing" in
      let files = Common2.glob (spf "%s/*.java" dir) in
      let lang = Lang.Java in
      parsing_tests_for_lang files lang
    );
    pack_tests "Go" (
      let dir= Filename.concat (Filename.concat tests_path "go") "parsing" in
      let files = Common2.glob (spf "%s/*.go" dir) in
      let lang = Lang.Go in
      parsing_tests_for_lang files lang
    );
    pack_tests "Ruby" (
      let dir = Filename.concat tests_path "ruby/parsing" in
      let files = Common2.glob (spf "%s/*.rb" dir) in
      let lang = Lang.Ruby in
      parsing_tests_for_lang files lang
    );
    pack_tests "Javascript" (
      let dir = Filename.concat tests_path "js/parsing" in
      let files = Common2.glob (spf "%s/*.js" dir) in
      let lang = Lang.Javascript in
      parsing_tests_for_lang files lang
    );
    pack_tests "Scala" (
      let dir = Filename.concat tests_path "scala/parsing" in
      let files = Common2.glob (spf "%s/*.scala" dir) in
      let lang = Lang.Scala in
      parsing_tests_for_lang files lang
    );
    pack_tests "HTML" (
      let dir = Filename.concat tests_path "html/parsing" in
      let files = Common2.glob (spf "%s/*.html" dir) in
      let lang = Lang.HTML in
      parsing_tests_for_lang files lang
    );
    pack_tests "Vue" (
      let dir = Filename.concat tests_path "vue/parsing" in
      let files = Common2.glob (spf "%s/*.vue" dir) in
      let lang = Lang.Vue in
      parsing_tests_for_lang files lang
    );
    (* TODO: also do parsing tests where we expect some partials.
     * See cpp/parsing_partial/
     *)
    pack_tests "C++" (
      let dir = Filename.concat tests_path "cpp/parsing" in
      let files = Common2.glob (spf "%s/*.cpp" dir) in
      let lang = Lang.Cplusplus in
      parsing_tests_for_lang files lang
    );
    pack_tests "C++ partial parsing" (
      let dir = Filename.concat tests_path "cpp/parsing_partial" in
      let files = Common2.glob (spf "%s/*.cpp" dir) in
      let lang = Lang.Cplusplus in
      partial_parsing_tests_for_lang files lang
    );
  ]

let lang_regression_tests ~with_caching =
  let regression_tests_for_lang files lang =
    regression_tests_for_lang ~with_caching files lang
  in
  let name_suffix =
    if with_caching then " with caching"
    else " no caching"
  in
  pack_suites ("lang testing" ^ name_suffix) [
  pack_tests "semgrep Bash" (
    let dir = Filename.concat tests_path "bash" in
    let files = Common2.glob (spf "%s/*.bash" dir) in
    let lang = Lang.Bash in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Python" (
    let dir = Filename.concat tests_path "python" in
    let files = Common2.glob (spf "%s/*.py" dir) in
    let lang = Lang.Python in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Javascript" (
    let dir = Filename.concat tests_path "js" in
    let files = Common2.glob (spf "%s/*.js" dir) in
    let lang = Lang.Javascript in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Typescript" (
    let dir = Filename.concat tests_path "ts" in
    let files = Common2.glob (spf "%s/*.ts" dir) in
    let lang = Lang.Typescript in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Typescript on Javascript (no JSX)" (
    let dir = Filename.concat tests_path "js" in
    let files = Common2.glob (spf "%s/*.js" dir) in
    let files = Common.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files in
    let lang = Lang.Typescript in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep JSON" (
    let dir = Filename.concat tests_path "json" in
    let files = Common2.glob (spf "%s/*.json" dir) in
    let lang = Lang.JSON in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Java" (
    let dir = Filename.concat tests_path "java" in
    let files = Common2.glob (spf "%s/*.java" dir) in
    let lang = Lang.Java in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep C" (
    let dir = Filename.concat tests_path "c" in
    let files = Common2.glob (spf "%s/*.c" dir) in
    let lang = Lang.C in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep C++" (
    let dir = Filename.concat tests_path "cpp" in
    let files = Common2.glob (spf "%s/*.cpp" dir) in
    let lang = Lang.Cplusplus in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep C++ on C tests" (
    let dir = Filename.concat tests_path "c" in
    let files = Common2.glob (spf "%s/*.c" dir) in
    let lang = Lang.Cplusplus in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Go" (
    let dir = Filename.concat tests_path "go" in
    let files = Common2.glob (spf "%s/*.go" dir) in
    let lang = Lang.Go in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep OCaml" (
    let dir = Filename.concat tests_path "ocaml" in
    let files = Common2.glob (spf "%s/*.ml" dir) in
    let lang = Lang.OCaml in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Ruby" (
    let dir = Filename.concat tests_path "ruby" in
    let files = Common2.glob (spf "%s/*.rb" dir) in
    let lang = Lang.Ruby in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep PHP" (
    let dir = Filename.concat tests_path "php" in
    let files = Common2.glob (spf "%s/*.php" dir) in
    let lang = Lang.PHP in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Hack" (
    let dir = Filename.concat tests_path "hack" in
    let files = Common2.glob (spf "%s/*.hack" dir) in
    let lang = Lang.Hack in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep C#" (
    let dir = Filename.concat tests_path "csharp" in
    let files = Common2.glob (spf "%s/*.cs" dir) in
    let lang = Lang.Csharp in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Lua" (
    let dir = Filename.concat tests_path "lua" in
    let files = Common2.glob (spf "%s/*.lua" dir) in
    let lang = Lang.Lua in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Rust" (
    let dir = Filename.concat tests_path "rust" in
    let files = Common2.glob (spf "%s/*.rs" dir) in
    let lang = Lang.Rust in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Yaml" (
    let dir = Filename.concat tests_path "yaml" in
    let files = Common2.glob (spf "%s/*.yaml" dir) in
    let lang = Lang.Yaml in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Scala" (
    let dir = Filename.concat tests_path "scala" in
    let files = Common2.glob (spf "%s/*.scala" dir) in
    let lang = Lang.Scala in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep HTML" (
    let dir = Filename.concat tests_path "html" in
    let files = Common2.glob (spf "%s/*.html" dir) in
    let lang = Lang.HTML in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Vue" (
    let dir = Filename.concat tests_path "vue" in
    let files = Common2.glob (spf "%s/*.vue" dir) in
    let lang = Lang.Vue in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep HCL" (
    let dir = Filename.concat tests_path "hcl" in
    let files = Common2.glob (spf "%s/*.tf" dir) in
    let lang = Lang.HCL in
    regression_tests_for_lang files lang
  );
  pack_tests "semgrep Kotlin" (
    let dir = Filename.concat tests_path "kotlin" in
    let files = Common2.glob (spf "%s/*.kt" dir) in
    let lang = Lang.Kotlin in
    regression_tests_for_lang files lang
  );
 ]

let full_rule_regression_tests = [
  "full rule", (fun () ->
    let path = Filename.concat tests_path "OTHER/rules" in
    Common2.save_excursion_and_enable Flag_semgrep.filter_irrelevant_rules (fun () ->
    logger#info "running with -filter_irrelevant_rules";  
    Test_engine.test_rules ~unit_testing:true [path])
  )
]

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
      let lang = Lang.PHP in
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
      let lang = Lang.Javascript in
      tainting_tests_for_lang files lang
    );
    pack_tests "tainting Typescript" (
      let dir = Filename.concat taint_tests_path "ts" in
      let files = Common2.glob (spf "%s/*.ts" dir) in
      let lang = Lang.Typescript in
      tainting_tests_for_lang files lang
    );
  ]

(* mostly a copy paste of pfff/linter/unit_linter.ml *)
let lint_regression_tests ~with_caching =
  let name =
    if with_caching then
      "lint regression testing with caching"
    else
      "lint regression testing without caching"
  in
  [
    name, (fun () ->
      let p path = Filename.concat tests_path path in
      let rule_file = Filename.concat data_path "basic.yml" in
      let lang = Lang.Python in

      let test_files = [
        p "OTHER/mini_rules/stupid.py";
      ] in

      (* expected *)
      let expected_error_lines = E.expected_error_lines_of_files test_files in

      (* actual *)
      E.g_errors := [];
      let rules = Parse_mini_rule.parse rule_file in
      let equivs = [] in

      test_files |> List.iter (fun file ->
        E.try_with_exn_to_error file (fun () ->
          let { Parse_target. ast; _} = 
            Parse_target.just_parse_with_lang lang file in
          Common.save_excursion
            Flag_semgrep.with_opt_cache with_caching (fun() ->
              Match_patterns.check ~hook:(fun _ _ -> ())
                Config_semgrep.default_config
                rules equivs 
                (file, lang, ast)
              |> List.iter JSON_report.match_to_error;
            )
        ));

      (* compare *)
      let actual_errors = !E.g_errors in
      actual_errors |> List.iter (fun e -> pr (E.string_of_error e));
      compare_actual_to_expected actual_errors expected_error_lines
    )
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

let test_irrelevant_rule_file rule_file =
  Filename.basename rule_file, (fun () ->
    let target_file =
      let (d,b,_e) = Common2.dbe_of_filename rule_file in
      (* TODO: Support other extensions, note that we don't need
       * to parse the target files! *)
      let candidate1 = Common2.filename_of_dbe (d,b,"py") in
      if Sys.file_exists candidate1
      then candidate1
      else failwith (spf "could not find target file for irrelevant rule %s" rule_file)
    in
    test_irrelevant_rule rule_file target_file
  )

let filter_irrelevant_rules_tests =
  pack_tests "filter irrelevant rules testing" (
    let dir = Filename.concat tests_path "OTHER/irrelevant_rules" in
    let files = Common2.glob (spf "%s/*.yaml" dir) in
    files |> List.map (fun file ->
      test_irrelevant_rule_file file
    )
  )

(*                 *)

let tests = List.flatten [
  (* just expression vs expression testing for one language (Python) *)
  Unit_matcher.tests ~any_gen_of_string;
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
  (* full testing for many languages *)
  lang_regression_tests ~with_caching:false;
  lang_regression_tests ~with_caching:true;
  (* TODO Unit_matcher.spatch_unittest ~xxx *)
  (* TODO Unit_matcher_php.unittest; (* sgrep, spatch, refactoring, unparsing *) *)
  lint_regression_tests ~with_caching:false;
  lint_regression_tests ~with_caching:true;
  eval_regression_tests;
  full_rule_regression_tests;
  lang_tainting_tests;
  metachecker_checks_tests;
  metachecker_regression_tests;
  filter_irrelevant_rules_tests;
]

let main () =
  let alcotest_tests = Testutil.to_alcotest tests in
  Alcotest.run "semgrep-core" alcotest_tests

let () = main ()
