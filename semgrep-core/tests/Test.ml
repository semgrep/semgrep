(*s: semgrep/tests/Test.ml *)
open Common
open OUnit
module E = Error_code
module P = Pattern_match
module R = Mini_rule
module T = Tainting_rule

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests runner (and a few dumpers) *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
(*s: constant [[Test.verbose]] *)
let verbose = ref false
(*e: constant [[Test.verbose]] *)

(*s: constant [[Test.dump_ast]] *)
(*e: constant [[Test.dump_ast]] *)

(*s: constant [[Test.tests_path]] *)
(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../tests"
(*e: constant [[Test.tests_path]] *)
(*s: constant [[Test.data_path]] *)
let data_path = "../../../data"
(*e: constant [[Test.data_path]] *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*s: function [[Test.ast_fuzzy_of_string]] *)
(*e: function [[Test.ast_fuzzy_of_string]] *)

(*s: function [[Test.any_gen_of_string]] *)
let any_gen_of_string str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any
  )
(*e: function [[Test.any_gen_of_string]] *)

(*s: function [[Test.parse_generic]] *)
(*e: function [[Test.parse_generic]] *)

let parsing_tests_for_lang files lang =
  files |> List.map (fun file ->
    (Filename.basename file) >:: (fun () ->
      let {Parse_target. errors = errs; _ } = 
         Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file in
      if errs <> []
      then failwith (String.concat ";" (List.map Error_code.string_of_error errs));
    )
  )

(*
   For each input file with the language's extension, locate a pattern file
   with the '.sgrep' extension.

   If foo/bar.sgrep is not found, POLYGLOT/bar.sgrep is used instead.
*)
(*s: function [[Test.regression_tests_for_lang]] *)
let regression_tests_for_lang ~with_caching files lang =
  files |> List.map (fun file ->
   (Filename.basename file) >:: (fun () ->
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
    Error_code.g_errors := [];

    let rule = { R.
      id = "unit testing"; pattern; message = ""; 
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
         Error_code.error minii (Error_code.SemgrepMatchFound ("",""))
       )
       Config_semgrep.default_config
       [rule] equiv (file, lang, ast) 
     |> ignore;
  
     let actual = !Error_code.g_errors in
     let expected = Error_code.expected_error_lines_of_files [file] in
       Error_code.compare_actual_to_expected actual expected; 
    )     
   )
 )
(*e: function [[Test.regression_tests_for_lang]] *)

let tainting_test lang rules_file file =
  let rules =
    try
      Parse_tainting_rules.parse rules_file
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
    rules |> List.filter (fun r -> List.mem lang r.T.languages) in
  let matches = Tainting_generic.check rules file ast in
  let actual =
    matches |> List.map (fun m ->
      { E.typ = SemgrepMatchFound(m.P.rule_id.id,m.P.rule_id.message);
        loc   = fst m.range_loc;
        sev   = Error; }
      )
  in
  let expected = Error_code.expected_error_lines_of_files [file] in
  Error_code.compare_actual_to_expected actual expected

let tainting_tests_for_lang files lang =
  files |> List.map (fun file ->
   (Filename.basename file) >:: (fun () ->
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
  "lang parsing testing" >::: [
   (* languages with only a tree-sitter parser *)
    "C#" >::: (
      let dir = Filename.concat (Filename.concat tests_path "csharp") "parsing" in
      let files = Common2.glob (spf "%s/*.cs" dir) in
      let lang = Lang.Csharp in
      parsing_tests_for_lang files lang
    );
    "Lua" >::: (
      let dir = Filename.concat (Filename.concat tests_path "lua") "parsing" in
      let files = Common2.glob (spf "%s/*.lua" dir) in
      let lang = Lang.Lua in
      parsing_tests_for_lang files lang
    );
    "Rust" >::: (
      let dir = Filename.concat (Filename.concat tests_path "rust") "parsing" in
      let files = Common2.glob (spf "%s/*.rs" dir) in
      let lang = Lang.Rust in
      parsing_tests_for_lang files lang
    );
    "Kotlin" >::: (
      let dir = Filename.concat (Filename.concat tests_path "kotlin") "parsing" in
      let files = Common2.glob (spf "%s/*.kt" dir) in
      let lang = Lang.Kotlin in
      parsing_tests_for_lang files lang
    );
    (* here we have both a Pfff and tree-sitter parser *)
    "Java" >::: (
      let dir= Filename.concat (Filename.concat tests_path "java") "parsing" in
      let files = Common2.glob (spf "%s/*.java" dir) in
      let lang = Lang.Java in
      parsing_tests_for_lang files lang
    );
    "Go" >::: (
      let dir= Filename.concat (Filename.concat tests_path "go") "parsing" in
      let files = Common2.glob (spf "%s/*.go" dir) in
      let lang = Lang.Go in
      parsing_tests_for_lang files lang
    );
    "Ruby" >::: (
      let dir = Filename.concat tests_path "ruby/parsing" in
      let files = Common2.glob (spf "%s/*.rb" dir) in
      let lang = Lang.Ruby in
      parsing_tests_for_lang files lang
    );
    "Javascript" >::: (
      let dir = Filename.concat tests_path "js/parsing" in
      let files = Common2.glob (spf "%s/*.js" dir) in
      let lang = Lang.Javascript in
      parsing_tests_for_lang files lang
    );
    "Scala" >::: (
      let dir = Filename.concat tests_path "scala/parsing" in
      let files = Common2.glob (spf "%s/*.scala" dir) in
      let lang = Lang.Scala in
      parsing_tests_for_lang files lang
    );
  ]

(*s: constant [[Test.lang_regression_tests]] *)
let lang_regression_tests ~with_caching =
  let regression_tests_for_lang files lang =
    regression_tests_for_lang ~with_caching files lang
  in
  let name_suffix =
    if with_caching then " with caching"
    else " without caching"
  in
  "lang regression testing" ^ name_suffix >::: [
  "semgrep Python" >::: (
    let dir = Filename.concat tests_path "python" in
    let files = Common2.glob (spf "%s/*.py" dir) in
    let lang = Lang.Python in
    regression_tests_for_lang files lang
  );
  "semgrep Javascript" >::: (
    let dir = Filename.concat tests_path "js" in
    let files = Common2.glob (spf "%s/*.js" dir) in
    let lang = Lang.Javascript in
    regression_tests_for_lang files lang
  );
  "semgrep Typescript" >::: (
    let dir = Filename.concat tests_path "ts" in
    let files = Common2.glob (spf "%s/*.ts" dir) in
    let lang = Lang.Typescript in
    regression_tests_for_lang files lang
  );
  "semgrep Typescript on Javascript (no JSX)" >::: (
    let dir = Filename.concat tests_path "js" in
    let files = Common2.glob (spf "%s/*.js" dir) in
    let files = Common.exclude (fun s -> s =~ ".*xml" || s =~ ".*jsx") files in
    let lang = Lang.Typescript in
    regression_tests_for_lang files lang
  );
  "semgrep JSON" >::: (
    let dir = Filename.concat tests_path "json" in
    let files = Common2.glob (spf "%s/*.json" dir) in
    let lang = Lang.JSON in
    regression_tests_for_lang files lang
  );
  "semgrep Java" >::: (
    let dir = Filename.concat tests_path "java" in
    let files = Common2.glob (spf "%s/*.java" dir) in
    let lang = Lang.Java in
    regression_tests_for_lang files lang
  );
  "semgrep C" >::: (
    let dir = Filename.concat tests_path "c" in
    let files = Common2.glob (spf "%s/*.c" dir) in
    let lang = Lang.C in
    regression_tests_for_lang files lang
  );
  "semgrep Go" >::: (
    let dir = Filename.concat tests_path "go" in
    let files = Common2.glob (spf "%s/*.go" dir) in
    let lang = Lang.Go in
    regression_tests_for_lang files lang
  );
  "semgrep OCaml" >::: (
    let dir = Filename.concat tests_path "ocaml" in
    let files = Common2.glob (spf "%s/*.ml" dir) in
    let lang = Lang.OCaml in
    regression_tests_for_lang files lang
  );
  "semgrep Ruby" >::: (
    let dir = Filename.concat tests_path "ruby" in
    let files = Common2.glob (spf "%s/*.rb" dir) in
    let lang = Lang.Ruby in
    regression_tests_for_lang files lang
  );
  "semgrep PHP" >::: (
    let dir = Filename.concat tests_path "php" in
    let files = Common2.glob (spf "%s/*.php" dir) in
    let lang = Lang.PHP in
    regression_tests_for_lang files lang
  );
  "semgrep C#" >::: (
    let dir = Filename.concat tests_path "csharp" in
    let files = Common2.glob (spf "%s/*.cs" dir) in
    let lang = Lang.Csharp in
    regression_tests_for_lang files lang
  );
  "semgrep Lua" >::: (
    let dir = Filename.concat tests_path "lua" in
    let files = Common2.glob (spf "%s/*.lua" dir) in
    let lang = Lang.Lua in
    regression_tests_for_lang files lang
  );
  "semgrep Rust" >::: (
    let dir = Filename.concat tests_path "rust" in
    let files = Common2.glob (spf "%s/*.rs" dir) in
    let lang = Lang.Rust in
    regression_tests_for_lang files lang
  );
  "semgrep Yaml" >::: (
    let dir = Filename.concat tests_path "yaml" in
    let files = Common2.glob (spf "%s/*.yaml" dir) in
    let lang = Lang.Yaml in
    regression_tests_for_lang files lang
  );
  "semgrep Scala" >::: (
    let dir = Filename.concat tests_path "scala" in
    let files = Common2.glob (spf "%s/*.scala" dir) in
    let lang = Lang.Scala in
    regression_tests_for_lang files lang
  );
 ]
(*e: constant [[Test.lang_regression_tests]] *)

let full_rule_regression_tests =
  "full rule" >:: (fun () ->
      let path = Filename.concat tests_path "OTHER/rules" in
      Test_engine.test_rules ~ounit_context:true [path]
  )

let lang_tainting_tests =
  let taint_tests_path = Filename.concat tests_path "tainting_rules" in
  "lang tainting rules testing" >::: [
    "tainting PHP" >::: (
      let dir = Filename.concat taint_tests_path "php" in
      let files = Common2.glob (spf "%s/*.php" dir) in
      let lang = Lang.PHP in
      tainting_tests_for_lang files lang
    );
    "tainting Python" >::: (
      let dir = Filename.concat taint_tests_path "python" in
      let files = Common2.glob (spf "%s/*.py" dir) in
      let lang = Lang.Python in
      tainting_tests_for_lang files lang
    );
    "tainting Typescript" >::: (
      let dir = Filename.concat taint_tests_path "ts" in
      let files = Common2.glob (spf "%s/*.ts" dir) in
      let lang = Lang.Typescript in
      tainting_tests_for_lang files lang
    );
  ]

(*s: constant [[Test.lint_regression_tests]] *)
(* mostly a copy paste of pfff/linter/unit_linter.ml *)
let lint_regression_tests ~with_caching =
  let name =
    if with_caching then
      "lint regression testing with caching"
    else
      "lint regression testing without caching"
  in
  name >:: (fun () ->
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
    Common.save_excursion Flag_semgrep.with_opt_cache with_caching (fun() ->
      Match_patterns.check ~hook:(fun _ _ -> ())
         Config_semgrep.default_config
         rules equivs 
         (file, lang, ast)
      |> List.iter JSON_report.match_to_error;
    )
  ));

  (* compare *)
  let actual_errors = !E.g_errors in
  if !verbose 
  then actual_errors |> List.iter (fun e -> pr (E.string_of_error e));
  E.compare_actual_to_expected actual_errors expected_error_lines
  )
(*e: constant [[Test.lint_regression_tests]] *)

let eval_regression_tests = 
  "eval regression resting" >:: (fun () ->
      let dir = Filename.concat tests_path "OTHER/eval" in
      let files = Common2.glob (spf "%s/*.json" dir) in
      files |> List.iter (fun file ->
        let (env, code) = Eval_generic.parse_json file in
        let res = Eval_generic.eval env code in
        OUnit.assert_equal ~msg:(spf "%s should evaluate to true" file)
          (Eval_generic.Bool true) res
      )
  )

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

(*s: function [[Test.test]] *)
let test regexp =
  (* There is no reflection in OCaml so the unit test framework OUnit requires
   * us to explicitely build the test suites (which is not too bad).
   *)
  let tests =
    "all" >::: [

      (* just expression vs expression testing for one language (Python) *)
      Unit_matcher.unittest ~any_gen_of_string;
      Unit_synthesizer.unittest;
      Unit_dataflow.unittest Parse_target.parse_program;
      Unit_typing_generic.unittest 
        Parse_target.parse_program 
        (fun lang file -> Parse_pattern.parse_pattern lang file);
      Unit_naming_generic.unittest Parse_target.parse_program;

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
    ]
  in
  let suite =
    if regexp = "all"
    then tests
    else
      let paths =
        OUnit.test_case_paths tests |> List.map OUnit.string_of_path in
      let keep = paths |> List.filter (fun path ->
        path =~ (".*" ^ regexp))
      in
      Common2.some (OUnit.test_filter keep tests)
  in

  let results = OUnit.run_test_tt ~verbose:!verbose suite in
  let has_an_error =
    results |> List.exists (function
    | OUnit.RSuccess _ | OUnit.RSkip _ | OUnit.RTodo _ -> false
    | OUnit.RFailure _ | OUnit.RError _ -> true
    )
  in
  if has_an_error
  then exit 1
  else exit 0
(*e: function [[Test.test]] *)

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)
(*s: function [[Test.ast_generic_of_file]] *)
(*e: function [[Test.ast_generic_of_file]] *)
(*s: function [[Test.dump_ast_generic]] *)
(*e: function [[Test.dump_ast_generic]] *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*s: constant [[Test.options]] *)
let options = [
  "-verbose", Arg.Set verbose,
  " verbose mode";
  ]
(*e: constant [[Test.options]] *)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(*s: constant [[Test.usage]] *)
let usage =
   Common.spf "Usage: %s [options] [regexp]> \nrun the unit tests matching the regexp\nOptions:"
      (Filename.basename Sys.argv.(0))
(*e: constant [[Test.usage]] *)

(*s: function [[Test.main]] *)
let main () =
  let args = ref [] in
  Arg.parse options (fun arg -> args := arg::!args) usage;

  (match List.rev !args with
  | [] -> test "all"
  | [x] -> test x
  | _::_::_ ->
    print_string "too many arguments\n";
    Arg.usage options usage;
  )
(*e: function [[Test.main]] *)

(*s: toplevel [[Test._1]] *)
let _ = main ()
(*e: toplevel [[Test._1]] *)
(*e: semgrep/tests/Test.ml *)
