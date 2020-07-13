(*s: semgrep/tests/Test.ml *)
open Common
open OUnit
module E = Error_code
module R = Rule

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
let dump_ast = ref false
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
let ast_fuzzy_of_string str =
  Common2.with_tmp_file ~str ~ext:"cpp" (fun tmpfile ->
    Parse_cpp.parse_fuzzy tmpfile |> fst
  )
(*e: function [[Test.ast_fuzzy_of_string]] *)

(*s: function [[Test.any_gen_of_string]] *)
let any_gen_of_string str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any
  )
(*e: function [[Test.any_gen_of_string]] *)

(*s: function [[Test.parse_generic]] *)
let parse_generic file = 
  let lang = List.hd (Lang.langs_of_filename file) in
  Parse_code.parse_and_resolve_name_use_pfff_or_treesitter lang file
(*e: function [[Test.parse_generic]] *)

(*s: function [[Test.regression_tests_for_lang]] *)
let regression_tests_for_lang files lang = 
  files |> List.map (fun file ->
   (Filename.basename file) >:: (fun () ->
    let sgrep_file =
      let (d,b,_e) = Common2.dbe_of_filename file in
      let candidate1 = Common2.filename_of_dbe (d,b,"sgrep") in
      if Sys.file_exists candidate1
      then candidate1
      else 
        let d = Filename.concat tests_path "GENERIC" in
        let candidate2 = Common2.filename_of_dbe (d,b,"sgrep") in
        if Sys.file_exists candidate2
        then candidate2
        else failwith (spf "could not find sgrep file for %s" file)
    in
    let ast = 
        try 
            parse_generic file 
        with exn ->
          failwith (spf "fail to parse %s (exn = %s)" file 
                    (Common.exn_to_s exn))
    in
    let pattern = 
      Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
        try 
          Parse_generic.parse_pattern lang (Common.read_file sgrep_file)
        with exn ->
          failwith (spf "fail to parse pattern %s with lang = %s (exn = %s)" 
                        sgrep_file 
                      (Lang.string_of_lang lang)
                    (Common.exn_to_s exn))
      )
    in
    Error_code.g_errors := [];

    let rule = { R.
      id = "unit testing"; pattern; message = ""; severity = R.Error; 
      languages = [lang] } in
    let equiv = 
     (* Python == is not the same than !(==) *)
     if lang <> Lang.Python
     then Parse_equivalences.parse 
       (Filename.concat data_path "basic_equivalences.yml")
     else []
    in
    Semgrep_generic.check
      ~hook:(fun _env matched_tokens ->
      (* there are a few fake tokens in the generic ASTs now (e.g., 
       * for DotAccess generated outside the grammar) *)
        let xs = Lazy.force matched_tokens in
        let toks = xs |> List.filter Parse_info.is_origintok in
        let (minii, _maxii) = Parse_info.min_max_ii_by_pos toks in
        Error_code.error minii (Error_code.SgrepLint ("",""))
      )
      [rule] equiv file lang ast |> ignore;

    let actual = !Error_code.g_errors in
    let expected = Error_code.expected_error_lines_of_files [file] in
      Error_code.compare_actual_to_expected actual expected; 
   )
 )
(*e: function [[Test.regression_tests_for_lang]] *)

(*****************************************************************************)
(* More tests *)
(*****************************************************************************)
(*s: constant [[Test.lang_regression_tests]] *)
let lang_regression_tests = 
 "lang regression testing" >::: [
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
 ]
(*e: constant [[Test.lang_regression_tests]] *)

(*s: constant [[Test.lint_regression_tests]] *)
(* mostly a copy paste of pfff/linter/unit_linter.ml *)
let lint_regression_tests = 
  "lint regression testing" >:: (fun () ->
  let p path = Filename.concat tests_path path in
  let rule_file = Filename.concat data_path "basic.yml" in
  let lang = Lang.Python in

  let test_files = [
   p "lint/stupid.py";
  ] in
  
  (* expected *)
  let expected_error_lines = E.expected_error_lines_of_files test_files in

  (* actual *)
  E.g_errors := [];
  let rules = Parse_rules.parse rule_file in
  let equivs = [] in

  test_files |> List.iter (fun file ->
    E.try_with_exn_to_error file (fun () ->
    let ast = Parse_generic.parse_with_lang lang file in
    Semgrep_generic.check ~hook:(fun _ _ -> ()) rules equivs file lang ast 
      |> List.iter JSON_report.match_to_error;
  ));

  (* compare *)
  let actual_errors = !E.g_errors in
  if !verbose 
  then actual_errors |> List.iter (fun e -> pr (E.string_of_error e));
  E.compare_actual_to_expected actual_errors expected_error_lines
  )
(*e: constant [[Test.lint_regression_tests]] *)

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
      (* full testing for many languages *)
      lang_regression_tests;
      (* ugly: todo: use a toy fuzzy parser instead of the one in lang_cpp/ *)
      Unit_fuzzy.sgrep_fuzzy_unittest ~ast_fuzzy_of_string;
      (* TODO Unit_matcher.spatch_unittest ~xxx *)
      (* TODO Unit_matcher_php.unittest; (* sgrep, spatch, refactoring, unparsing *) *)
      lint_regression_tests;
      Unit_files.unittest;
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
module FT = File_type

(*s: function [[Test.ast_generic_of_file]] *)
let ast_generic_of_file file =
 let typ = File_type.file_type_of_file file in
 match typ with
 | FT.PL (FT.Web (FT.Js)) ->
    let cst = Parse_js.parse_program file in
    let ast = Ast_js_build.program cst in
    Js_to_generic.program ast
 | FT.PL (FT.Python) ->
    let ast = Parse_python.parse_program file in
    Resolve_python.resolve ast;
    Python_to_generic.program ast
 | _ -> failwith (spf "file type not supported for %s" file)
(*e: function [[Test.ast_generic_of_file]] *)

(*s: function [[Test.dump_ast_generic]] *)
(* copy paste of code in pfff/main_test.ml *)
let dump_ast_generic file =
  let ast = ast_generic_of_file file in
  let v = Meta_AST.vof_any (AST_generic.Pr ast) in
  let s = OCaml.string_of_v v in
  pr2 s
(*e: function [[Test.dump_ast_generic]] *)

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

(*s: constant [[Test.options]] *)
let options = [
  "-verbose", Arg.Set verbose,
  " verbose mode";
  "-dump_ast", Arg.Set dump_ast,
  " <file> dump the generic Abstract Syntax Tree of a file";
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
  | [file] when !dump_ast -> dump_ast_generic file
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
