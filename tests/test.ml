open Common
open OUnit

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests runner (and a few dumpers) *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)
let verbose = ref false

let dump_ast = ref false

(* ran from _build/default/tests/ hence the '..'s below *)
let tests_path = "../../../tests"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ast_fuzzy_of_string str =
  Common2.with_tmp_file ~str ~ext:"cpp" (fun tmpfile ->
    Parse_cpp.parse_fuzzy tmpfile |> fst
  )

let any_gen_of_string str =
  Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any
  )

(*****************************************************************************)
(* More tests *)
(*****************************************************************************)
let python_regression_tests = 
  "sgrep python" >:: (fun () ->
    let dir = Filename.concat tests_path "python" in
    let files = Common2.glob (spf "%s/*.py" dir) in
    let lang = Lang.Python in
    
    files |> List.iter (fun file ->
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
      let ast = Parse_generic.parse_program file in
      let pattern = 
        Common.save_excursion Flag_parsing.sgrep_mode true (fun () ->
          Parse_generic.parse_pattern lang (Common.read_file sgrep_file)
        )
      in
      Error_code.g_errors := [];
      Sgrep_generic.sgrep_ast
        ~hook:(fun _env matched_tokens ->
          let (minii, _maxii) = Parse_info.min_max_ii_by_pos matched_tokens in
          Error_code.error minii (Error_code.SgrepLint ("",""))
        )
      pattern ast;

      let actual = !Error_code.g_errors in
      let expected = Error_code.expected_error_lines_of_files [file] in
      Error_code.compare_actual_to_expected actual expected;
    )
  )

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let test regexp =
  (* There is no reflection in OCaml so the unit test framework OUnit requires
   * us to explicitely build the test suites (which is not too bad).
   *)
  let tests =
    "all" >::: [

     (* ugly: todo: use a toy fuzzy parser instead of the one in lang_cpp/ *)
      Unit_matcher.sgrep_fuzzy_unittest ~ast_fuzzy_of_string;
      Unit_matcher.sgrep_gen_unittest ~any_gen_of_string;
      python_regression_tests;
      (* TODO Unit_matcher.spatch_unittest ~xxx *)
      (* TODO Unit_matcher_php.unittest; (* sgrep, spatch, refactoring, unparsing *) *)
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

(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)

let dump_ast_file _file =
  raise Todo

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let options = [
  "-verbose", Arg.Set verbose,
  " verbose mode";
  "-dump_ast", Arg.Set dump_ast,
  " <file> dump the generic Abstract Syntax Tree of a file";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let usage =
   Common.spf "Usage: %s [options] [regexp]> \nrun the unit tests matching the regexp\nOptions:"
      (Filename.basename Sys.argv.(0))

let main () =
  let args = ref [] in
  Arg.parse options (fun arg -> args := arg::!args) usage;

  (match List.rev !args with
  | [] -> test "all"
  | [file] when !dump_ast -> dump_ast_file file
  | [x] -> test x
  | _::_::_ ->
    print_string "too many arguments\n";
    Arg.usage options usage;
  )

let _ = main ()
