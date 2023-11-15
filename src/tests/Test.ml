(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(* Unit tests entry point.
 *
 * From semgrep-core you can do
 *
 *   $./test foo
 *
 * to run all the tests containing foo in their description.
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let any_gen_of_string str =
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
(* This file used to contain lots of tests, but it's better to now
 * distribute them in their relevant directory (e.g., engine/Unit_engine.ml)
 *)

(*****************************************************************************)
(* All tests *)
(*****************************************************************************)

(*
   Some test suites are created from files present in file system.
   To avoid errors during module initialization when running 'dune utop'
   from an arbitrary location, these test suites must be created
   explicitly by calling a function. These functions are roughly those
   that call 'Common2.glob'.
*)
let tests () =
  List.flatten
    [
      Unit_list_files.tests;
      Glob.Unit_glob.tests;
      Unit_semgrepignore.tests;
      Unit_parsing.tests ();
      Unit_entropy.tests;
      Unit_ReDoS.tests;
      Unit_guess_lang.tests;
      Unit_memory_limit.tests;
      Unit_SPcre.tests;
      Unit_tok.tests;
      Unit_regexp_engine.tests;
      Unit_Rpath.tests;
      Unit_immutable_buffer.tests;
      Unit_ugly_print_AST.tests;
      Unit_autofix_printer.tests;
      Unit_synthesizer.tests;
      Unit_synthesizer_targets.tests;
      Unit_dataflow.tests Parse_target.parse_program;
      Unit_typing_generic.tests Parse_target.parse_program (fun lang file ->
          Parse_pattern.parse_pattern lang file);
      Unit_naming_generic.tests Parse_target.parse_program;
      (* just expression vs expression testing for one language (Python) *)
      Unit_matcher.tests ~any_gen_of_string;
      (* TODO Unit_matcher.spatch_unittest ~xxx *)
      (* TODO Unit_matcher_php.unittest; (* sgrep, spatch, refactoring, unparsing *) *)
      Unit_engine.tests ();
      Unit_jsonnet_subst.tests ();
      Unit_metachecking.tests ();
      (* OSemgrep tests *)
      Unit_LS.tests;
      Unit_Login.tests;
      Unit_Fetching.tests;
      (* Networking tests disabled as they will get rate limited sometimes *)
      (* And the SSL issues they've been testing have been stable *)
      (*Unit_Networking.tests;*)
      Test_LS_e2e.tests;
      (* End OSemgrep tests *)
      Aliengrep.Unit_tests.tests;
      (* Inline tests *)
      Testutil.get_registered_tests ();
    ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(*
   This allows running the test program with '--help' from any folder
   without getting an error due to not being able to load test data.

   See https://github.com/mirage/alcotest/issues/358 for a request
   to allow what we want without this workaround.
*)
let tests_with_delayed_error () =
  try tests () with
  | e ->
      let exn = Exception.catch e in
      [
        ( "ERROR DURING TEST SUITE INITIALIZATION",
          fun () -> Exception.reraise exn );
      ]

let main () =
  (* find the root of the semgrep repo as many of our tests rely on
     'let test_path = "tests/"' to find their test files *)
  let repo_root =
    match Git_wrapper.get_project_root () with
    | Some path -> path
    | None ->
        failwith
          "You must run the test program from within the semgrep repo and not \
           one of its submodules."
  in
  Testutil_files.with_chdir repo_root (fun () ->
      print_endline ("Running tests from directory: " ^ Sys.getcwd ());
      Http_helpers.client_ref := Some (module Cohttp_lwt_unix.Client);
      Parsing_init.init ();
      Data_init.init ();
      Core_CLI.register_exception_printers ();
      Logs_helpers.setup_logging ~force_color:false ~level:(Some Logs.Debug) ();
      let alcotest_tests = Testutil.to_alcotest (tests_with_delayed_error ()) in
      Alcotest.run "semgrep-core" alcotest_tests)

let () = main ()
