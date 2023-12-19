(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Unit tests entry point.
 *
 * From semgrep-core you can do
 *
 *   $./test foo
 *
 * to run all the tests containing foo in their description.
 *
 * This file used to contain lots of tests, but it's better to now
 * distribute them in their relevant directory (e.g., engine/Unit_engine.ml)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let any_gen_of_string str =
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any

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
let tests (caps : Cap.all_caps) =
  List.flatten
    [
      Commons_tests.tests;
      Unit_list_files.tests;
      Glob.Unit_glob.tests;
      Unit_semgrepignore.tests;
      Unit_gitignore.tests;
      Unit_parsing.tests ();
      Unit_entropy.tests;
      Unit_ReDoS.tests;
      Unit_guess_lang.tests;
      Unit_memory_limit.tests;
      Unit_tok.tests;
      Unit_Rpath.tests;
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
      Unit_jsonnet.tests ();
      Unit_metachecking.tests ();
      (* OSemgrep tests *)
      Unit_LS.tests;
      Unit_Login.tests caps;
      Unit_Fetching.tests (caps :> < Cap.network >);
      Test_login_subcommand.tests (caps :> < Cap.stdout ; Cap.network >);
      Test_publish_subcommand.tests (caps :> < Cap.stdout ; Cap.network >);
      Test_osemgrep.tests caps;
      (* Networking tests disabled as they will get rate limited sometimes *)
      (* And the SSL issues they've been testing have been stable *)
      (*Unit_Networking.tests;*)
      Test_LS_e2e.tests;
      (* End OSemgrep tests *)
      Spacegrep_tests.Test.tests ();
      Aliengrep.Unit_tests.tests;
      (* Inline tests *)
      Alcotest_ext.get_registered_tests ();
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
let tests_with_delayed_error caps =
  try tests caps with
  | e ->
      let exn = Exception.catch e in
      Alcotest_ext.simple_tests
        [
          ( "ERROR DURING TEST SUITE INITIALIZATION",
            fun () -> Exception.reraise exn );
        ]

let main (caps : Cap.all_caps) : unit =
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
      Logs_.setup_logging ~force_color:false ~level:(Some Logs.Debug) ();
      Alcotest_ext.interpret_argv ~project_name:"semgrep-core" (fun () ->
          tests_with_delayed_error caps))

let () = Cap.main (fun all_caps -> main all_caps)
