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

let t = Testo.create

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
      Unit_graph_code.tests;
      Unit_list_files.tests;
      Glob.Unit_glob.tests;
      Unit_find_targets.tests;
      Unit_semgrepignore.tests;
      Unit_gitignore.tests;
      Unit_include_filter.tests;
      Unit_parsing.tests ();
      Unit_entropy.tests;
      Unit_ReDoS.tests;
      Unit_guess_lang.tests;
      Unit_memory_limit.tests;
      Unit_tok.tests;
      Unit_Ppath.tests;
      Unit_Rpath.tests;
      Unit_git_wrapper.tests;
      Unit_ugly_print_AST.tests;
      Unit_autofix.tests;
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
      Unit_metachecking.tests (caps :> < Cap.tmp >);
      (* OSemgrep tests *)
      Unit_LS.tests (caps :> < Cap.random ; Cap.network ; Cap.tmp >);
      Unit_Login.tests caps;
      Unit_Fetching.tests (caps :> < Cap.network ; Cap.tmp >);
      Test_is_blocking_helpers.tests;
      Test_login_subcommand.tests (caps :> < Cap.stdout ; Cap.network >);
      Test_publish_subcommand.tests
        (caps :> < Cap.stdout ; Cap.network ; Cap.tmp >);
      Osemgrep_tests.tests (caps :> CLI.caps);
      (* Networking tests disabled as they will get rate limited sometimes *)
      (* And the SSL issues they've been testing have been stable *)
      (*Unit_Networking.tests;*)
      Test_LS_e2e.tests (caps :> < Cap.random ; Cap.network ; Cap.tmp >);
      (* End OSemgrep tests *)
      Spacegrep_tests.Test.tests ();
      Aliengrep.Unit_tests.tests;
      (* Inline tests *)
      Testo.get_registered_tests ();
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
  try
    Printf.printf "Gathering tests from %s...\n%!" (Sys.getcwd ());
    let tests = tests caps in
    Printf.printf "Done gathering tests.\n%!";
    tests
  with
  | e ->
      let exn = Exception.catch e in
      [
        t "ERROR DURING TEST SUITE INITIALIZATION" (fun () ->
            Exception.reraise exn);
      ]

let cleanup_before_each_test reset tests =
  tests
  |> List_.map (fun (test : Testo.test) ->
         Testo.update
           ~func:(fun () ->
             reset ();
             test.func ())
           test)

let main (caps : Cap.all_caps) : unit =
  (* find the root of the semgrep repo as many of our tests rely on
     'let test_path = "tests/"' to find their test files *)
  let repo_root =
    match Git_wrapper.get_project_root_for_files_in_dir Fpath_.current_dir with
    | Some path -> path
    | None ->
        failwith
          "You must run the test program from within the semgrep repo and not \
           one of its submodules."
  in
  Testutil_files.with_chdir repo_root (fun () ->
      Http_helpers.client_ref := Some (module Cohttp_lwt_unix.Client);
      Parsing_init.init ();
      Data_init.init ();
      Core_CLI.register_exception_printers ();
      (* Show log messages produced when building the list of tests *)
      Std_msg.setup ~highlight_setting:On ();
      Logs_.setup ~level:(Some Logs.Info) ();
      let reset () =
        (* Some tests change this configuration so we have to reset
           it before each test. In particular, tests that check the semgrep
           output can or should change these settings. *)
        Std_msg.setup ~highlight_setting:On ();
        Logs_.setup ~highlight_setting:On ~level:(Some Logs.Debug) ()
      in
      Testo.interpret_argv ~project_name:"semgrep-core" (fun () ->
          tests_with_delayed_error caps |> cleanup_before_each_test reset))

let () = Cap.main (fun all_caps -> main all_caps)
