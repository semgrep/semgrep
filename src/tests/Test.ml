(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common

let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* semgrep-core and osemgrep tests suite entry point.
 *
 * From the root of the semgrep repo you can do
 *
 *   $ ./test -s foo
 *
 * to run all the OCaml tests containing foo in their test name.
 *
 * hisyory: this file used to contain lots of tests, but it's better to now
 * distribute them in their relevant directory (e.g., engine/Unit_engine.ml)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let parse_env_entry ~ignore_empty s =
  match String.index_opt s '=' with
  | Some i ->
      let k = String_.safe_sub s 0 i in
      let v = String_.safe_sub s (i + 1) (String.length s - i - 1) in
      if ignore_empty && v = "" then None else Some (k, v)
  | None -> None

(* Get the set of environment variables and their values, optionally
   excluding empty values. *)
let get_environment ~ignore_empty () =
  Unix.environment () |> Array.to_list
  |> List_.filter_map (parse_env_entry ~ignore_empty)
  |> Set_.of_list

let string_of_set (set : (string * string) Set_.t) =
  set |> Set_.elements
  |> List_.map (fun (k, v) -> spf "%s=%s" k v)
  |> String.concat ", "

(*
   Wrap the test function so as to check no environment variables were altered
   and not restored during the test.

   TODO: if this proves useful, move it to Testo
*)
let with_env_check ?(ignore_empty = false) (test : Testo.t) =
  let func () =
    let orig_env = get_environment ~ignore_empty () in
    Common.protect test.func ~finally:(fun () ->
        let final_env = get_environment ~ignore_empty () in
        let removed = Set_.diff orig_env final_env in
        let added = Set_.diff final_env orig_env in
        if not (Set_.is_empty removed && Set_.is_empty added) then
          let msg =
            spf
              {|One or more environment variables changed during the test.%s
  * removed bindings: %s
  * added bindings: %s|}
              (if ignore_empty then
                 "\nVariables bound to empty values are treated as unbound."
               else "")
              (string_of_set removed) (string_of_set added)
          in
          failwith msg)
  in
  Testo.update ~func test

let any_gen_of_string str =
  let any = Parse_python.any_of_string str in
  Python_to_generic.any any

(* alt: could be in Testutil_files.ml or even Testo library *)
let cleanup_before_each_test (reset : unit -> unit) (tests : Testo.t list) :
    Testo.t list =
  tests
  |> List_.map (fun (test : Testo.t) ->
         Testo.update
           ~func:(fun () ->
             reset ();
             test.func ())
           test)

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
  List_.flatten
    [
      Commons_tests.tests;
      Unit_list_files.tests;
      Glob.Unit_glob.tests;
      Unit_find_targets.tests;
      Unit_semgrepignore.tests;
      Unit_gitignore.tests;
      Unit_include_filter.tests;
      Unit_parsing.tests ();
      Unit_entropy.tests;
      Parser_regexp.Unit_parsing.tests;
      Unit_ReDoS.tests;
      Unit_guess_lang.tests;
      Unit_memory_limit.tests (caps :> < Cap.memory_limit >);
      Unit_tok.tests;
      Unit_Ppath.tests;
      Unit_Rpath.tests;
      Unit_git_wrapper.tests;
      Unit_ugly_print_AST.tests;
      Unit_autofix.tests;
      Unit_autofix_printer.tests;
      Unit_synthesizer.tests;
      Unit_synthesizer_targets.tests;
      Unit_dataflow.tests
        (caps :> < Cap.time_limit >)
        Parse_target.parse_program;
      Unit_typing_generic.tests Parse_target.parse_program (fun lang file ->
          Parse_pattern.parse_pattern lang file);
      Unit_naming_generic.tests Parse_target.parse_program;
      (* just expression vs expression testing for one language (Python) *)
      Unit_matcher.tests ~any_gen_of_string;
      (* TODO Unit_matcher.spatch_unittest ~xxx *)
      (* TODO Unit_matcher_php.unittest; sgrep/spatch/refactoring/unparsing *)
      Unit_engine.tests ();
      Unit_jsonnet.tests (caps :> < Cap.time_limit >);
      Unit_metachecking.tests (caps :> Core_scan.caps);
      (* osemgrep unit tests *)
      Unit_LS.tests (caps :> Session.caps);
      Unit_Login.tests caps;
      Unit_Fetching.tests (caps :> < Cap.network ; Cap.tmp >);
      Unit_reporting.tests (caps :> < >);
      Unit_ci.tests;
      Test_is_blocking_helpers.tests;
      (* osemgrep e2e subcommand tests *)
      Test_login_subcommand.tests (caps :> Login_subcommand.caps);
      Test_scan_subcommand.tests (caps :> Scan_subcommand.caps);
      Test_ci_subcommand.tests (caps :> Ci_subcommand.caps);
      Unit_test_subcommand.tests (caps :> Test_subcommand.caps);
      Test_show_subcommand.tests (caps :> Show_subcommand.caps);
      Test_publish_subcommand.tests
        (* = Publish_subcommand.caps + Cap.exec for 'semgrep login' *)
        (caps :> < Cap.stdout ; Cap.network ; Cap.tmp ; Cap.exec >);
      Test_osemgrep.tests (caps :> CLI.caps);
      Test_target_selection.tests (caps :> CLI.caps);
      (* Networking tests disabled as they will get rate limited sometimes *)
      (* And the SSL issues they've been testing have been stable *)
      (*Unit_Networking.tests;*)
      Test_LS_e2e.tests (caps :> Lsp_subcommand.caps);
      (* End osemgrep tests *)
      Spacegrep_tests.Test.tests ();
      Aliengrep.Unit_tests.tests;
      Unit_core_json_output.tests;
      Test_core_CLI.tests (caps :> Cap.all_caps);
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
    let tests = tests caps |> List_.map (with_env_check ~ignore_empty:true) in
    Printf.printf "Done gathering tests.\n%!";
    tests
  with
  | e ->
      let exn = Exception.catch e in
      [
        t "ERROR DURING TEST SUITE INITIALIZATION" (fun () ->
            Exception.reraise exn);
      ]

let main (caps : Cap.all_caps) : unit =
  (* find the root of the semgrep repo as many of our tests rely on
     'let test_path = "tests/"' to find their test files *)
  let project_root = Test_LS_e2e.project_root () in
  (* Don't read ~/.gitconfig since it varies from one developer to another,
     resulting in variable output *)
  Unix.putenv "GIT_CONFIG_NOGLOBAL" "true";
  Testutil_files.with_chdir project_root (fun () ->
      (* coupling: partial copy of the content of CLI.main() *)
      Core_CLI.register_exception_printers ();
      Parsing_init.init ();
      Data_init.init ();
      Http_helpers.set_client_ref (module Cohttp_lwt_unix.Client);
      let reset () =
        (* Some tests change this configuration so we have to reset
           it before each test. In particular, tests that check the semgrep
           output can or should change these settings. *)
        UConsole.setup ~highlight_setting:On ();
        (* TODO? use Log_semgrep.setup? *)
        Logs_.setup_basic ~level:(Some Logs.Debug) ()
      in
      (* Show log messages produced when building the list of tests *)
      reset ();
      (* let's go *)
      Testo.interpret_argv ~project_name:"semgrep-core" (fun _env ->
          tests_with_delayed_error caps |> cleanup_before_each_test reset))

let () = Cap.main (fun all_caps -> main all_caps)
