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
 * history: this file used to contain lots of tests, but it's better to now
 * distribute them in their relevant directory (e.g., engine/Unit_engine.ml)
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type env_entry = { variable : string; value : string } [@@deriving ord]

module EnvEntrySet = Set.Make (struct
  type t = env_entry

  let compare = compare_env_entry
end)

let parse_env_entry ~ignore_empty s =
  match String.index_opt s '=' with
  | Some i ->
      let k = String_.safe_sub s 0 i in
      let v = String_.safe_sub s (i + 1) (String.length s - i - 1) in
      if ignore_empty && v = "" then None else Some { variable = k; value = v }
  | None -> None

(* Get the set of environment variables and their values, optionally
   excluding empty values. *)
let get_environment ~ignore_empty () =
  Unix.environment () |> Array.to_list
  |> List.filter_map (parse_env_entry ~ignore_empty)
  |> EnvEntrySet.of_list

let string_of_set (set : EnvEntrySet.t) =
  set |> EnvEntrySet.elements
  |> List.map (fun { variable; value } -> spf "%s=%s" variable value)
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
        let removed = EnvEntrySet.diff orig_env final_env in
        let added = EnvEntrySet.diff final_env orig_env in
        if not (EnvEntrySet.is_empty removed && EnvEntrySet.is_empty added) then
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
let tests =
  (* Tests that still fork via CapProcess.apply_in_child_process_promise,
     Bos.OS.Cmd.run, or still run a scan with a default scan config (which
     will use parmap) must come before any that spawn Domains. *)
  let forking_tests =
    List_.flatten [ Test_core_CLI.tests (); Legacy_unit_ls.tests () ]
  in

  (* Tests that use [Testutio_git.with_git_repo] interact via git through
   * a subprocess. *)
  let gitutil_tests =
    List_.flatten
      [
        Test_ci_subcommand.tests ();
        Test_target_selection.tests ();
        Unit_find_targets.tests;
      ]
  in

  gitutil_tests @ forking_tests
  @ List_.flatten
      [
        Commons_tests.tests;
        Collections_tests.tests;
        Unit_list_files.tests ();
        Unit_glob.tests;
        Unit_semgrepignore.tests;
        Unit_gitignore.tests;
        Unit_include_filter.tests;
        Unit_disk_cache.tests;
        Unit_parsing.tests ();
        Unit_parsing_python.tests;
        Unit_parsing_scala.tests;
        Unit_entropy.tests;
        Parser_regexp_tests.Unit_parsing.tests;
        Unit_ReDoS.tests;
        Unit_guess_lang.tests;
        Unit_cgroup_limits.tests;
        Unit_memory_limit.tests ();
        Unit_tok.tests;
        Unit_parsed_float.tests;
        Unit_fast_json.tests;
        Unit_Ppath.tests;
        Unit_Rpath.tests;
        Unit_git_wrapper.tests;
        Unit_ugly_print_AST.tests;
        Unit_autofix.tests;
        Unit_autofix_printer.tests;
        Unit_dataflow.tests Parse_target.parse_program;
        Unit_typing_generic.tests Parse_target.parse_program (fun lang file ->
            Parse_pattern.parse_pattern lang file);
        Unit_naming_generic.tests Parse_target.parse_program;
        (* just expression vs expression testing for one language (Python) *)
        Unit_matcher.tests ~any_gen_of_string;
        (* TODO Unit_matcher.spatch_unittest ~xxx *)
        (* TODO Unit_matcher_php.unittest; sgrep/spatch/refactoring/unparsing *)
        Unit_engine.tests;
        Unit_jsonnet.tests;
        Unit_metachecking.tests;
        Unit_http_helpers.tests;
        (* osemgrep unit tests *)
        Unit_Login.tests;
        Unit_app.tests;
        Unit_Fetching.tests;
        Unit_reporting.tests;
        Unit_sarif_output.tests;
        Unit_ci.tests;
        Test_is_blocking_helpers.tests;
        (* osemgrep e2e subcommand tests *)
        Test_login_subcommand.tests ();
        Unit_test_subcommand.tests ();
        Test_show_subcommand.tests ();
        Test_osemgrep.tests ();
        (* Networking tests disabled as they will get rate limited sometimes *)
        (* And the SSL issues they've been testing have been stable *)
        (*Unit_Networking.tests;*)
        Legacy_test_ls_e2e.tests ();
        (* End osemgrep tests *)
        Spacegrep_tests.Test.tests ();
        Unit_tests.tests;
        Unit_core_json_output.tests;
        (* Inline tests *)
        Testo.get_registered_tests ();
        Parallelism_tests.tests;
        Test_compiler_version.tests;
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
  try
    Printf.printf "Gathering tests from %s...\n%!" (Sys.getcwd ());
    let tests = tests |> List.map (with_env_check ~ignore_empty:true) in
    Printf.printf "Done gathering tests.\n%!";
    tests
  with
  | e ->
      let exn = Exception.catch e in
      [
        t "ERROR DURING TEST SUITE INITIALIZATION" (fun () ->
            Exception.reraise exn);
      ]

let main () =
  (* find the root of the semgrep repo as many of our tests rely on
     'let test_path = "tests/"' to find their test files *)
  let project_root = Legacy_test_ls_e2e.project_root () in
  (* Don't read ~/.gitconfig since it varies from one developer to another,
     resulting in variable output *)
  Testo.with_environment_variables [ ("GIT_CONFIG_NOGLOBAL", "true") ]
  @@ fun () ->
  Testutil_files.with_chdir project_root (fun () ->
      (* coupling: partial copy of the content of CLI.main() *)
      Core_CLI.register_exception_printers ();
      Http_helpers.set_client_ref (module Cohttp_lwt_unix.Client);
      (* Show log messages produced when building the list of tests.
         Log_semgrep.setup prints a lengthy welcome message that we
         don't want in the output of each unit test. *)
      Log_semgrep.with_setup ~color:On ~level:(Some Info) @@ fun () ->
      (* let's go *)
      Testo.interpret_argv ~project_name:"semgrep-core" (fun _env ->
          tests_with_delayed_error ()))

let () = main ()
