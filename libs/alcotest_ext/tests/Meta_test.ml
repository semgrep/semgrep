(*
   Test suite that runs the dummy test program and checks that its output
   is what we expect.
*)

open Printf
module T = Alcotest_ext

let t = T.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
   Invoke an arbitrary shell command.
*)
let shell_command command =
  printf "RUN %s\n%!" command;
  (* nosemgrep: forbid-exec *)
  match Sys.command command with
  | 0 -> ()
  | n -> failwith (sprintf "Command '%s' failed with exit code %i" command n)

let section text =
  printf
    {|#####################################################################
# %s
#####################################################################
%!|}
    text

(*****************************************************************************)
(* Exercise the regular test suite *)
(*****************************************************************************)

(*
   Invoke the pre-build test program.
*)
let test_subcommand shell_command_string =
  let command = "./test " ^ shell_command_string in
  shell_command command

let clear_status () =
  shell_command "rm -rf _build/alcotest_ext/status/alcotest_ext_dummy_tests"

let clear_snapshots () =
  shell_command "rm -rf tests/snapshots/alcotest_ext_dummy_tests"

let test_standard_flow () =
  section "Clean start";
  clear_status ();
  clear_snapshots ();
  test_subcommand "status";
  test_subcommand "run";
  test_subcommand "status";
  test_subcommand "status --short";
  test_subcommand "approve";
  test_subcommand "status";
  section "Delete statuses but not snapshots";
  clear_status ();
  test_subcommand "status";
  test_subcommand "run";
  section "Delete snapshots but not statuses";
  clear_snapshots ();
  test_subcommand "status";
  test_subcommand "approve"

(*****************************************************************************)
(* Exercise the failing test suite *)
(*****************************************************************************)

let failing_test_subcommand shell_command_string =
  let command = "./failing-test " ^ shell_command_string in
  shell_command command

let test_failing_flow_run () = failing_test_subcommand "run"
let test_failing_flow_status () = failing_test_subcommand "status"

(*****************************************************************************)
(* Meta test suite *)
(*****************************************************************************)

let delete pat = T.mask_pcre_pattern ~mask:"" pat

let mask_alcotest_output =
  [
    T.mask_line ~mask:"<MASKED RUN ID>" ~after:"This run has ID `" ~before:"'"
      ();
    T.mask_pcre_pattern ~mask:"<MASKED DURATION>" {|in [0-9]+\.[0-9]+s|};
    T.mask_line ~after:"Called from " ();
    T.mask_line ~after:"Re-raised at " ();
    T.mask_line ~after:"Logs saved to " ();
    T.mask_line ~after:"Full test results in " ();
    (* These extra markers show up in the Alcotest output in GitHub Actions.
       There may be a better way to disable them but this will have to do for
       now. *)
    delete (Re.Pcre.quote "::group::{test}\n");
    delete (Re.Pcre.quote "::endgroup::\n");
  ]

let tests =
  [
    t ~output_kind:Merged_stdout_stderr ~mask_output:mask_alcotest_output
      "standard flow" test_standard_flow;
    t "failing flow run"
      ~expected_outcome:
        (Should_fail "the invoked test suite is designed to fail")
      test_failing_flow_run;
    t "failing flow status"
      ~expected_outcome:
        (Should_fail "the invoked test suite is designed to fail")
      test_failing_flow_status;
  ]

let () =
  Alcotest_ext.interpret_argv ~project_name:"alcotest_ext_meta_tests" (fun () ->
      tests)
