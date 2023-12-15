(*
   Test suite that runs the dummy test program and checks that its output
   is what we expect.
*)

open Printf
module T = Alcotest_ext

let t = T.create

(*
   Invoke an arbitrary shell command.
*)
let shell_command command =
  printf "RUN %s\n%!" command;
  (* nosemgrep: forbid-exec *)
  match Sys.command command with
  | 0 -> ()
  | n -> failwith (sprintf "Command '%s' failed with exit code %i" command n)

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

let section text =
  printf
    {|#####################################################################
# %s
#####################################################################
%!|}
    text

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

(* Function composition *)
let ( @@@ ) f g x = f (g x)

let tests =
  [
    t ~output_kind:Merged_stdout_stderr
      ~mask_output:
        (T.mask_line ~mask:"<MASKED RUN ID>" ~after:"This run has ID `"
           ~before:"'" ()
        @@@ T.mask_pcre_pattern ~mask:"<MASKED DURATION>" {|in [0-9]+\.[0-9]+s|}
        @@@ T.mask_line ~after:"Called from " ()
        @@@ T.mask_line ~after:"Re-raised at " ())
      "standard flow" test_standard_flow;
  ]

let () =
  Alcotest_ext.interpret_argv ~project_name:"alcotest_ext_meta_tests" (fun () ->
      tests)
