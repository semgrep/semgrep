(*
   Command-line interface generated for a test program.
*)

open Cmdliner

(*
   Configuration object type that is used for all subcommands although
   not all of them use all the fields.
*)
type conf = { filter_by_substring : string option }

(*
   Subcommands:
   - run
   - status
   - approve
*)
type cmd_conf = Run_tests of conf | Status of conf | Approve of conf

(****************************************************************************)
(* Dispatch subcommands to do real work *)
(****************************************************************************)

let run_with_conf tests (cmd_conf : cmd_conf) =
  match cmd_conf with
  | Run_tests conf ->
      Store.init_workspace ();
      (* TODO: display the alcotest_ext status in the first pass, which
         would require running the test suite ourselves rather than
         using the original Alcotest.run. *)
      Run.run_tests ?filter_by_substring:conf.filter_by_substring tests;
      print_newline ();
      Run.list_status ?filter_by_substring:conf.filter_by_substring
        ~only_important:true tests
      |> exit
  | Status conf ->
      Run.list_status ?filter_by_substring:conf.filter_by_substring tests
      |> exit
  | Approve conf ->
      Store.init_workspace ();
      Run.approve_output ?filter_by_substring:conf.filter_by_substring tests

(****************************************************************************)
(* Command-line options *)
(****************************************************************************)
(*
   Some of the command-line options are shared among subcommands.
*)

let filter_by_substring_term =
  let info =
    Arg.info
      [ "s"; "filter-substring" ]
      ~docv:"SUBSTRING"
      ~doc:"Select tests whose description contains SUBSTRING."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

(****************************************************************************)
(* Subcommand: run (replaces alcotest's 'test') *)
(****************************************************************************)

let run_doc = "Run the tests."

let subcmd_run_term tests =
  let combine filter_by_substring =
    Run_tests { filter_by_substring } |> run_with_conf tests
  in
  Term.(const combine $ filter_by_substring_term)

let subcmd_run tests =
  let info = Cmd.info "run" ~doc:run_doc in
  Cmd.v info (subcmd_run_term tests)

(****************************************************************************)
(* Subcommand: status (replaces alcotest's 'list') *)
(****************************************************************************)

let status_doc = "List the current status of the tests."

let subcmd_status_term tests =
  let combine filter_by_substring =
    Status { filter_by_substring } |> run_with_conf tests
  in
  Term.(const combine $ filter_by_substring_term)

let subcmd_status tests =
  let info = Cmd.info "status" ~doc:status_doc in
  Cmd.v info (subcmd_status_term tests)

(****************************************************************************)
(* Subcommand: approve *)
(****************************************************************************)

let approve_doc = "Approve the new output of tests run previously."

let subcmd_approve_term tests =
  let combine filter_by_substring =
    Approve { filter_by_substring } |> run_with_conf tests
  in
  Term.(const combine $ filter_by_substring_term)

let subcmd_approve tests =
  let info = Cmd.info "approve" ~doc:approve_doc in
  Cmd.v info (subcmd_approve_term tests)

(****************************************************************************)
(* Main command *)
(****************************************************************************)

let root_doc = "run this project's tests"

let root_info ~project_name =
  let name = "test " ^ project_name in
  Cmd.info name ~doc:root_doc

let root_term tests =
  (*
  Term.ret (Term.const (`Help (`Pager, None)))
*)
  subcmd_run_term tests

let subcommands tests =
  [ subcmd_run tests; subcmd_status tests; subcmd_approve tests ]

let with_record_backtrace func =
  let original_state = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  (* nosemgrep: no-fun-protect *)
  Fun.protect ~finally:(fun () -> Printexc.record_backtrace original_state) func

(*
     $ cmdliner-demo-subcmd           -> parsed as root subcommand
     $ cmdliner-demo-subcmd --help    -> also parsed as root subcommand
     $ cmdliner-demo-subcmd subcmd1   -> parsed as 'subcmd1' subcommand

   If there is a request to display the help page, it displayed at this point,
   returning '`Help'.

   Otherwise, 'conf' is returned to the application.
*)
let interpret_argv ?(argv = Sys.argv) ?expectation_workspace_root
    ?status_workspace_root ~project_name tests =
  (* TODO: is there any reason why we shouldn't always record a stack
     backtrace when running tests? *)
  with_record_backtrace (fun () ->
      Store.init_settings ?expectation_workspace_root ?status_workspace_root
        ~project_name ();
      Cmd.group ~default:(root_term tests) (root_info ~project_name)
        (subcommands tests)
      |> Cmd.eval ~argv)
