(*
   Command-line interface generated for a test program.
*)

open Printf
open Cmdliner

(*
   Configuration object type that is used for all subcommands although
   not all of them use all the fields.
*)
type conf = {
  (* All subcommands *)
  filter_by_substring : string option;
  (* Status *)
  status_output_style : Run.status_output_style;
  (* Run *)
  lazy_ : bool;
}

let default_conf =
  { filter_by_substring = None; status_output_style = Full; lazy_ = false }

(*
   Subcommands:
   - run
   - status
   - approve
*)
type cmd_conf = Run_tests of conf | Status of conf | Approve of conf

type subcommand_result =
  | Run_result of unit Types.test_with_status list
  | Status_result of unit Types.test_with_status list
  | Approve_result

(****************************************************************************)
(* Dispatch subcommands to do real work *)
(****************************************************************************)

let run_with_conf
    ( (get_tests : unit -> unit Types.test list),
      (handle_subcommand_result : int -> subcommand_result -> 'a) )
    (cmd_conf : cmd_conf) =
  (*
     The creation of tests can take a while so it's delayed until we
     really need the tests. This makes '--help' fast.
  *)
  let tests = get_tests () in
  match cmd_conf with
  | Run_tests conf ->
      let exit_code, tests_with_status =
        Run.run_tests ?filter_by_substring:conf.filter_by_substring
          ~lazy_:conf.lazy_ tests
      in
      handle_subcommand_result exit_code (Run_result tests_with_status)
  | Status conf ->
      let exit_code, tests_with_status =
        Run.list_status ?filter_by_substring:conf.filter_by_substring
          ~output_style:conf.status_output_style tests
      in
      handle_subcommand_result exit_code (Status_result tests_with_status)
  | Approve conf ->
      let exit_code =
        Run.approve_output ?filter_by_substring:conf.filter_by_substring tests
      in
      handle_subcommand_result exit_code Approve_result

(****************************************************************************)
(* Command-line options *)
(****************************************************************************)
(*
   Some of the command-line options are shared among subcommands.
*)

let filter_by_substring_term : string option Term.t =
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

let lazy_term : bool Term.t =
  let info =
    Arg.info [ "lazy" ]
      ~doc:"Run only the tests that were not previously successful."
  in
  Arg.value (Arg.flag info)

let run_doc = "run the tests"

let subcmd_run_term test_spec : unit Term.t =
  let combine filter_by_substring lazy_ =
    Run_tests { default_conf with filter_by_substring; lazy_ }
    |> run_with_conf test_spec
  in
  Term.(const combine $ filter_by_substring_term $ lazy_term)

let subcmd_run test_spec =
  let info = Cmd.info "run" ~doc:run_doc in
  Cmd.v info (subcmd_run_term test_spec)

(****************************************************************************)
(* Subcommand: status (replaces alcotest's 'list') *)
(****************************************************************************)

let short_term : bool Term.t =
  let info =
    Arg.info [ "short" ]
      ~doc:"Report only the status of tests that need attention."
  in
  Arg.value (Arg.flag info)

let status_doc = "show test status"

let subcmd_status_term tests : unit Term.t =
  let combine filter_by_substring short =
    let status_output_style : Run.status_output_style =
      if short then Short else Full
    in
    Status { default_conf with filter_by_substring; status_output_style }
    |> run_with_conf tests
  in
  Term.(const combine $ filter_by_substring_term $ short_term)

let subcmd_status tests =
  let info = Cmd.info "status" ~doc:status_doc in
  Cmd.v info (subcmd_status_term tests)

(****************************************************************************)
(* Subcommand: approve *)
(****************************************************************************)

let approve_doc = "approve new test output"

let subcmd_approve_term tests : unit Term.t =
  let combine filter_by_substring =
    Approve { default_conf with filter_by_substring } |> run_with_conf tests
  in
  Term.(const combine $ filter_by_substring_term)

let subcmd_approve tests =
  let info = Cmd.info "approve" ~doc:approve_doc in
  Cmd.v info (subcmd_approve_term tests)

(****************************************************************************)
(* Main command *)
(****************************************************************************)

let root_doc ~project_name = sprintf "run tests for %s" project_name

let root_man ~project_name : Manpage.block list =
  [
    `S Manpage.s_description;
    `P
      (sprintf
         {|This is the program built for running and managing the tests for this project,
%s. It revolves around 3 main subcommands: 'run', 'status', and 'approve'.
Use the 'status' subcommand to check the status of each test without having
to re-run them. 'approve' must be used on tests whose output is captured
so as to make their latest output the new reference.
|}
         project_name);
    `P
      (sprintf
         {|This test program was configured to store the temporary results in
'%s' and the expected test output in the persistent folder '%s'.
The latter should be kept under version control (git or similar).
|}
         (Store.get_status_workspace ())
         (Store.get_expectation_workspace ()));
  ]

let root_info ~project_name =
  let name = Filename.basename Sys.argv.(0) in
  Cmd.info name ~doc:(root_doc ~project_name) ~man:(root_man ~project_name)

let root_term test_spec =
  (*
  Term.ret (Term.const (`Help (`Pager, None)))
*)
  subcmd_run_term test_spec

let subcommands test_spec =
  [ subcmd_run test_spec; subcmd_status test_spec; subcmd_approve test_spec ]

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
    ?(handle_subcommand_result = fun exit_code _ -> exit exit_code)
    ?status_workspace_root ~project_name
    (get_tests : unit -> unit Types.test list) =
  (* TODO: is there any reason why we shouldn't always record a stack
     backtrace when running tests? *)
  let test_spec = (get_tests, handle_subcommand_result) in
  with_record_backtrace (fun () ->
      Store.init_settings ?expectation_workspace_root ?status_workspace_root
        ~project_name ();
      Cmd.group ~default:(root_term test_spec) (root_info ~project_name)
        (subcommands test_spec)
      |> Cmd.eval ~argv |> (* does not reach this point by default *) exit)
