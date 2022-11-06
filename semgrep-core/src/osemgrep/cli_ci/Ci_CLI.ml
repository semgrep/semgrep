open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep ci' subcommand

   Translated from ci.py
*)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* 'semgrep ci' shares most of its flags with 'semgrep scan' *)
type conf = Scan_CLI.conf

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let doc = "the recommended way to run semgrep in CI"

let man : Manpage.block list =
  [
    `S Manpage.s_description;
    `P
      "In pull_request/merge_request (PR/MR) contexts, `semgrep ci` will only \
       report findings that were introduced by the PR/MR.";
    `P
      "When logged in, `semgrep ci` runs rules configured on Semgrep App and \
       sends findings to your findings dashboard.";
    `P "Only displays findings that were marked as blocking.";
  ]
  @ CLI_common.help_page_bottom

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : (conf, Exit_code.t) result =
  (* mostly a copy of Scan_CLI.parse_argv with different doc and man *)
  let info : Cmd.info = Cmd.info "semgrep ci" ~doc ~man in
  let cmd : conf Cmd.t = Cmd.v info Scan_CLI.cmdline_term in
  match Cmd.eval_value ~argv cmd with
  | Error _n -> Error Exit_code.fatal
  | Ok ok -> (
      match ok with
      | `Ok config -> Ok config
      | `Version
      | `Help ->
          Error Exit_code.ok)
