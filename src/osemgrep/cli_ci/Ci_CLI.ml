module Cmd = Cmdliner.Cmd

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
[@@deriving show]

(*************************************************************************)
(* Command-line parsing: turn argv into conf *)
(*************************************************************************)

let doc = "the recommended way to run semgrep in CI"

let man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P
      "In pull_request/merge_request (PR/MR) contexts, `semgrep ci` will only \
       report findings that were introduced by the PR/MR.";
    `P
      "When logged in, `semgrep ci` runs rules configured on Semgrep App and \
       sends findings to your findings dashboard.";
    `P "Only displays findings that were marked as blocking.";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep ci" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  (* mostly a copy of Scan_CLI.parse_argv with different doc and man *)
  let cmd : conf Cmd.t =
    Cmd.v cmdline_info (Scan_CLI.cmdline_term ~allow_empty_config:true)
  in
  CLI_common.eval_value ~argv cmd
