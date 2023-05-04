(* Provides the 'Arg', 'Cmd', 'Manpage', and 'Term' modules. *)
open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep login' command-line arguments processing.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = { logging_level : Logs.level option } [@@deriving show]

(*****************************************************************************)
(* Login subcommand *)
(*****************************************************************************)

let login_doc = "Obtain and save credentials for semgrep.dev"

let login_man : Manpage.block list =
  [
    `S Manpage.s_description;
    `P
      "Obtain and save credentials for semgrep.dev\n\n\
      \    Looks for an semgrep.dev API token in the environment variable \
       SEMGREP_APP_TOKEN.\n\
      \    If not defined and running in a TTY, prompts interactively.\n\
      \    Once token is found, saves it to global settings file";
  ]
  @ CLI_common.help_page_bottom

let login_cmdline_info : Cmd.info =
  Cmd.info "semgrep login" ~doc:login_doc ~man:login_man

(*****************************************************************************)
(* Logout subcommand *)
(*****************************************************************************)

let logout_doc = "Remove locally stored credentials to semgrep.dev"

let logout_man : Manpage.block list =
  [
    `S Manpage.s_description;
    `P "Remove locally stored credentials to semgrep.dev";
  ]
  @ CLI_common.help_page_bottom

let logout_cmdline_info : Cmd.info =
  Cmd.info "semgrep logout" ~doc:logout_doc ~man:logout_man

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let term =
  let combine logging_level = { logging_level } in
  Term.(const combine $ CLI_common.logging_term)

let parse_argv (cmd_info : Cmd.info) (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmd_info term in
  CLI_common.eval_value ~argv cmd
