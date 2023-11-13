module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep logout' command-line arguments processing.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = { common : CLI_common.conf } [@@deriving show]

(*****************************************************************************)
(* Logout subcommand *)
(*****************************************************************************)

let logout_doc = "Remove locally stored credentials to semgrep.dev"

let logout_man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
    `P "Remove locally stored credentials to semgrep.dev";
  ]
  @ CLI_common.help_page_bottom

let logout_cmdline_info : Cmd.info =
  Cmd.info "semgrep logout" ~doc:logout_doc ~man:logout_man

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let term =
  let combine common = { common } in
  Term.(const combine $ CLI_common.o_common)

let parse_argv (cmd_info : Cmd.info) (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmd_info term in
  CLI_common.eval_value ~argv cmd
