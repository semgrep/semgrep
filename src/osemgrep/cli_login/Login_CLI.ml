module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   'semgrep login' command-line arguments processing.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type conf = {
  common : CLI_common.conf;
  (* Initialize the auth exchange with a temporary shared secret *)
  one_time_seed : string;
}
[@@deriving show]

(*****************************************************************************)
(* Login subcommand *)
(*****************************************************************************)

let login_doc = "Obtain and save credentials for semgrep.dev"

let login_man : Cmdliner.Manpage.block list =
  [
    `S Cmdliner.Manpage.s_description;
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
(* Flags *)
(*****************************************************************************)

let o_temporary_secret : string Term.t =
  let doc =
    "Initialize login with a temporary secret from the onboarding flow"
  in
  Arg.(value & opt string "" & info [ "init"; "setup" ] ~docv:"secret" ~doc)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let term =
  let combine common init = { common; init } in
  Term.(const combine $ CLI_common.o_common $ o_temporary_secret)

let parse_argv (cmd_info : Cmd.info) (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmd_info term in
  CLI_common.eval_value ~argv cmd
