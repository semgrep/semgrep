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
(* Command-line Flags *)
(*****************************************************************************)

let o_temporary_secret : string Term.t =
  let doc =
    "Initialize login with a temporary secret from the Semgrep App onboarding \
     flow"
  in
  Arg.(value & opt string "" & info [ "init"; "setup" ] ~docv:"secret" ~doc)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let cmdline_term : conf Term.t =
  let combine common one_time_seed = { common; one_time_seed } in
  Term.(const combine $ CLI_common.o_common $ o_temporary_secret)

let doc = "Obtain and save credentials for semgrep.dev"

let man : Cmdliner.Manpage.block list =
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

let cmdline_info : Cmd.info = Cmd.info "semgrep login" ~doc ~man

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd
