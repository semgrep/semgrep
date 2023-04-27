open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-logout command, execute it and exit.

   Translated from login.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* no CLI parameters for now *)
type conf = unit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* this could be moved in a Login_CLI.ml file at some point if needed *)
let cmdline_term : conf Term.t =
  let combine = () in
  Term.(const combine)

let doc = "Remove locally stored credentials to semgrep.dev"

let man : Manpage.block list =
  [
    `S Manpage.s_description;
    `P "Remove locally stored credentials to semgrep.dev";
  ]
  @ CLI_common.help_page_bottom

let cmdline_info : Cmd.info = Cmd.info "semgrep logout" ~doc ~man

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (_conf : conf) : Exit_code.t =
  Logs_helpers.setup_logging ~force_color:false ~level:(Some Logs.Debug);
  let settings = Semgrep_settings.get () in
  let settings = Semgrep_settings.{ settings with api_token = None } in
  Semgrep_settings.save settings;
  Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = parse_argv argv in
  run conf
