open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-login command, execute it and exit.

   Translated from login.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* no CLI parameters for now *)
type login_cli_conf = unit
type conf = login_cli_conf

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* this could be moved in a Login_CLI.ml file at some point *)
let cmdline_term : conf Term.t =
  let combine = () in
  Term.(const combine)

let doc = "Obtain and save credentials for semgrep.dev"

let man : Manpage.block list =
  [
    `S Manpage.s_description;
    `P
      "Obtain and save credentials for semgrep.dev\n\n\
      \    Looks for an semgrep.dev API token in the environment variable \
       SEMGREP_API_TOKEN_SETTINGS_KEY.\n\
      \    If not defined and running in a TTY, prompts interactively.\n\
      \    Once token is found, saves it to global settings file";
  ]
  @ CLI_common.help_page_bottom

let parse_argv (argv : string array) : (conf, Exit_code.t) result =
  let info : Cmd.info = Cmd.info "semgrep login" ~doc ~man in
  let cmd : conf Cmd.t = Cmd.v info cmdline_term in
  match Cmd.eval_value ~argv cmd with
  | Error _n -> Error Exit_code.fatal
  | Ok ok -> (
      match ok with
      | `Ok config -> Ok config
      | `Version
      | `Help ->
          Error Exit_code.ok)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (_conf : login_cli_conf) : Exit_code.t =
  (* TODO:
     Setup_logging.setup config;
     logger#info "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " ");
     logger#info "Version: %s" config.version;
  *)
  Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let res = parse_argv argv in
  match res with
  | Ok conf -> CLI_common.safe_run run conf
  | Error exit_code -> exit_code
