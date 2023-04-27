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
(* Helpers *)
(*****************************************************************************)

(*************************************************************************)
(* Command-line flags *)
(*************************************************************************)

(* ------------------------------------------------------------------ *)
(* "Verbosity options" (mutually exclusive) *)
(* ------------------------------------------------------------------ *)
(* alt: we could use Logs_cli.level(), but by defining our own flags
 * we can give better ~doc:. We lose the --verbosity=Level though.
 *)
let o_quiet : bool Term.t =
  let info = Arg.info [ "q"; "quiet" ] ~doc:{|Only output findings.|} in
  Arg.value (Arg.flag info)

let o_verbose : bool Term.t =
  let info =
    Arg.info [ "v"; "verbose" ]
      ~doc:
        {|Show more details about what rules are running, which files
failed to parse, etc.
|}
  in
  Arg.value (Arg.flag info)

let o_debug : bool Term.t =
  let info =
    Arg.info [ "debug" ]
      ~doc:{|All of --verbose, but with additional debugging information.|}
  in
  Arg.value (Arg.flag info)

(*****************************************************************************)
(* Turn argv into a conf *)
(*****************************************************************************)

let cmdline_term : conf Term.t =
  let combine debug quiet verbose =
    let logging_level =
      match (verbose, debug, quiet) with
      | false, false, false -> Some Logs.Warning
      | true, false, false -> Some Logs.Info
      | false, true, false -> Some Logs.Debug
      | false, false, true -> None
      | _else_ ->
          (* TOPORT: list the possibilities *)
          Error.abort "mutually exclusive options --quiet/--verbose/--debug"
    in
    { logging_level }
  in
  Term.(const combine $ o_debug $ o_quiet $ o_verbose)

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

let parse_argv (cmd_info : Cmd.info) (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmd_info cmdline_term in
  CLI_common.eval_value ~argv cmd
