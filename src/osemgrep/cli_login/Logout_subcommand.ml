(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-logout command, execute it and exit.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps = < Cap.stdout >

let logout_message = "Logged out (log back in with `semgrep login`)"

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : caps) (conf : Logout_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "conf = %s" (Logout_CLI.show_conf conf));
  let settings = Semgrep_settings.load ~include_env:false () in
  match settings.Semgrep_settings.api_token with
  | None ->
      Logs.warn (fun m ->
          m "You are not logged in! This command had no effect.");
      CapConsole.print caps#stdout logout_message;
      Exit_code.ok ~__LOC__
  | Some _ ->
      let settings = Semgrep_settings.{ settings with api_token = None } in
      if Semgrep_settings.save settings then (
        (* old: {|%s Logged out! You can log back in with @{<cyan>`semgrep login`@}|} *)
        CapConsole.print caps#stdout logout_message;
        Exit_code.ok ~__LOC__)
      else Exit_code.fatal ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : caps) (argv : string array) : Exit_code.t =
  let conf = Logout_CLI.parse_argv Logout_CLI.logout_cmdline_info argv in
  run_conf caps conf
