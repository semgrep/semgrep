(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-logout command, execute it and exit.

   Translated from login.py
*)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Login_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.logging_level;
  let settings = Semgrep_settings.load () in
  let settings = Semgrep_settings.{ settings with api_token = None } in
  if Semgrep_settings.save settings then (
    Logs.app (fun m -> m "Logged out (log back in with `semgrep login`)");
    Exit_code.ok)
  else Exit_code.fatal

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Login_CLI.parse_argv Login_CLI.logout_cmdline_info argv in
  run conf
