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
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | None ->
      Logs.app (fun m ->
          m "%s You are not logged in! This command had no effect."
            (Logs_helpers.warn_tag ()));
      Exit_code.ok
  | Some _ ->
      let settings =
        Semgrep_settings.
          { settings with api_token = None; anonymous_user_id = Uuidm.v `V4 }
      in
      if Semgrep_settings.save settings then (
        Logs.app (fun m ->
            m "%s Logged out! Log back in with `semgrep login`"
              (Logs_helpers.success_tag ()));
        Exit_code.ok)
      else Exit_code.fatal

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Login_CLI.parse_argv Login_CLI.logout_cmdline_info argv in
  run conf
