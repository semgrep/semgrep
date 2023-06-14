(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-login command, execute it and exit.

   Translated from login.py
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let make_login_url () =
  let session_id = Uuidm.v `V4 in
  ( session_id,
    Uri.(
      add_query_params'
        (with_path Semgrep_envvars.v.semgrep_url "login")
        [
          ("cli-token", Uuidm.to_string session_id);
          ("docker", if Semgrep_envvars.v.in_docker then "True" else "False");
          ("gha", if Semgrep_envvars.v.in_gh_action then "True" else "False");
        ]) )

(* from login.py *)
let save_token ~echo_token settings token =
  if Semgrep_App.get_deployment_from_token token <> None then
    let settings = Semgrep_settings.{ settings with api_token = Some token } in
    if Semgrep_settings.save settings then (
      Logs.app (fun m ->
          m "Saved login token@.@.\t%s@.@.in %a"
            (if echo_token then token else "<redacted>")
            Fpath.pp Semgrep_envvars.v.user_settings_file);
      Logs.app (fun m ->
          m
            "Note: You can always generate more tokens at \
             %s/orgs/-/settings/tokens"
            (Uri.to_string Semgrep_envvars.v.semgrep_url));
      true)
    else false
  else (
    Logs.err (fun m -> m "Login token is not valid. Please try again.");
    false)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let wait_between_retry_in_sec = 6 (* So every 10 retries is a minute *)
let max_retries = 30 (* Give users 3 minutes to log in / open link *)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Login_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.logging_level;
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | None -> (
      match Semgrep_envvars.v.app_token with
      | Some token when String.length token > 0 ->
          if save_token ~echo_token:false settings token then Exit_code.ok
          else Exit_code.fatal
      | None
      | Some _ ->
          if not Unix.(isatty stdin) then (
            Logs.err (fun m ->
                m
                  "Error: semgrep login is an interactive command: run in an \
                   interactive terminal (or define SEMGREP_APP_TOKEN)");
            Exit_code.fatal)
          else
            let session_id, url = make_login_url () in
            Logs.app (fun m ->
                m
                  "Login enables additional proprietary Semgrep Registry rules \
                   and running custom policies from Semgrep App.");
            Logs.app (fun m -> m "Login at: %s" (Uri.to_string url));
            Logs.app (fun m ->
                m
                  "@.Once you've logged in, return here and you'll be ready to \
                   start using new Semgrep rules.");
            let rec fetch = function
              | 0 ->
                  Logs.err (fun m ->
                      m
                        "Failed to login: please check your internet \
                         connection or contact support@r2c.dev");
                  Exit_code.fatal
              | n -> (
                  let url =
                    Uri.with_path Semgrep_envvars.v.semgrep_url
                      "api/agent/tokens/requests"
                  in
                  let body =
                    {|{"token_request_key": "|} ^ Uuidm.to_string session_id
                    ^ {|"}|}
                  in
                  match Http_helpers.post ~body url with
                  | Ok body -> (
                      try
                        match Yojson.Basic.from_string body with
                        | `Assoc e -> (
                            match List.assoc_opt "token" e with
                            | Some (`String token) ->
                                if save_token ~echo_token:true settings token
                                then Exit_code.ok
                                else Exit_code.fatal
                            | None
                            | Some _ ->
                                Logs.debug (fun m ->
                                    m "failed to decode json token %s" body);
                                Exit_code.fatal)
                        | _ ->
                            Logs.debug (fun m ->
                                m "failed to decode json %s" body);
                            Exit_code.fatal
                      with
                      | Yojson.Json_error msg ->
                          Logs.debug (fun m ->
                              m "failed to parse json %s: %s" msg body);
                          Exit_code.fatal)
                  | Error (status_code, msg) ->
                      if status_code = 404 then (
                        Unix.sleep wait_between_retry_in_sec;
                        fetch (n - 1))
                      else (
                        Logs.err (fun m ->
                            m
                              "Unexpected failure from %s: status code %d; \
                               please contact support@r2c.dev if this persists"
                              (Uri.to_string Semgrep_envvars.v.semgrep_url)
                              status_code);
                        Logs.info (fun m -> m "HTTP error: %s" msg);
                        Exit_code.fatal))
            in
            fetch max_retries)
  | Some _ ->
      Logs.app (fun m ->
          m
            "API token already exists in %s. To login with a different token \
             logout use `semgrep logout`"
            (Fpath.to_string Semgrep_envvars.v.user_settings_file));
      Exit_code.fatal

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Login_CLI.parse_argv Login_CLI.login_cmdline_info argv in
  run conf
