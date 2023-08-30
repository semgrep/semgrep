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
let save_token settings token =
  if Semgrep_App.get_deployment_from_token token <> None then
    let settings = Semgrep_settings.{ settings with api_token = Some token } in
    if Semgrep_settings.save settings then (
      Logs.app (fun m ->
          m "\nSaved access token in %a" Fpath.pp
            Semgrep_envvars.v.user_settings_file);
      let epilog =
        Ocolor_format.asprintf
          {|
💡 From now on you can run @{<cyan>`semgrep ci`@} to start a Semgrep scan.
   Supply Chain, Secrets and Pro rules will be applied in your scans automatically.

💎 Happy scanning!
|}
      in
      Logs.app (fun m -> m "%s" epilog);
      true)
    else false
  else (
    Logs.err (fun m -> m "Login token is not valid. Please try again.");
    false)

(*****************************************************************************)
(* Console Experience *)
(*****************************************************************************)

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let show_spinner ~frame_index:i =
  let spinner = spinner.(i mod Array.length spinner) in
  ANSITerminal.set_cursor 1 (-1);
  ANSITerminal.printf [ ANSITerminal.green ] "%s Waiting for sign in..." spinner

let erase_spinner () =
  ANSITerminal.move_cursor 0 (-1);
  ANSITerminal.move_bol ();
  ANSITerminal.erase ANSITerminal.Below

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let min_wait_between_retry_in_ms = 2000 (* initially start with 2 seconds *)

let next_wait_offset_in_ms =
  ref 1000 (* increase wait time relative to number of attempts *)

let max_retries = 12 (* Give users ~2 minutes to log in *)

let apply_backoff () =
  next_wait_offset_in_ms :=
    Float.to_int (Float.ceil (Float.of_int !next_wait_offset_in_ms *. 1.3))

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Login_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.logging_level;
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | None -> (
      match Semgrep_envvars.v.app_token with
      | Some token when String.length token > 0 ->
          if save_token settings token then Exit_code.ok else Exit_code.fatal
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
            Logs.app (fun m -> m "%a" Fmt_helpers.pp_heading "Login");
            let preamble =
              Ocolor_format.asprintf
                {|
Logging in gives you access to Supply Chain, Secrets and Pro rules.

Plus, you can manage your rules and code findings with Semgrep Cloud Platform.

@{<ul>Steps@}
1. Sign in with your authentication provider
2. Activate your access token
3. Return here and start scanning!
|}
            in
            Logs.app (fun m -> m "%s" preamble);
            let cmd = Bos.Cmd.(v "open" % Uri.to_string url) in
            let () =
              let res = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string in
              match res with
              | Ok _ ->
                  Logs.app (fun m ->
                      m "Opening your sign-in link automatically...");
                  let msg =
                    Ocolor_format.asprintf
                      "If nothing happened, please open this link in your \
                       browser:\n\n\
                       @{<cyan;ul>%s@}\n"
                      (Uri.to_string url)
                  in
                  Logs.app (fun m -> m "%s" msg)
              | __else__ -> ()
            in
            let rec fetch = function
              | 0 ->
                  Logs.err (fun m ->
                      m
                        "%s Failed to login: please check your internet \
                         connection or contact support@semgrep.com"
                        (Logs_helpers.with_err_tag ()));
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
                      erase_spinner ();
                      (* Remove the spinner from the console *)
                      try
                        let json = Yojson.Basic.from_string body in
                        let open Yojson.Basic.Util in
                        match json |> member "token" with
                        | `String token ->
                            if save_token settings token then (
                              let user_name =
                                json |> member "user_name" |> to_string
                              in
                              Logs.app (fun m ->
                                  m
                                    "%s Successfully logged in as %s! You can \
                                     now run `semgrep ci` to start a scan."
                                    (Logs_helpers.with_success_tag ())
                                    user_name);
                              Exit_code.ok)
                            else Exit_code.fatal
                        | `Null
                        | _ ->
                            Logs.debug (fun m ->
                                m "failed to decode json token %s" body);
                            Exit_code.fatal
                      with
                      | Yojson.Json_error msg ->
                          Logs.debug (fun m ->
                              m "failed to parse json %s: %s" msg body);
                          Exit_code.fatal)
                  | Error (status_code, msg) ->
                      if status_code = 404 then (
                        let steps = 100 in
                        for i = 1 to steps do
                          (* 100 steps for each iteration with variable sleep time between *)
                          show_spinner ~frame_index:i;
                          Unix.sleepf
                            (Float.of_int
                               (min_wait_between_retry_in_ms
                              + !next_wait_offset_in_ms)
                            /. Float.of_int (1000 * steps))
                        done;
                        apply_backoff ();
                        fetch (n - 1))
                      else (
                        Logs.err (fun m ->
                            m
                              "%s Unexpected failure from %s: status code %d; \
                               please contact support@semgrep.com if this \
                               persists"
                              (Logs_helpers.with_err_tag ())
                              (Uri.to_string url) status_code);
                        Logs.info (fun m -> m "HTTP error: %s" msg);
                        Exit_code.fatal))
            in
            Unix.sleepf 0.1;
            (* wait 100ms for the browser to open and then start showing the spinner *)
            fetch max_retries)
  | Some _ ->
      Logs.app (fun m ->
          m
            "%s API token already exists in %s. To login with a different \
             token logout use `semgrep logout`"
            (Logs_helpers.with_err_tag ())
            (Fpath.to_string Semgrep_envvars.v.user_settings_file));
      Exit_code.fatal

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Login_CLI.parse_argv Login_CLI.login_cmdline_info argv in
  run conf
