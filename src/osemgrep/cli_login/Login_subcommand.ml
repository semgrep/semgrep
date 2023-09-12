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

(* from login.py *)
let save_token token =
  match Semgrep_login.save_token token with
  | Ok () ->
      Logs.app (fun m ->
          m "\nSaved access token in %a" Fpath.pp
            !Semgrep_envvars.v.user_settings_file);
      let epilog =
        Ocolor_format.asprintf
          {|
💡 From now on you can run @{<cyan>`semgrep ci`@} to start a Semgrep scan.
   Supply Chain, Secrets and Pro rules will be applied in your scans automatically.

💎 Happy scanning!
|}
      in
      Logs.app (fun m -> m "%s" epilog);
      Exit_code.ok
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      Exit_code.fatal

(*****************************************************************************)
(* Console Experience *)
(*****************************************************************************)

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let show_spinner () =
  for frame_index = 1 to 100 do
    let spinner = spinner.(frame_index mod Array.length spinner) in
    ANSITerminal.set_cursor 1 (-1);
    ANSITerminal.printf [ ANSITerminal.green ] "%s Waiting for sign in..."
      spinner
  done

let erase_spinner () =
  ANSITerminal.move_cursor 0 (-1);
  ANSITerminal.move_bol ();
  ANSITerminal.erase ANSITerminal.Below

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Login_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.logging_level;
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | None -> (
      match !Semgrep_envvars.v.app_token with
      | Some token when String.length token > 0 -> save_token token
      | None
      | Some _ -> (
          if not Unix.(isatty stdin) then (
            Logs.err (fun m ->
                m
                  "Error: semgrep login is an interactive command: run in an \
                   interactive terminal (or define SEMGREP_APP_TOKEN)");
            Exit_code.fatal)
          else if true then (
            (* FAKE hook *)
            let payload =
              [
                ("deployment_id", "12345");
                ("deployment_name", "zz");
                ("user_id", "1234");
                ("user_name", "zzeleznick");
                ("token", "abcdefg");
              ]
            in
            let signed_token = Jwto.encode Jwto.HS256 "secret" payload in
            let signed_token = Result.get_ok signed_token in
            Logs.app (fun m -> m "%s" signed_token);
            let decoded_token = Jwto.decode signed_token in
            Result.iter
              (fun v ->
                Logs.app (fun m ->
                    m "decoded: %s"
                      (Jwto.get_payload v |> Jwto.payload_to_string)))
              decoded_token;
            Exit_code.ok)
          else
            let session_id, url = Semgrep_login.make_login_url () in
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
            Unix.sleepf 0.1;
            (* wait 100ms for the browser to open and then start showing the spinner *)
            match
              Semgrep_login.fetch_token ~wait_hook:show_spinner (session_id, url)
            with
            | Error msg ->
                Logs.err (fun m -> m "%s" msg);
                Exit_code.fatal
            | Ok (token, user_name) ->
                erase_spinner ();

                Logs.app (fun m ->
                    m
                      "%s Successfully logged in as %s! You can now run \
                       `semgrep ci` to start a scan."
                      (Logs_helpers.success_tag ())
                      user_name);
                save_token token))
  | Some _ ->
      Logs.app (fun m ->
          m
            "%s You're already logged in. Use `semgrep logout` to log out \
             first, and then you can login with a new access token."
            (Logs_helpers.err_tag ()));
      Exit_code.fatal
(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Login_CLI.parse_argv Login_CLI.login_cmdline_info argv in
  run conf
