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

let print_did_save_token () : unit =
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
  Logs.app (fun m -> m "%s" epilog)

(* Helper to call our save token implementation when the token is passed as an env var *)
let save_token token =
  match Semgrep_login.save_token token with
  | Ok () ->
      print_did_save_token ();
      Exit_code.ok
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      Exit_code.fatal

let print_preamble () : unit =
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
  Logs.app (fun m -> m "%s" preamble)

(* Print out the flow, create the activation url and open the url in the browser *)
let start_interactive_flow () : Uuidm.t option =
  if not Unix.(isatty stdin) then (
    Logs.err (fun m ->
        m
          "Error: semgrep login is an interactive command: run in an \
           interactive terminal (or define SEMGREP_APP_TOKEN)");
    None)
  else (
    print_preamble ();
    let session_id, url = Semgrep_login.make_login_url () in
    let cmd = Bos.Cmd.(v "open" % Uri.to_string url) in
    let res = Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string in
    match res with
    | Ok _ ->
        Logs.app (fun m -> m "Opening your sign-in link automatically...");
        let msg =
          Ocolor_format.asprintf
            "If nothing happened, please open this link in your browser:\n\n\
             @{<cyan;ul>%s@}\n"
            (Uri.to_string url)
        in
        Logs.app (fun m -> m "%s" msg);
        Some session_id
    | __else__ -> None)

(* NOTE: fetch_token will save the token iff valid (else error) *)
let fetch_token session_id =
  match
    Semgrep_login.fetch_token ~wait_hook:Console_Spinner.show_spinner session_id
  with
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      Exit_code.fatal
  | Ok (_, display_name) ->
      Console_Spinner.erase_spinner ();
      Logs.app (fun m ->
          m
            "%s Successfully logged in as %s! You can now run `semgrep ci` to \
             start a scan."
            (Logs_helpers.success_tag ())
            display_name);
      print_did_save_token ();
      Exit_code.ok

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Login_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | None -> (
      match !Semgrep_envvars.v.app_token with
      | Some token when String.length token > 0 -> save_token token
      | None when String.length conf.init > 0 ->
          let shared_secret = Uuidm.v5 Uuidm.nil conf.init in
          Logs.debug (fun m ->
              m "using seed %s with uuid %s" conf.init
                (Uuidm.to_string shared_secret));
          fetch_token shared_secret
      | None
      | Some _ -> (
          (* Print a few messages to the console and open the active link *)
          match start_interactive_flow () with
          | None -> Exit_code.fatal
          | Some session_id ->
              (* wait 100ms for the browser to open and then start showing the spinner *)
              Unix.sleepf 0.1;
              fetch_token session_id))
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
