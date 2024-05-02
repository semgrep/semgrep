(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-login command, execute it and exit.

   Note that in practice this subcommand is not as critical as it seems.
   Indeed, in most cases one does not have to login in CI but instead one
   can set the SEMGREP_APP_TOKEN in the environment which is then used
   by 'semgrep ci'.

   Translated from login.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type caps = < Cap.stdout ; Cap.network >

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let print_success_message display_name : unit =
  let message =
    Ocolor_format.asprintf {|%s Successfully logged in as @{<cyan>%s@}! |}
      (Console.success_tag ()) display_name
  in
  Logs.app (fun m -> m "%s" message)

let print_did_save_token () : unit =
  Logs.app (fun m ->
      m "\nSaved access token in %a" Fpath.pp
        !Semgrep_envvars.v.user_settings_file);
  let epilog =
    Ocolor_format.asprintf
      {|
💡 From now on you can run @{<cyan>`semgrep ci`@} to start a Semgrep scan.
   Supply Chain, Secrets, and Pro rules will be applied in your scans automatically.

💎 Happy scanning!
  |}
  in
  Logs.app (fun m -> m "%s" epilog)

(* Helper to call our save token implementation when the token is passed as an env var *)
let save_token ?(display_name = None) token =
  match Semgrep_login.save_token token with
  | Ok deployment_config ->
      print_did_save_token ();
      let display_name =
        match display_name with
        | Some name -> name
        | None -> deployment_config.display_name
      in
      print_success_message display_name;
      Exit_code.ok ~__LOC__
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      Exit_code.fatal ~__LOC__

let print_preamble () : unit =
  Logs.app (fun m -> m "%a" Fmt_.pp_heading "Login");
  let preamble =
    Ocolor_format.asprintf
      {|
Logging in gives you access to Supply Chain, Secrets and Pro rules.

Plus, you can manage your rules and code findings with Semgrep Cloud Platform.

@{<ul>Steps@}
1. Sign in with your authentication provider.
2. Activate your access token.
3. Return here and start scanning!
|}
  in
  Logs.app (fun m -> m "%s" preamble)

(* Print out the flow, create the activation url and open the url in the browser *)
let start_interactive_flow () : Uuidm.t option =
  if not Unix.(isatty stdin) then (
    let msg =
      Ocolor_format.asprintf
        {|%s @{<cyan>`semgrep login`@} is meant to be run in an interactive terminal.
You can pass @{<cyan>`SEMGREP_APP_TOKEN`@} as an environment variable instead.|}
        (Console.error_tag ())
    in
    Logs.err (fun m -> m "%s" msg);
    None)
  else (
    print_preamble ();
    let session_id, url = Semgrep_login.make_login_url () in
    if Sys.win32 then (
      let msg =
        Ocolor_format.asprintf
          "Please open this link in your browser:\n\n@{<cyan;ul>%s@}\n"
          (Uri.to_string url)
      in
      Logs.app (fun m -> m "%s" msg);
      Some session_id)
    else
      let cmd = (Cmd.Name "open", [ Uri.to_string url ]) in
      match UCmd.status_of_run cmd with
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
let fetch_token caps session_id =
  match
    Semgrep_login.fetch_token caps ~wait_hook:Console_Spinner.show_spinner
      session_id
  with
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      Exit_code.fatal ~__LOC__
  | Ok (_, display_name) ->
      Console_Spinner.erase_spinner ();
      print_did_save_token ();
      print_success_message display_name;
      Exit_code.ok ~__LOC__

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run_conf (caps : caps) (conf : Login_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  let settings = Semgrep_settings.load () in
  match settings.Semgrep_settings.api_token with
  | None -> (
      match !Semgrep_envvars.v.app_token with
      | Some token when String.length (Auth.string_of_token token) > 0 ->
          let caps = Auth.cap_token_and_network token caps in
          save_token ~display_name:None caps
      | None when String.length conf.one_time_seed > 0 ->
          let shared_secret = Uuidm.v5 Uuidm.nil conf.one_time_seed in
          Logs.debug (fun m ->
              m "using seed %s with uuid %s" conf.one_time_seed
                (Uuidm.to_string shared_secret));
          fetch_token caps shared_secret
      | None
      | Some _ -> (
          let session_id = start_interactive_flow () in
          match session_id with
          | None -> Exit_code.fatal ~__LOC__
          | Some session_id -> (
              Unix.sleepf 0.1;
              (* wait 100ms for the browser to open and then start showing the spinner *)
              match
                Semgrep_login.fetch_token
                  ~wait_hook:Console_Spinner.show_spinner caps session_id
              with
              | Error msg ->
                  Logs.err (fun m -> m "%s" msg);
                  Exit_code.fatal ~__LOC__
              | Ok (token, display_name) ->
                  Console_Spinner.erase_spinner ();
                  let caps = Auth.cap_token_and_network token caps in
                  save_token caps ~display_name:(Some display_name))))
  | Some _ ->
      Logs.app (fun m ->
          m
            "%s You're already logged in. Use `semgrep logout` to log out \
             first, and then you can login with a new access token."
            (Console.error_tag ()));
      Exit_code.fatal ~__LOC__

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : caps) (argv : string array) : Exit_code.t =
  let conf = Login_CLI.parse_argv Login_CLI.login_cmdline_info argv in
  run_conf caps conf
