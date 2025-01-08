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
(* We need Cap.exec because we call the 'open' command line tool to
 * open a browser.
 *)
type caps = < Cap.stdout ; Cap.network ; Cap.exec >

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
let save_token ?display_name token =
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
  Logs.app (fun m -> m "%s" (Console.heading "Login"));
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

(* Print out the flow, create the activation url and open the url in a browser *)
let start_interactive_flow (caps : < Cap.exec ; .. >) : Uuidm.t option =
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
      match CapExec.status_of_run caps#exec cmd with
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
let run_conf (caps : < caps ; .. >) (conf : Login_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.common.logging_level;
  Logs.debug (fun m -> m "conf = %s" (Login_CLI.show_conf conf));
  (* stricter: the login/logout metrics are actually not tracked in pysemgrep *)
  Metrics_.configure Metrics_.On;
  (* don't include env here since we care if the token comes from the env or a
     file, and Semgrep_setting.load will check for either if include_env is
     true (the default)*)
  (* Also, Settings.load checks if token is well formed *)
  let settings = Semgrep_settings.load ~include_env:false () in
  (* Ensure that the env token is also well formed *)
  let app_token_opt =
    Option.bind !Semgrep_envvars.v.app_token (fun token ->
        if Auth.well_formed token then Some token else None)
  in
  (* Weird logic for logging in that's from login.py. *)
  match (settings.Semgrep_settings.api_token, app_token_opt) with
  | None, None when String.length conf.one_time_seed > 0 ->
      let shared_secret = Uuidm.v5 Uuidm.nil conf.one_time_seed in
      Logs.debug (fun m ->
          m "using seed %s with uuid %s" conf.one_time_seed
            (Uuidm.to_string shared_secret));
      fetch_token caps shared_secret
  (* If the token only exists in env, and well formed, exit ok *)
  | None, Some token ->
      Logs.debug (fun m ->
          m
            "Token in settings file is unset, environment variable \
             (SEMGREP_API_TOKEN) is set and well formed, saving the env token \
             to the settings file and exiting ok");
      let caps = Auth.cap_token_and_network token caps in
      save_token caps
  | Some file_token, Some env_token ->
      (* If the token exists in both locations, but aren't the same, tell user
         and exit error *)
      if not Auth.(equal file_token env_token) then (
        Logs.err (fun m ->
            m
              "API token set in both settings file, and environment variable \
               (SEMGREP_API_TOKEN), but the tokens differ. To login with a \
               different token logout use `semgrep logout`");
        Exit_code.fatal ~__LOC__
        (* If the token exists in both locations, but are the same, save the token and exit ok *)
        (* It's weird we save anyways, since they're equivalent but that's what pysemgrep does *))
      else (
        Logs.debug (fun m ->
            m
              "Token is set in settings file, and environment variable \
               (SEMGREP_API_TOKEN) is set and well formed, using env var token \
               and exiting ok");
        let caps = Auth.cap_token_and_network env_token caps in
        save_token caps)
  (* If the token exists in the settings file, nowhere else, exit error *)
  | Some _, None ->
      (* TODO: why not Logs.err instead? *)
      Logs.app (fun m ->
          m
            "%s You're already logged in. Use `semgrep logout` to log out \
             first, and then you can login with a new access token."
            (Console.error_tag ()));
      Exit_code.fatal ~__LOC__
  (* Token doesn't exist, or it's in the env and not well formed *)
  | None, None -> (
      Logs.debug (fun m ->
          m
            "Token is not set in settings file, and environment variable \
             (SEMGREP_API_TOKEN) is not set or not well formed, starting \
             interactive login flow");
      let session_id = start_interactive_flow caps in
      match session_id with
      | None -> Exit_code.fatal ~__LOC__
      | Some session_id -> (
          Unix.sleepf 0.1;
          (* wait 100ms for the browser to open and then start showing the spinner *)
          match
            Semgrep_login.fetch_token ~wait_hook:Console_Spinner.show_spinner
              caps session_id
          with
          | Error msg ->
              Logs.err (fun m -> m "%s" msg);
              Exit_code.fatal ~__LOC__
          | Ok (token, display_name) ->
              Console_Spinner.erase_spinner ();
              let caps = Auth.cap_token_and_network token caps in
              save_token caps ~display_name))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (caps : < caps ; .. >) (argv : string array) : Exit_code.t =
  let conf = Login_CLI.parse_argv argv in
  run_conf caps conf
