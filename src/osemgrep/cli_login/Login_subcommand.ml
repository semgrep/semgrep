open Cmdliner

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-login command, execute it and exit.

   Translated from login.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* no CLI parameters for now *)
type conf = unit

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* this could be moved in a Login_CLI.ml file at some point *)
let cmdline_term : conf Term.t =
  let combine = () in
  Term.(const combine)

let doc = "Obtain and save credentials for semgrep.dev"

let man : Manpage.block list =
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

let cmdline_info : Cmd.info = Cmd.info "semgrep login" ~doc ~man

let parse_argv (argv : string array) : conf =
  let cmd : conf Cmd.t = Cmd.v cmdline_info cmdline_term in
  CLI_common.eval_value ~argv cmd

let make_login_url () =
  let session_id = Uuidm.v `V4 in
  ( session_id,
    Uri.(
      add_query_params
        (with_path Semgrep_envvars.env.semgrep_url "login")
        [
          ("cli-token", [ Uuidm.to_string session_id ]);
          ( "docker",
            [ (if Semgrep_envvars.env.in_docker then "True" else "False") ] );
          ( "gha",
            [ (if Semgrep_envvars.env.in_gh_action then "True" else "False") ]
          );
        ]) )

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let wait_between_retry_in_sec = 6 (* So every 10 retries is a minute *)
let max_retries = 30 (* Give users 3 minutes to log in / open link *)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (_conf : conf) : Exit_code.t =
  Logs_helpers.setup_logging ~force_color:false ~level:(Some Logs.Debug);
  Logs.info (fun m -> m "Semgrep version: %s" Version.version);
  Logs.info (fun m ->
      m "Executed as: %s" (Sys.argv |> Array.to_list |> String.concat " "));
  let settings = Semgrep_settings.get () in
  match settings.Semgrep_settings.api_token with
  | None -> (
      match Semgrep_envvars.env.app_token with
      | Some token when String.length token > 0 ->
          let settings =
            Semgrep_settings.{ settings with api_token = Some token }
          in
          Semgrep_settings.save settings;
          Exit_code.ok
      | None
      | Some _ ->
          (* if not auth.is_a_tty():
             click.echo(
                 f"Error: semgrep login is an interactive command: run in an interactive terminal (or define SEMGREP_APP_TOKEN)",
                 err=True,
             )
             sys.exit(FATAL_EXIT_CODE)
          *)
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
                      "Failed to login: please check your internet connection \
                       or contact support@r2c.dev");
                Exit_code.fatal
            | n -> (
                let url =
                  Uri.with_path Semgrep_envvars.env.semgrep_url
                    "api/agent/tokens/requests"
                in
                let body =
                  {|{"token_request_key": "|} ^ Uuidm.to_string session_id
                  ^ {|"}|}
                in
                match Network.post ~body url with
                | Ok body -> (
                    try
                      match Yojson.Basic.from_string body with
                      | `Assoc e -> (
                          match List.assoc_opt "token" e with
                          | Some (`String token) ->
                              let settings =
                                Semgrep_settings.
                                  { settings with api_token = Some token }
                              in
                              Semgrep_settings.save settings;
                              (* TODO if save failed, Exit_code.fatal *)
                              Exit_code.ok
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
                            (Uri.to_string Semgrep_envvars.env.semgrep_url)
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
            Semgrep_envvars.env.user_settings_file);
      Exit_code.fatal

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = parse_argv argv in
  run conf
