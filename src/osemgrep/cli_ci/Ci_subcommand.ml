(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-ci command, execute it and exit.

   Translated from ci.py
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Ci_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:conf.force_color
    ~level:conf.logging_level;
  Metrics_.configure conf.metrics;
  let settings = Semgrep_settings.load ~legacy:conf.legacy () in
  let deployment =
    match (settings.api_token, conf.rules_source) with
    | None, Rules_source.Configs [] ->
        Logs.app (fun m ->
            m "run `semgrep login` before using `semgrep ci` or set `--config`");
        Error Exit_code.invalid_api_key
    | Some _, Rules_source.Configs (_ :: _) ->
        Logs.app (fun m ->
            m
              "Cannot run `semgrep ci` with --config while logged in. The \
               `semgrep ci` command will upload findings to semgrep-app and \
               those findings must come from rules configured there. Drop the \
               `--config` to use rules configured on semgrep.dev or log out.");
        Error Exit_code.fatal
    | None, _ -> Ok None
    | Some token, _ -> (
        match Semgrep_App.get_deployment_from_token ~token with
        | None ->
            Logs.app (fun m ->
                m
                  "API token not valid. Try to run `semgrep logout` and \
                   `semgrep login` again.");
            Error Exit_code.invalid_api_key
        | Some t -> Ok (Some t))
  in
  match deployment with
  | Error e -> e
  | Ok depl ->
      Logs.app (fun m -> m "%a" Fmt_helpers.pp_heading "Debugging Info");
      Logs.app (fun m ->
          m "  %a" Fmt.(styled `Underline string) "SCAN ENVIRONMENT");
      (* TODO: "on python sys.version_info.micro" *)
      Logs.app (fun m ->
          m "  versions    - semgrep %a"
            Fmt.(styled `Bold string)
            Version.version);
      (* TODO: use metadata.environment and metadata.event_name *)
      Logs.app (fun m ->
          m
            "  environment - running in environment %a, triggering event is \
             %a@."
            Fmt.(styled `Bold string)
            "git"
            Fmt.(styled `Bold string)
            "unknown");
      (* TODO: fix_head_if_github_action(metadata) *)
      let _rules_source =
        match depl with
        | None -> conf.rules_source
        | Some deployment ->
            Logs.app (fun m ->
                m "  %a" Fmt.(styled `Underline string) "CONNECTION");
            Logs.app (fun m ->
                m "  Reporting start of scan for %a"
                  Fmt.(styled `Bold string)
                  deployment);
            (* TODO: scan_handler.start_scan(metadata_dict) *)
            let at_url_maybe ppf () = Fmt.string ppf "" in
            (* TODO
               at_url_maybe = (
                        f" at [bold]{state.env.semgrep_url}[/bold]"
                        if state.env.semgrep_url != "https://semgrep.dev"
                        else ""
                    )
            *)
            Logs.app (fun m ->
                m "  Fetching configuration from Semgrep Cloud Platform%a"
                  at_url_maybe ());
            (* TODO scan_handler.fetch_and_init_scan_config(metadata_dict) *)
            conf.rules_source
      in
      (* TODO: do the actual scan, and reporting *)
      Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Ci_CLI.parse_argv argv in
  run conf
