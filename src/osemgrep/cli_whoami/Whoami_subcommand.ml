(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)
module Http_helpers = Http_helpers.Make (Lwt_platform)

let get_identity_async ~token =
  let headers =
    [
      ("Authorization", Fmt.str "Bearer %s" token);
      ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
    ]
  in
  let url =
    Uri.(with_path !Semgrep_envvars.v.semgrep_url "api/agent/identity")
  in
  let%lwt res = Http_helpers.get_async ~headers url in
  match res with
  | Ok body -> Lwt.return body
  | Error msg ->
      Logs.err (fun m ->
          m "Failed to download config from %s: %s" (Uri.to_string url) msg);
      Lwt.return ""

let get_identity ~token = Lwt_platform.run (get_identity_async ~token)

let run (_conf : Whoami_CLI.conf) : Exit_code.t =
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  match api_token with
  | None ->
      Logs.err (fun m ->
          m
            "%s You are not logged in! Run `semgrep login` before using \
             `semgrep whoami`"
            (Logs_helpers.warn_tag ()));
      Exit_code.fatal
  | Some token ->
      let identity = get_identity ~token in
      Logs.app (fun m ->
          m "%s You are logged in as %s" (Logs_helpers.success_tag ()) identity);
      Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (argv : string array) : Exit_code.t =
  let conf = Whoami_CLI.parse_argv argv in
  run conf
