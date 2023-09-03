(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is a command to install semgrep (in CI) for a given repository *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let install_gh_cli () =
  let cmd = Bos.Cmd.(v "brew" % "install" % "github") in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> Logs.app (fun m -> m "Github cli installed successfully")
  | _ ->
      Logs.err (fun m ->
          m "%s Github cli failed to install" (Logs_helpers.with_err_tag ()))

let test_gh_cli () : bool =
  let cmd = Bos.Cmd.(v "command" % "-v" % "gh") in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> true
  | _ -> false

let install_gh_cli_if_needed () =
  let installed = test_gh_cli () in
  match installed with
  | true ->
      Logs.app (fun m ->
          m "Github cli already installed, skipping installation")
  | false ->
      Logs.app (fun m -> m "Github cli not installed, installing now");
      install_gh_cli ()

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run (conf : Install_CLI.conf) : Exit_code.t =
  Logs.app (fun m ->
      m "Running install command with env %s"
        (Install_CLI.show_ci_env_flavor conf.ci_env));
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  match api_token with
  | None ->
      Logs.app (fun m ->
          m
            "%s You are not logged in! Run `semgrep login` before using \
             `semgrep install`"
            (Logs_helpers.with_err_tag ()));
      Exit_code.fatal
  | Some _ ->
      Logs.app (fun m ->
          install_gh_cli_if_needed ();
          m "%s Installed semgrep for this repository"
            (Logs_helpers.with_success_tag ()));
      Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (argv : string array) : Exit_code.t =
  let conf = Install_CLI.parse_argv argv in
  run conf
