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
      Logs.info (fun m ->
          m "Github CLI already installed, skipping installation")
  | false ->
      Logs.info (fun m -> m "Github CLI not installed, installing now");
      install_gh_cli ()

let test_gh_authed () : bool =
  let cmd = Bos.Cmd.(v "gh" % "auth" % "status") in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> true
  | _ -> false

let prompt_gh_auth () =
  let cmd = Bos.Cmd.(v "gh" % "auth" % "login" % "--web") in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | _ -> ()

let prompt_gh_auth_if_needed () =
  let authed = test_gh_authed () in
  match authed with
  | true ->
      Logs.info (fun m ->
          m "Github CLI already logged in, skipping authentication")
  | false ->
      Logs.info (fun m -> m "Prompting Github CLI authentication");
      prompt_gh_auth ()

let test_semgrep_workflow_added ~repo : bool =
  let repo_path =
    match repo with
    | _ when Common2.is_directory repo -> Fpath.to_dir_path Fpath.(v repo)
    | _ -> Bos.OS.Dir.current () |> Rresult.R.get_ok
  in
  Logs.info (fun m ->
      m "Checking for semgrep workflow in %a" Fpath.pp repo_path);
  let cmd =
    match repo with
    | _ when Common2.is_directory repo ->
        Bos.Cmd.(v "gh" % "workflow" % "view" % "semgrep.yml")
    | _ ->
        Bos.Cmd.(v "gh" % "workflow" % "view" % "semgrep.yml" % "--repo" % repo)
  in
  match
    Bos.OS.Dir.with_current repo_path
      (fun () ->
        match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
        | Ok _ -> true
        | _ -> false)
      ()
  with
  | Ok _ -> true
  | _ -> false

let print_help () = Printf.printf {| Hello World |}

let add_semgrep_workflow ~repo =
  let added = test_semgrep_workflow_added ~repo in
  match added with
  | true -> Logs.info (fun m -> m "Semgrep workflow already present, skipping")
  | false ->
      Logs.info (fun m -> m "Preparing Semgrep workflow");
      print_help ()

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run (conf : Install_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:true ~level:conf.logging_level;
  Logs.debug (fun m ->
      m "Running install command with env %s"
        (Install_CLI.show_ci_env_flavor conf.ci_env));
  Logs.debug (fun m ->
      m "Running with repo %s" (Install_CLI.get_repo conf.repo));
  let settings = Semgrep_settings.load () in
  let api_token = settings.Semgrep_settings.api_token in
  match api_token with
  | None ->
      Logs.err (fun m ->
          m
            "%s You are not logged in! Run `semgrep login` before using \
             `semgrep install`"
            (Logs_helpers.with_err_tag ()));
      Exit_code.fatal
  | Some _ ->
      Logs.app (fun m ->
          install_gh_cli_if_needed ();
          prompt_gh_auth_if_needed ();
          add_semgrep_workflow ~repo:(Install_CLI.get_repo conf.repo);
          m "%s Installed semgrep for this repository"
            (Logs_helpers.with_success_tag ()));
      Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (argv : string array) : Exit_code.t =
  let conf = Install_CLI.parse_argv argv in
  run conf
