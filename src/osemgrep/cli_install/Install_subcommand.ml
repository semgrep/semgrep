(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This is a command to install semgrep (in CI) for a given repository *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let install_gh_cli () =
  (* NOTE: This only supports mac users and we would need to direct users to
     their own platform-specific instructions at https://github.com/cli/cli#installation
  *)
  let cmd = Bos.Cmd.(v "brew" % "install" % "github") in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> Logs.app (fun m -> m "Github cli installed successfully")
  | _ ->
      Logs.err (fun m ->
          m "%s Github cli failed to install" (Logs_helpers.with_err_tag ()));
      Error.abort
        (Printf.sprintf
           "Please install the Github CLI manually by following the \
            instructions at %s"
           "https://github.com/cli/cli#installation")

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

(* TODO: handle GitHub Enterprise *)
let set_ssh_as_default () =
  let cmd =
    Bos.Cmd.(
      v "gh" % "config" % "set" % "git_protocol" % "ssh" % "--host"
      % "github.com")
  in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> ()
  | _ -> Error.abort "failed to set git_protocol as ssh"

let test_semgrep_workflow_added ~repo : bool =
  let repo_path =
    match repo with
    | _ when Common2.dir_exists repo -> Fpath.to_dir_path Fpath.(v repo)
    | _ -> Bos.OS.Dir.current () |> Rresult.R.get_ok
  in
  Logs.info (fun m ->
      m "Checking for semgrep workflow in %a" Fpath.pp repo_path);
  let cmd =
    match repo with
    | _ when Common2.dir_exists repo ->
        Bos.Cmd.(v "gh" % "workflow" % "view" % "semgrep.yml")
    | _ ->
        Bos.Cmd.(v "gh" % "workflow" % "view" % "semgrep.yml" % "--repo" % repo)
  in
  let res = ref false in
  match
    Bos.OS.Dir.with_current repo_path
      (fun () ->
        match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
        | Ok _ -> res := true
        | _ -> res := false)
      ()
  with
  | Ok _ -> !res
  | _ -> false

let sprint_workflow () =
  {|
# This workflow runs Semgrep on pull requests and pushes to the main branch

name: semgrep
on:
  workflow_dispatch: {}
  pull_request_target: {}
  push:
    branches:
    # This workflow will run against PRs on the following default branches
      - develop
      - main
      - master

  jobs:
    semgrep:
        name: semgrep/ci
        runs-on: ubuntu-latest
        if: (github.actor != 'dependabot[bot]')
        env:
            SEMGREP_APP_TOKEN: ${{ secrets.SEMGREP_APP_TOKEN }}
        container:
            image: returntocorp/semgrep
        steps:
            - uses: actions/checkout@v3
            - run: semgrep ci
|}

(* NOTE: we use the gh repo clone subcommand over
   the regular git clone as the subcommand allows for
   both OWNER/REPO and cannonical GitHub URLs as arguments
   to clone the repo.
*)
let clone_repo ~repo =
  let cmd =
    Bos.Cmd.(v "gh" % "repo" % "clone" % repo % "--" % "--depth" % "1")
  in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> ()
  | _ -> Error.abort (Printf.sprintf "failed to clone remote repo: %s" repo)

let clone_repo_to ~repo ~dst =
  match Bos.OS.Dir.with_current dst (fun () -> clone_repo ~repo) () with
  | Ok _ ->
      Logs.info (fun m -> m "Cloned repo %s to %s." repo (Fpath.to_string dst))
  | _ ->
      Logs.warn (fun m ->
          m "Failed to clone repo %s to %s." repo (Fpath.to_string dst))

let get_default_branch () =
  let cmd =
    Bos.Cmd.(v "git" % "symbolic-ref" % "refs/remotes/origin/HEAD" % "--short")
  in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok s -> s
  | _ ->
      Logs.warn (fun m -> m "Failed to get default branch");
      "origin/main"

let get_default_branch_in ~dst =
  let res = ref "origin/main" in
  match
    Bos.OS.Dir.with_current dst (fun () -> res := get_default_branch ()) ()
  with
  | Ok _ -> !res
  | _ ->
      Logs.warn (fun m ->
          m "Failed to get default branch in %s, defaulting to %s"
            (Fpath.to_string dst) !res);
      !res

let add_all_to_git () =
  let cmd = Bos.Cmd.(v "git" % "add" % ".") in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> ()
  | _ -> Error.abort "Failed to add files to git"

let get_new_branch () =
  let version = "v1" in
  Printf.sprintf "semgrep/install-ci-%s" version

let git_push () =
  let branch = get_new_branch () in
  let cmd = Bos.Cmd.(v "git" % "push" % "--set-upstream" % "origin" % branch) in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> ()
  | _ ->
      Logs.warn (fun m -> m "Failed to push to branch %s" branch);
      Error.abort
        (Printf.sprintf "Failed to push to branch %s. Please push manually"
           branch)

let git_commit () =
  let cmd =
    Bos.Cmd.(
      v "git" % "commit" % "-m" % "Add semgrep workflow"
      % "--author=\"Semgrep CI Installer <support@semgrep.com>\"")
  in
  match Bos.OS.Cmd.run_out cmd |> Bos.OS.Cmd.to_string with
  | Ok _ -> ()
  | _ ->
      Logs.warn (fun m -> m "Failed to commit changes to current branch!");
      Error.abort "Failed to commit changes. Please commit manually"

let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o777

let write_workflow_file ~repo =
  let dir =
    match repo with
    | _ when Common2.dir_exists repo -> Fpath.v repo
    | _ ->
        let tmp_dir =
          Filename.concat
            (Filename.get_temp_dir_name ())
            (Printf.sprintf "semgrep_install_ci_%6X" (Random.int 0xFFFFFF))
        in
        mkdir tmp_dir;
        clone_repo_to ~repo ~dst:(Fpath.v tmp_dir);
        (* NOTE: when we clone we get a directory with the repo name.
           we need to strip the owner from the repo name if it is present
           and then join the tmp_dir with the repo name to get the full path
        *)
        let repo =
          match String.split_on_char '/' repo with
          | [ _; repo ] -> repo
          | _ -> repo
        in
        Fpath.v (Filename.concat tmp_dir repo)
  in
  let commit = get_default_branch_in ~dst:dir in
  match
    Bos.OS.Dir.with_current dir
      (fun () ->
        Git_wrapper.run_with_worktree ~commit
          ~branch:(Some (get_new_branch ()))
          (fun () ->
            let github_dir = ".github" in
            mkdir github_dir;
            let workflow_dir = Filename.concat github_dir "workflows" in
            mkdir workflow_dir;
            let file = Filename.concat workflow_dir "semgrep.yml" in
            let oc = open_out_bin file in
            output_string oc (sprint_workflow ());
            close_out oc;
            Logs.info (fun m -> m "Wrote semgrep workflow to %s" file);
            add_all_to_git ();
            git_commit ();
            git_push ();
            let cwd =
              Bos.OS.Dir.current () |> Rresult.R.get_ok |> Fpath.to_string
            in
            Logs.info (fun m -> m "Current dir: %s" cwd)))
      ()
  with
  | Ok _ -> ()
  | _ -> Logs.err (fun m -> m "Failed to write workflow file")

let add_semgrep_workflow ~repo =
  let added = test_semgrep_workflow_added ~repo in
  match added with
  | true -> Logs.info (fun m -> m "Semgrep workflow already present, skipping")
  | false ->
      Logs.info (fun m -> m "Preparing Semgrep workflow");
      write_workflow_file ~repo

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
          set_ssh_as_default ();
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
