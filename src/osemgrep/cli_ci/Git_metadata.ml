module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env = {
  _SEMGREP_REPO_NAME : string option;
  _SEMGREP_REPO_URL : Uri.t option;
  _SEMGREP_COMMIT : Digestif.SHA1.t option;
  _SEMGREP_JOB_URL : Uri.t option;
  _SEMGREP_PR_ID : string option;
  _SEMGREP_PR_TITLE : string option;
  _SEMGREP_BRANCH : string option;
}

(*****************************************************************************)
(* Cmdliner *)
(*****************************************************************************)

(* TODO? why using cmdliner to parse environment variables?
 * not simpler to just access them? We do not support
 * those --semgrep-repo-name CLI flags, so not much sense to use
 * Cmdliner then.
 *)
let env : env Term.t =
  let _semgrep_repo_name =
    let doc = "The name of the Git repository." in
    let env = Cmd.Env.info "SEMGREP_REPO_NAME" in
    Arg.(
      value & opt (some string) None & info [ "semgrep-repo-name" ] ~env ~doc)
  in
  let _semgrep_repo_url =
    let doc = "The URL of the Git repository." in
    let env = Cmd.Env.info "SEMGREP_REPO_URL" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.uri) None
      & info [ "semgrep-repo-url" ] ~env ~doc)
  in
  let _semgrep_commit =
    let doc = "The commit of the Git repository." in
    let env = Cmd.Env.info "SEMGREP_COMMIT" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.sha1) None
      & info [ "semgrep-commit" ] ~env ~doc)
  in
  let _semgrep_job_url =
    let doc = "The job URL." in
    let env = Cmd.Env.info "SEMGREP_JOB_URL" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.uri) None
      & info [ "semgrep-job-url" ] ~env ~doc)
  in
  let _semgrep_pr_id =
    let doc = "The PR/MR ID." in
    let env = Cmd.Env.info "SEMGREP_PR_ID" in
    Arg.(value & opt (some string) None & info [ "semgrep-pr-id" ] ~env ~doc)
  in
  let _semgrep_pr_title =
    let doc = "The PR/MR title." in
    let env = Cmd.Env.info "SEMGREP_PR_TITLE" in
    Arg.(value & opt (some string) None & info [ "semgrep-pr-title" ] ~env ~doc)
  in
  let _semgrep_branch =
    let doc = "The Git branch." in
    let env = Cmd.Env.info "SEMGREP_BRANCH" in
    Arg.(value & opt (some string) None & info [ "semgrep-branch" ] ~env ~doc)
  in
  let run _SEMGREP_REPO_NAME _SEMGREP_REPO_URL _SEMGREP_COMMIT _SEMGREP_JOB_URL
      _SEMGREP_PR_ID _SEMGREP_PR_TITLE _SEMGREP_BRANCH =
    {
      _SEMGREP_REPO_NAME;
      _SEMGREP_REPO_URL;
      _SEMGREP_COMMIT;
      _SEMGREP_JOB_URL;
      _SEMGREP_PR_ID;
      _SEMGREP_PR_TITLE;
      _SEMGREP_BRANCH;
    }
  in
  Term.(
    const run $ _semgrep_repo_name $ _semgrep_repo_url $ _semgrep_commit
    $ _semgrep_job_url $ _semgrep_pr_id $ _semgrep_pr_title $ _semgrep_branch)

(*****************************************************************************)
(* Implement signature in Project_metadata.S *)
(*****************************************************************************)

(* TODO: this is not even used ... rewrite the code to maybe use
 * classes instead of modules, to better mimic meta.py
 *)
let get_event_name env =
  match env._SEMGREP_PR_ID with
  | Some _ -> Some "pull_request"
  | None -> None

let get_repo_name env =
  match env._SEMGREP_REPO_NAME with
  | Some repo_name -> repo_name
  | None ->
      let str =
        Project_metadata.git_check_output
          Bos.Cmd.(v "git" % "rev-parse" % "--show-toplevel")
      in
      Fpath.basename (Fpath.v str)

let get_repo_url env =
  match env._SEMGREP_REPO_URL with
  | Some repo_url -> Some repo_url
  | None -> (
      let cmd = Bos.Cmd.(v "git" % "remote" % "get-url" % "origin") in
      let out = Bos.OS.Cmd.run_out cmd in
      match Bos.OS.Cmd.out_string ~trim:true out with
      | Ok (str, _status) -> Project_metadata.get_url_from_sstp_url (Some str)
      | Error (`Msg _err) ->
          Logs.warn (fun m ->
              m
                "Unable to infer repo_url. Set SEMGREP_REPO_URL environment \
                 variable or run in a valid git project with remote origin \
                 defined.");
          None)

let get_commit_sha env =
  match env._SEMGREP_COMMIT with
  | Some sha1 -> Some sha1
  | None -> (
      let cmd = Bos.Cmd.(v "git" % "rev-parse" % "HEAD") in
      let out = Bos.OS.Cmd.run_out cmd in
      let out =
        Result.bind (Bos.OS.Cmd.out_string ~trim:true out) @@ function
        | str, (_, `Exited 0) -> Ok (Digestif.SHA1.of_hex_opt str)
        | __else__ -> Error (`Msg "Invalid status")
      in
      match out with
      | Ok value -> value
      | Error (`Msg _msg) -> None)

let get_ci_job_url env = env._SEMGREP_JOB_URL
let get_pr_id env = env._SEMGREP_PR_ID
let get_pr_title env = env._SEMGREP_PR_TITLE
let get_merge_base_ref _ = Lwt.return_none

let get_branch env =
  match env._SEMGREP_BRANCH with
  | Some branch -> Some branch
  | None -> (
      let cmd = Bos.Cmd.(v "git" % "rev-parse" % "--abbrev-ref" % "HEAD") in
      let out = Bos.OS.Cmd.run_out cmd in
      match Bos.OS.Cmd.out_string ~trim:true out with
      | Ok (branch, (_, `Exited 0)) -> Some branch
      | Ok _
      | Error (`Msg _) ->
          None)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let make (env : env) : Project_metadata.t =
  let commit_title =
    Project_metadata.git_check_output
      Bos.Cmd.(v "git" % "show" % "-s" % "--format=%B")
  in
  let commit_author_email =
    Project_metadata.git_check_output
      Bos.Cmd.(v "git" % "show" % "-s" % "--format=%ae")
    |> Emile.of_string |> Result.get_ok
  in
  let commit_author_name =
    Project_metadata.git_check_output
      Bos.Cmd.(v "git" % "show" % "-s" % "--format=%an")
  in
  let on =
    if Option.is_some env._SEMGREP_PR_ID then "pull_request" else "unknown"
  in
  {
    semgrep_version = Version.version;
    repository = get_repo_name env;
    repo_url = get_repo_url env |> Option.map Uri.to_string;
    branch = get_branch env;
    ci_job_url = get_ci_job_url env |> Option.map Uri.to_string;
    commit = get_commit_sha env |> Option.map Digestif.SHA1.to_raw_string;
    commit_author_email = Some (Emile.to_string commit_author_email);
    commit_author_name = Some commit_author_name;
    commit_author_username = None;
    commit_author_image_url = None;
    commit_title = Some commit_title;
    on;
    pull_request_author_username = None;
    pull_request_author_image_url = None;
    pull_request_id = env._SEMGREP_PR_ID;
    pull_request_title = env._SEMGREP_PR_TITLE;
    scan_environment = "git";
    is_full_scan = true;
    (* TODO, not ported? *)
    commit_timestamp = None;
    (* TODO ugly: gitlab stuff, should maybe split semgrep_output_v1.metadata
     * and use inherit *)
    base_sha = None;
    start_sha = None;
    is_sca_scan = None;
    is_code_scan = None;
    is_secrets_scan = None;
  }

let term = Cmdliner.Term.(const make $ env)
