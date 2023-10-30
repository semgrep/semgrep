open Common
module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmd = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata from the git repo, or from SEMGREP_XXX environment
 * variables if set.
 *
 * TODO? rename to Git_and_semgrep_metadata.ml ?
 *)

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

(* TODO: right now we have also to use CLI flags like "semgrep-repo-name"
 * otherwise cmdliner raise an exn about empty info.
 *)
let env : env Term.t =
  let semgrep_repo_name =
    let doc = "The name of the Git repository." in
    let env = Cmd.Env.info "SEMGREP_REPO_NAME" in
    Arg.(
      value & opt (some string) None & info [ "semgrep-repo-name" ] ~env ~doc)
  in
  let semgrep_repo_url =
    let doc = "The URL of the Git repository." in
    let env = Cmd.Env.info "SEMGREP_REPO_URL" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.uri) None
      & info [ "semgrep-repo-url" ] ~env ~doc)
  in
  let semgrep_commit =
    let doc = "The commit of the Git repository." in
    let env = Cmd.Env.info "SEMGREP_COMMIT" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.sha1) None
      & info [ "semgrep-commit" ] ~env ~doc)
  in
  let semgrep_job_url =
    let doc = "The job URL." in
    let env = Cmd.Env.info "SEMGREP_JOB_URL" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.uri) None
      & info [ "semgrep-job-url" ] ~env ~doc)
  in
  let semgrep_pr_id =
    let doc = "The PR/MR ID." in
    let env = Cmd.Env.info "SEMGREP_PR_ID" in
    Arg.(value & opt (some string) None & info [ "semgrep-pr-id" ] ~env ~doc)
  in
  let semgrep_pr_title =
    let doc = "The PR/MR title." in
    let env = Cmd.Env.info "SEMGREP_PR_TITLE" in
    Arg.(value & opt (some string) None & info [ "semgrep-pr-title" ] ~env ~doc)
  in
  let semgrep_branch =
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
    const run $ semgrep_repo_name $ semgrep_repo_url $ semgrep_commit
    $ semgrep_job_url $ semgrep_pr_id $ semgrep_pr_title $ semgrep_branch)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta ~scan_environment ~(baseline_ref : Digestif.SHA1.t option) env =
  object (self)
    method project_metadata : Project_metadata.t =
      let commit_title : string =
        Git_wrapper.git_check_output
          Bos.Cmd.(v "git" % "show" % "-s" % "--format=%B")
      in
      let commit_author_email : Emile.mailbox =
        Git_wrapper.git_check_output
          Bos.Cmd.(v "git" % "show" % "-s" % "--format=%ae")
        |> Emile.of_string |> Result.get_ok
      in
      let commit_author_name : string =
        Git_wrapper.git_check_output
          Bos.Cmd.(v "git" % "show" % "-s" % "--format=%an")
      in
      (* Returns epoch time as str of head commit *)
      let commit_datetime : string =
        Git_wrapper.git_check_output
          Bos.Cmd.(v "git" % "show" % "-s" % "--format=%ct")
      in
      {
        semgrep_version = Version.version;
        (* REQUIRED for semgrep backed *)
        repository = self#repo_name;
        (* OPTIONAL for semgrep backed *)
        repo_url = self#repo_url;
        branch = self#branch;
        ci_job_url = self#ci_job_url;
        commit = self#commit_sha;
        commit_author_email = Some (Emile.to_string commit_author_email);
        commit_author_name = Some commit_author_name;
        commit_author_username = None;
        commit_author_image_url = None;
        commit_title = Some commit_title;
        commit_timestamp =
          (let time = float_of_string commit_datetime in
           let tm : Unix.tm = Unix.gmtime time in
           Some (ATDStringWrap.Datetime.unwrap tm));
        on = self#event_name;
        pull_request_author_username = None;
        pull_request_author_image_url = None;
        pull_request_id = self#pr_id;
        pull_request_title = self#pr_title;
        scan_environment;
        is_full_scan = self#is_full_scan;
        repo_id = None;
        org_id = None;
        (* TODO ugly: gitlab stuff, should maybe split
         * semgrep_output_v1.metadata and use inherit
         *)
        base_sha = None;
        start_sha = None;
        is_sca_scan = None;
        is_code_scan = None;
        is_secrets_scan = None;
      }

    (* to be overriden in children *)
    method repo_name =
      match env._SEMGREP_REPO_NAME with
      | Some repo_name -> repo_name
      | None ->
          let str =
            Git_wrapper.git_check_output
              Bos.Cmd.(v "git" % "rev-parse" % "--show-toplevel")
          in
          Fpath.basename (Fpath.v str)

    method repo_url =
      match env._SEMGREP_REPO_URL with
      | Some repo_url -> Some repo_url
      | None -> (
          let cmd = Bos.Cmd.(v "git" % "remote" % "get-url" % "origin") in
          let out = Bos.OS.Cmd.run_out cmd in
          match Bos.OS.Cmd.out_string ~trim:true out with
          | Ok (str, _status) ->
              Project_metadata.get_url_from_sstp_url (Some str)
          | Error (`Msg _err) ->
              Logs.warn (fun m ->
                  m
                    "Unable to infer repo_url. Set SEMGREP_REPO_URL \
                     environment variable or run in a valid git project with \
                     remote origin defined.");
              None)

    method branch =
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

    method ci_job_url = env._SEMGREP_JOB_URL

    method commit_sha =
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

    method event_name =
      match self#pr_id with
      | Some _ -> "pull_request"
      | None -> "unknown"

    method pr_id = env._SEMGREP_PR_ID
    method pr_title = env._SEMGREP_PR_TITLE
    method is_full_scan = self#merge_base_ref =*= None

    (* TODO? get rid of? use directly baseline_ref in is_full_scan? *)
    method merge_base_ref = baseline_ref
  end
