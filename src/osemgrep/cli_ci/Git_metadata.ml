open Common
module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module XCmd = Cmdliner.Cmd

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
  _SEMGREP_REPO_DISPLAY_NAME : string option;
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
    let env = XCmd.Env.info "SEMGREP_REPO_NAME" in
    Arg.(
      value & opt (some string) None & info [ "semgrep-repo-name" ] ~env ~doc)
  in
  let semgrep_repo_display_name =
    let doc =
      "The name the repository should be displayed as for this scan. Setting \
       it allows users to scan individual repos in one monorepo separately."
    in
    let env = XCmd.Env.info "SEMGREP_REPO_DISPLAY_NAME" in
    Arg.(
      value
      & opt (some string) None
      & info [ "semgrep-repo-display-name" ] ~env ~doc)
  in
  let semgrep_repo_url =
    let doc = "The URL of the Git repository." in
    let env = XCmd.Env.info "SEMGREP_REPO_URL" in
    Arg.(
      value
      & opt (some Cmdliner_.uri) None
      & info [ "semgrep-repo-url" ] ~env ~doc)
  in
  let semgrep_commit =
    let doc = "The commit of the Git repository." in
    let env = XCmd.Env.info "SEMGREP_COMMIT" in
    Arg.(
      value
      & opt (some Cmdliner_.sha1) None
      & info [ "semgrep-commit" ] ~env ~doc)
  in
  let semgrep_job_url =
    let doc = "The job URL." in
    let env = XCmd.Env.info "SEMGREP_JOB_URL" in
    Arg.(
      value
      & opt (some Cmdliner_.uri) None
      & info [ "semgrep-job-url" ] ~env ~doc)
  in
  let semgrep_pr_id =
    let doc = "The PR/MR ID." in
    let env = XCmd.Env.info "SEMGREP_PR_ID" in
    Arg.(value & opt (some string) None & info [ "semgrep-pr-id" ] ~env ~doc)
  in
  let semgrep_pr_title =
    let doc = "The PR/MR title." in
    let env = XCmd.Env.info "SEMGREP_PR_TITLE" in
    Arg.(value & opt (some string) None & info [ "semgrep-pr-title" ] ~env ~doc)
  in
  let semgrep_branch =
    let doc = "The Git branch." in
    let env = XCmd.Env.info "SEMGREP_BRANCH" in
    Arg.(value & opt (some string) None & info [ "semgrep-branch" ] ~env ~doc)
  in
  let run _SEMGREP_REPO_NAME _SEMGREP_REPO_DISPLAY_NAME _SEMGREP_REPO_URL
      _SEMGREP_COMMIT _SEMGREP_JOB_URL _SEMGREP_PR_ID _SEMGREP_PR_TITLE
      _SEMGREP_BRANCH =
    {
      _SEMGREP_REPO_NAME;
      _SEMGREP_REPO_DISPLAY_NAME;
      _SEMGREP_REPO_URL;
      _SEMGREP_COMMIT;
      _SEMGREP_JOB_URL;
      _SEMGREP_PR_ID;
      _SEMGREP_PR_TITLE;
      _SEMGREP_BRANCH;
    }
  in
  Term.(
    const run $ semgrep_repo_name $ semgrep_repo_display_name $ semgrep_repo_url
    $ semgrep_commit $ semgrep_job_url $ semgrep_pr_id $ semgrep_pr_title
    $ semgrep_branch)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta (caps : < Cap.exec >) ~scan_environment
  ~(baseline_ref : Digestif.SHA1.t option) env =
  object (self)
    method project_metadata : Project_metadata.t =
      let commit_title : string =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%B" ]
      in
      let commit_author_email_str : string =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%ae" ]
      in
      (* old: |> Emile.of_string |> Result.get_ok
       * but github generates emails like
       * 106279034+semgrep-ci[bot]@users.noreply.github.com
       * which are not valid for Emile so simpler to use
       * the raw string as in pysemgrep
       *)
      let commit_author_name : string =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%an" ]
      in
      (* Returns strict ISO 8601 time as str of head commit *)
      let commit_timestamp : Timedesc.Timestamp.t =
        Git_wrapper.command caps [ "show"; "-s"; "--format=%cI" ]
        |> Timedesc.Timestamp.of_iso8601 |> Result.get_ok
      in
      {
        (* REQUIRED for semgrep backend *)
        repository = self#repo_name;
        (* OPTIONAL for semgrep backend *)
        repo_url = self#repo_url;
        repo_display_name = Some self#repo_display_name;
        branch = self#branch;
        ci_job_url = self#ci_job_url;
        commit = self#commit_sha;
        commit_author_email = Some commit_author_email_str;
        commit_author_name = Some commit_author_name;
        commit_author_username = None;
        commit_author_image_url = None;
        commit_title = Some commit_title;
        commit_timestamp = Some commit_timestamp;
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
            Git_wrapper.command caps [ "rev-parse"; "--show-toplevel" ]
          in
          Printf.sprintf "local_scan/%s" (Fpath.basename (Fpath.v str))

    method repo_display_name =
      match env._SEMGREP_REPO_DISPLAY_NAME with
      | Some repo_display_name -> repo_display_name
      | None -> self#repo_name

    method repo_url =
      match env._SEMGREP_REPO_URL with
      | Some repo_url -> Some repo_url
      | None -> (
          let cmd = (Cmd.Name "git", [ "remote"; "get-url"; "origin" ]) in
          match CapExec.string_of_run caps#exec ~trim:true cmd with
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
          let cmd = (Cmd.Name "git", [ "rev-parse"; "--abbrev-ref"; "HEAD" ]) in
          match CapExec.string_of_run caps#exec ~trim:true cmd with
          | Ok (branch, (_, `Exited 0)) -> Some branch
          | Ok _
          | Error (`Msg _) ->
              None)

    method ci_job_url = env._SEMGREP_JOB_URL

    method commit_sha =
      match env._SEMGREP_COMMIT with
      | Some sha1 -> Some sha1
      | None -> (
          let cmd = (Cmd.Name "git", [ "rev-parse"; "HEAD" ]) in
          match CapExec.string_of_run caps#exec ~trim:true cmd with
          | Ok (str, (_, `Exited 0)) -> Digestif.SHA1.of_hex_opt str
          | Ok _
          | Error (`Msg _) ->
              None)

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
