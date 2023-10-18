(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type env = {
  git : Git_metadata.env;
  _GITHUB_EVENT_JSON : Yojson.Basic.t;
  _GITHUB_REPOSITORY : string option;
  _GITHUB_API_URL : Uri.t option;
  _GITHUB_SHA : Digestif.SHA1.t option;
  _GITHUB_SERVER_URL : Uri.t;
  _GITHUB_REF : string option;
  _GITHUB_HEAD_REF : string option;
  _GITHUB_RUN_ID : string option;
  _GITHUB_EVENT_NAME : string option;
  _GH_TOKEN : string option;
}

let _MAX_FETCH_ATTEMPT_COUNT = 10
(* A limit of how many fetch we should do until we find the common commit
   between two branches. *)

let scan_environment = "github-actions"

(*****************************************************************************)
(* Implement signature in Project_metadata.S *)
(*****************************************************************************)

let get_repo_name env =
  let err = "Could not get repo_name when running in GitHub Action" in
  if Option.is_none env.git.Git_metadata._SEMGREP_REPO_NAME then
    match env._GITHUB_REPOSITORY with
    | Some repo_name -> repo_name
    | None -> failwith err
  else failwith err

let get_repo_url env =
  match (env.git.Git_metadata._SEMGREP_REPO_URL, get_repo_name env) with
  | (Some _ as v), _ -> v
  | None, repo_name -> Some (Uri.with_path env._GITHUB_SERVER_URL repo_name)

let is_pull_request_event env =
  match Git_metadata.get_event_name env with
  | Some ("pull_request" | "pull_request_target") -> true
  | _ -> false

let get_commit_sha env =
  if is_pull_request_event env.git then
    Option.bind
      (Glom.get_and_coerce_opt Glom.string env._GITHUB_EVENT_JSON
         Glom.[ k "pull_request"; k "head"; k "sha" ])
      Digestif.SHA1.of_hex_opt
  else if Git_metadata.get_event_name env.git = Some "push" then env._GITHUB_SHA
  else env.git._SEMGREP_COMMIT

(* This branch name gets used for tracking issue state over time on the
   backend. The head ref is in GITHUB_HEAD_REF and the base ref is in
   GITHUB_REF.

   Event name            GITHUB_HEAD_REF -> GITHUB_REF
   ---------------------------------------------------
   pull_request        - johnny-path-1   -> refs/pulls/123/merge
   pull_request_target - johnny-path-1   -> refs/heads/main
   push/schedule/etc.  - <unset>         -> refs/heads/main

   This code originally always sent GITHUB_REF. This caused obvious breakage
   for pull_request_target, so we just fixed the ref we report for that event.
   But it's more subtly wrong for pull_request events: what we'e scanning
   there is still the head ref; we force-switch to the head ref in
   `fix_head_if_github_action`. But fixing the slight data inaccuracy would be
   incompatible with all existing data. So as of May 2022 we have not
   corrected it. *)
let get_branch env =
  if Git_metadata.get_event_name env.git = Some "pull_request_target" then
    env._GITHUB_HEAD_REF
  else
    match (env.git._SEMGREP_BRANCH, env._GITHUB_REF) with
    | Some branch, _ -> Some branch
    | None, Some branch -> Some branch
    | None, None -> Git_metadata.get_branch env.git

let get_pr_id env =
  match env.git._SEMGREP_PR_ID with
  | Some _ as value -> value
  | None ->
      Glom.(
        get_and_coerce_opt int env._GITHUB_EVENT_JSON
          [ k "pull_request"; k "number" ])
      |> Option.map string_of_int

let get_pr_title env =
  match env.git._SEMGREP_PR_TITLE with
  | Some _ as value -> value
  | None ->
      Glom.(
        get_and_coerce_opt string env._GITHUB_EVENT_JSON
          [ k "pull_request"; k "title" ])

let get_ci_job_url env =
  match Git_metadata.get_ci_job_url env.git with
  | Some _ as value -> value
  | None -> (
      match (Git_metadata.get_repo_url env.git, env._GITHUB_RUN_ID) with
      | Some repo_url, Some value ->
          Some (Uri.with_path repo_url (Fmt.str "/actions/runs/%s" value))
      | _ -> None)

let get_event_name env = env._GITHUB_EVENT_NAME

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Split out shallow fetch so we can mock it away in tests. *)
let _shallow_fetch_branch branch_name =
  let _ =
    Git_wrapper.git_check_output
      Bos.Cmd.(
        v "git" % "fetch" % "origin" % "--depth=1" % "--force"
        % "--update-head-ok"
        % Fmt.str "%s:%s" branch_name branch_name)
  in
  ()

(* Split out shallow fetch so we can mock it away in tests.

   Different from _shallow_fetch_branch because it does not assign a local
   name to the commit. It just does the fetch. *)
let _shallow_fetch_commit commit_hash =
  let _ =
    Git_wrapper.git_check_output
      Bos.Cmd.(
        v "git" % "fetch" % "origin" % "--depth=1" % "--force"
        % "--update-head-ok"
        % Digestif.SHA1.to_hex commit_hash)
  in
  ()

(* Return sha hash of latest commit in a given branch.

   Does a git fetch of given branch with depth = 1. *)
let _get_latest_commit_hash_in_branch branch_name =
  _shallow_fetch_branch branch_name;
  Git_wrapper.git_check_output Bos.Cmd.(v "git" % "rev-parse" % branch_name)
  |> Digestif.SHA1.of_hex_opt |> Option.get

(* Ref name of the branch pull request if from. *)
let _get_head_branch_ref env =
  Glom.(
    get_and_coerce_opt string env._GITHUB_EVENT_JSON
      [ k "pull_request"; k "head"; k "ref" ])

(* Commit of the head branch, reported via the GitHub pull_request event.
   This will also ensure that a fetch is done prior to returning.

   Assumes we are in PR context. *)
let get_head_branch_hash env =
  let commit =
    Glom.(
      get_and_coerce_opt string env._GITHUB_EVENT_JSON
        [ k "pull_request"; k "head"; k "sha" ])
  in
  let commit = Option.bind commit Digestif.SHA1.of_hex_opt in
  match (_get_head_branch_ref env, commit) with
  | Some head_branch_name, Some commit ->
      Logs.debug (fun m ->
          m "head branch %s has latest commit %a, fetching that commit now."
            head_branch_name Digestif.SHA1.pp commit);
      let _ =
        Git_wrapper.git_check_output
          Bos.Cmd.(
            v "git" % "fetch" % "origin" % "--force" % "--depth=1"
            % Digestif.SHA1.to_hex commit)
      in
      Some commit
  | _ -> None

let _get_base_branch_ref env =
  Glom.(
    get_and_coerce_opt string env._GITHUB_EVENT_JSON
      [ k "pull_request"; k "base"; k "ref" ])

(* Latest commit hash of the base branch of PR is being merged to.

   Assumes we are in PR context. *)
let get_base_branch_hash env =
  let commit =
    Option.map _get_latest_commit_hash_in_branch (_get_base_branch_ref env)
  in
  match (_get_base_branch_ref env, commit) with
  | Some base_branch_name, Some commit ->
      Logs.debug (fun m ->
          m "base branch (%s) has latest commit %a" base_branch_name
            Digestif.SHA1.pp commit);
      commit
  | _ ->
      invalid_arg
        "We are not into a PR context (the GitHub pull_request event is \
         missing)"

let _find_branchoff_point_from_github_api env =
  let base_branch_hash = get_base_branch_hash env
  and head_branch_hash = get_head_branch_hash env
  and repo_name = get_repo_name env in

  match (env._GH_TOKEN, env._GITHUB_API_URL, head_branch_hash) with
  | Some gh_token, Some api_url, Some head_branch_hash -> (
      let headers = [ ("Authorization", Fmt.str "Bearer %s" gh_token) ] in
      let open Lwt.Infix in
      Http_helpers.get_async ~headers
        (Uri.of_string
           (Fmt.str "%a/repos/%s/compare/%a...%a" Uri.pp api_url repo_name
              Digestif.SHA1.pp base_branch_hash Digestif.SHA1.pp
              head_branch_hash))
      >>= function
      | Ok body ->
          let body = body |> Yojson.Basic.from_string in
          let commit =
            Option.bind
              Glom.(
                get_and_coerce_opt string body
                  [ k "merge_base_commit"; k "sha" ])
              Digestif.SHA1.of_hex_opt
          in
          Option.iter _shallow_fetch_commit commit;
          Lwt.return commit
      | __else__ -> Lwt.return_none)
  | __else__ -> Lwt.return_none

let rec _find_branchoff_point ?(attempt_count = 0) env =
  let base_branch_hash = get_base_branch_hash env
  and head_branch_hash = Option.get (get_head_branch_hash env) in

  let base_branch_name = Option.get (_get_base_branch_ref env)
  and head_branch_name = Option.get (_get_head_branch_ref env) in

  let fetch_depth = 4. ** Float.of_int attempt_count |> Float.to_int in
  let fetch_depth = fetch_depth + !Semgrep_envvars.v.min_fetch_depth in
  let fetch_depth =
    if attempt_count > _MAX_FETCH_ATTEMPT_COUNT then
      Float.to_int (2. ** 31.) - 1
    else fetch_depth
  in
  if attempt_count = 0 then _find_branchoff_point_from_github_api env
  else
    (* XXX(dinosaure): we safely can use [Option.get]. This information is
       required to [get_base_branch_ref]. *)
    let _ =
      Git_wrapper.git_check_output
        Bos.Cmd.(
          v "git" % "fetch" % "origin" % "--force" % "--update-head-ok"
          % "--depth" % string_of_int fetch_depth
          % Fmt.str "%s:%s" base_branch_name base_branch_name)
    in
    let _ =
      Git_wrapper.git_check_output
        Bos.Cmd.(
          v "git" % "fetch" % "origin" % "--force" % "--update-head-ok"
          % "--depth" % string_of_int fetch_depth
          % Digestif.SHA1.to_hex head_branch_hash)
    in

    let cmd =
      Bos.Cmd.(
        v "git" % "merge-base"
        % Digestif.SHA1.to_hex base_branch_hash
        % Digestif.SHA1.to_hex head_branch_hash)
    in
    let out = Bos.OS.Cmd.run_out cmd in
    match Bos.OS.Cmd.out_string ~trim:true out with
    | Ok (merge_base, (_, `Exited 0)) ->
        Lwt.return (Digestif.SHA1.of_hex_opt merge_base)
    | Ok (_, _) when attempt_count < _MAX_FETCH_ATTEMPT_COUNT ->
        _find_branchoff_point ~attempt_count:(succ attempt_count) env
    | Ok (_, _) ->
        Fmt.failwith
          "Could not find branch-off point between the baseline tip %s@%a and \
           current head %s@%a"
          base_branch_name Digestif.SHA1.pp base_branch_hash head_branch_name
          Digestif.SHA1.pp head_branch_hash
    | Error (`Msg err) -> failwith err

let get_merge_base_ref env =
  match (is_pull_request_event env.git, get_head_branch_hash env) with
  | true, Some _ -> _find_branchoff_point_from_github_api env
  | _ -> Lwt.return_none

let env =
  let open Cmdliner in
  let _github_event_path =
    let doc = "The GitHub event path." in
    let env = Cmd.Env.info "GITHUB_EVENT_PATH" in
    Arg.(
      value & opt Glom.cli Glom.default & info [ "github-event-path" ] ~env ~doc)
  in
  let _github_sha =
    let doc = "The GitHub commit." in
    let env = Cmd.Env.info "GITHUB_SHA" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.sha1) None
      & info [ "github-sha" ] ~env ~doc)
  in
  let gh_token =
    let doc = "The GitHub token." in
    let env = Cmd.Env.info "GH_TOKEN" in
    Arg.(value & opt (some string) None & info [ "gh-token" ] ~env ~doc)
  in
  let _github_repository =
    let doc = "The GitHub repository." in
    let env = Cmd.Env.info "GITHUB_REPOSITORY" in
    Arg.(
      value & opt (some string) None & info [ "github-repository" ] ~env ~doc)
  in
  let _github_server_url =
    let doc = "The GitHub server URL." in
    let env = Cmd.Env.info "GITHUB_SERVER_URL" in
    Arg.(
      value
      & opt Cmdliner_helpers.uri (Uri.of_string "https://github.com")
      & info [ "github-server-url" ] ~doc ~env)
  in
  let _github_api_url =
    let doc = "The GitHub API URL." in
    let env = Cmd.Env.info "GITHUB_API_URL" in
    Arg.(
      value
      & opt (some Cmdliner_helpers.uri) None
      & info [ "github-api-url" ] ~doc ~env)
  in
  let _github_run_id =
    let doc = "The GitHub run ID." in
    let env = Cmd.Env.info "GITHUB_RUN_ID" in
    Arg.(value & opt (some string) None & info [ "github-run-id" ] ~doc ~env)
  in
  let _github_event_name =
    let doc = "The GitHub event name." in
    let env = Cmd.Env.info "GITHUB_EVENT_NAME" in
    Arg.(
      value & opt (some string) None & info [ "github-event-name" ] ~doc ~env)
  in
  let _github_ref =
    let doc = "The GitHub ref." in
    let env = Cmd.Env.info "GITHUB_REF" in
    Arg.(value & opt (some string) None & info [ "github-ref" ] ~doc ~env)
  in
  let _github_head_ref =
    let doc = "The GitHub HEAD ref." in
    let env = Cmd.Env.info "GITHUB_HEAD_REF" in
    Arg.(value & opt (some string) None & info [ "github-head-ref" ] ~doc ~env)
  in
  let run git (_, _GITHUB_EVENT_JSON) _GITHUB_SHA _GITHUB_REPOSITORY
      _GITHUB_SERVER_URL _GITHUB_API_URL _GITHUB_RUN_ID _GITHUB_EVENT_NAME
      _GITHUB_REF _GITHUB_HEAD_REF _GH_TOKEN =
    {
      git;
      _GITHUB_EVENT_JSON;
      _GITHUB_REPOSITORY;
      _GITHUB_API_URL;
      _GITHUB_SHA;
      _GITHUB_SERVER_URL;
      _GITHUB_RUN_ID;
      _GITHUB_EVENT_NAME;
      _GITHUB_REF;
      _GITHUB_HEAD_REF;
      _GH_TOKEN;
    }
  in
  Term.(
    const run $ Git_metadata.env $ _github_event_path $ _github_sha
    $ _github_repository $ _github_server_url $ _github_api_url $ _github_run_id
    $ _github_event_name $ _github_ref $ _github_head_ref $ gh_token)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let make env =
  let _SEMGREP_REPO_NAME = Some (get_repo_name env) in
  let _SEMGREP_REPO_URL = get_repo_url env in
  let commit_author_username =
    Glom.(
      get_and_coerce_opt string env._GITHUB_EVENT_JSON [ k "sender"; k "login" ])
  in
  let commit_author_image_url =
    Glom.(
      get_and_coerce_opt string env._GITHUB_EVENT_JSON
        [ k "sender"; k "avatar_url" ])
    |> Option.map Uri.of_string
  in
  let pull_request_author_username =
    Glom.(
      get_and_coerce_opt string env._GITHUB_EVENT_JSON
        [ k "pull_request"; k "user"; k "login" ])
  in
  let pull_request_author_image_url =
    Glom.(
      get_and_coerce_opt string env._GITHUB_EVENT_JSON
        [ k "pull_request"; k "user"; k "avatar_url" ])
    |> Option.map Uri.of_string
  in
  let value =
    (* XXX(dinosaure): like [**super.to_dict()] *)
    Git_metadata.make { env.git with _SEMGREP_REPO_NAME; _SEMGREP_REPO_URL }
  in
  {
    value with
    commit_author_username;
    commit_author_image_url;
    pull_request_author_username;
    pull_request_author_image_url;
    scan_environment;
  }

let term = Cmdliner.Term.(const make $ env)
