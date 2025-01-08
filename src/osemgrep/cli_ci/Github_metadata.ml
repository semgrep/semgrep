open Common
module Arg = Cmdliner.Arg
module Term = Cmdliner.Term
module Cmdl = Cmdliner.Cmd

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Extract metadata using environment variables setup by Github,
 * or default to Git_(and_semgrep_)metadata.ml if unset.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* See https://docs.github.com/en/actions/learn-github-actions/variables#default-environment-variables *)

(* TODO: don't start field names with an underscore because it prevents
   some warnings (e.g. unused variable) *)
type env = {
  (* actually GITHUB_EVENT_PATH *)
  _GITHUB_EVENT_JSON : Yojson.Basic.t;
  _GITHUB_REPOSITORY : string option;
  (* alt: could use 'int option' for both _ID fields *)
  _GITHUB_REPOSITORY_ID : string option;
  _GITHUB_REPOSITORY_OWNER_ID : string option;
  _GITHUB_API_URL : Uri.t option;
  (* default to https://github.com if not set *)
  _GITHUB_SERVER_URL : Uri.t;
  _GITHUB_SHA : Digestif.SHA1.t option;
  _GITHUB_REF : string option;
  _GITHUB_HEAD_REF : string option;
  _GITHUB_RUN_ID : string option;
  _GITHUB_EVENT_NAME : string option;
  _GH_TOKEN : string option;
}

let _MAX_FETCH_ATTEMPT_COUNT = 10
(* A limit of how many fetch we should do until we find the common commit
   between two branches. *)

(*****************************************************************************)
(* Cmdliner *)
(*****************************************************************************)

let env : env Term.t =
  let github_event_path =
    let doc = "The GitHub event path." in
    let env = Cmdl.Env.info "GITHUB_EVENT_PATH" in
    Arg.(
      value & opt Glom.cli Glom.default & info [ "github-event-path" ] ~env ~doc)
  in
  let github_sha =
    let doc = "The GitHub commit." in
    let env = Cmdl.Env.info "GITHUB_SHA" in
    Arg.(
      value & opt (some Cmdliner_.sha1) None & info [ "github-sha" ] ~env ~doc)
  in
  let gh_token =
    let doc = "The GitHub token." in
    let env = Cmdl.Env.info "GH_TOKEN" in
    Arg.(value & opt (some string) None & info [ "gh-token" ] ~env ~doc)
  in
  let github_repository =
    let doc = "The GitHub repository." in
    let env = Cmdl.Env.info "GITHUB_REPOSITORY" in
    Arg.(
      value & opt (some string) None & info [ "github-repository" ] ~env ~doc)
  in
  let github_server_url =
    let doc = "The GitHub server URL." in
    let env = Cmdl.Env.info "GITHUB_SERVER_URL" in
    Arg.(
      value
      & opt Cmdliner_.uri (Uri.of_string "https://github.com")
      & info [ "github-server-url" ] ~doc ~env)
  in
  let github_api_url =
    let doc = "The GitHub API URL." in
    let env = Cmdl.Env.info "GITHUB_API_URL" in
    Arg.(
      value
      & opt (some Cmdliner_.uri) None
      & info [ "github-api-url" ] ~doc ~env)
  in
  let github_run_id =
    let doc = "The GitHub run ID." in
    let env = Cmdl.Env.info "GITHUB_RUN_ID" in
    Arg.(value & opt (some string) None & info [ "github-run-id" ] ~doc ~env)
  in
  let github_event_name =
    let doc = "The GitHub event name." in
    let env = Cmdl.Env.info "GITHUB_EVENT_NAME" in
    Arg.(
      value & opt (some string) None & info [ "github-event-name" ] ~doc ~env)
  in
  let github_ref =
    let doc = "The GitHub ref." in
    let env = Cmdl.Env.info "GITHUB_REF" in
    Arg.(value & opt (some string) None & info [ "github-ref" ] ~doc ~env)
  in
  let github_head_ref =
    let doc = "The GitHub HEAD ref." in
    let env = Cmdl.Env.info "GITHUB_HEAD_REF" in
    Arg.(value & opt (some string) None & info [ "github-head-ref" ] ~doc ~env)
  in
  let github_repository_id =
    let doc = "The ID of the repository." in
    let env = Cmdl.Env.info "GITHUB_REPOSITORY_ID" in
    Arg.(
      value & opt (some string) None & info [ "github-repository-id" ] ~doc ~env)
  in
  let github_repository_owner_id =
    let doc = "The repository owner's account ID." in
    let env = Cmdl.Env.info "GITHUB_REPOSITORY_OWNER_ID" in
    Arg.(
      value
      & opt (some string) None
      & info [ "github-repository-owner-id" ] ~doc ~env)
  in
  let run (_, _GITHUB_EVENT_JSON) _GITHUB_SHA _GITHUB_REPOSITORY
      _GITHUB_SERVER_URL _GITHUB_API_URL _GITHUB_RUN_ID _GITHUB_EVENT_NAME
      _GITHUB_REF _GITHUB_HEAD_REF _GH_TOKEN _GITHUB_REPOSITORY_ID
      _GITHUB_REPOSITORY_OWNER_ID =
    {
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
      _GITHUB_REPOSITORY_ID;
      _GITHUB_REPOSITORY_OWNER_ID;
    }
  in
  Term.(
    const run $ github_event_path $ github_sha $ github_repository
    $ github_server_url $ github_api_url $ github_run_id $ github_event_name
    $ github_ref $ github_head_ref $ gh_token $ github_repository_id
    $ github_repository_owner_id)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Split out shallow fetch so we can mock it away in tests. *)
let shallow_fetch_branch (caps : < Cap.exec >) branch_name =
  let _ =
    Git_wrapper.command caps
      [
        "fetch";
        "origin";
        "--depth=1";
        "--force";
        "--update-head-ok";
        spf "%s:%s" branch_name branch_name;
      ]
  in
  ()

(* Split out shallow fetch so we can mock it away in tests.

   Different from _shallow_fetch_branch because it does not assign a local
   name to the commit. It just does the fetch. *)
let _shallow_fetch_commit (caps : < Cap.exec >) commit_hash =
  let _ =
    Git_wrapper.command caps
      [
        "fetch";
        "origin";
        "--depth=1";
        "--force";
        "--update-head-ok";
        Digestif.SHA1.to_hex commit_hash;
      ]
  in
  ()

(* Return sha hash of latest commit in a given branch.

   Does a git fetch of given branch with depth = 1. *)
let get_latest_commit_hash_in_branch (caps : < Cap.exec >) branch_name =
  shallow_fetch_branch caps branch_name;
  Git_wrapper.command caps [ "rev-parse"; branch_name ]
  |> Digestif.SHA1.of_hex_opt |> Option.get

(* Ref name of the branch pull request if from. *)
let get_head_branch_ref env =
  Glom.(
    get_and_coerce_opt string env._GITHUB_EVENT_JSON
      [ k "pull_request"; k "head"; k "ref" ])

(* Commit of the head branch, reported via the GitHub pull_request event.
   This will also ensure that a fetch is done prior to returning.

   Assumes we are in PR context. *)
let get_head_branch_hash (caps : < Cap.exec >) (env : env) :
    Digestif.SHA1.t option =
  let commit =
    Glom.(
      get_and_coerce_opt string env._GITHUB_EVENT_JSON
        [ k "pull_request"; k "head"; k "sha" ])
  in
  let commit = Option.bind commit Digestif.SHA1.of_hex_opt in
  match (get_head_branch_ref env, commit) with
  | Some head_branch_name, Some commit ->
      Logs.debug (fun m ->
          m "head branch %s has latest commit %a, fetching that commit now."
            head_branch_name Digestif.SHA1.pp commit);
      let _ =
        Git_wrapper.command caps
          [
            "fetch";
            "origin";
            "--force";
            "--depth=1";
            Digestif.SHA1.to_hex commit;
          ]
      in
      Some commit
  | _ -> None

let get_base_branch_ref (env : env) : string option =
  Glom.(
    get_and_coerce_opt string env._GITHUB_EVENT_JSON
      [ k "pull_request"; k "base"; k "ref" ])

(* Latest commit hash of the base branch of PR is being merged to.

   Assumes we are in PR context. *)
let get_base_branch_hash (caps : < Cap.exec >) (env : env) =
  let commit =
    Option.map (get_latest_commit_hash_in_branch caps) (get_base_branch_ref env)
  in
  match (get_base_branch_ref env, commit) with
  | Some base_branch_name, Some commit ->
      Logs.debug (fun m ->
          m "base branch (%s) has latest commit %a" base_branch_name
            Digestif.SHA1.pp commit);
      commit
  | _ ->
      invalid_arg
        "We are not into a PR context (the GitHub pull_request event is \
         missing)"

(* from meta.py:
   "By default, the GitHub Actions checkout action gives you a shallow
   clone of the repository. In order to get the merge base, we need to
   fetch the history of the head branch and the base branch, all the way
   back to the point where the head branch diverged. In a large
   repository, this can be a lot of commits, and this fetching can
   dramatically impact performance.
     To avoid this, on the first attempt to find the merge base, we try to
   use the GitHub REST API instead of fetching enough history to compute
   it locally. We only do this if the `GH_TOKEN` environment variable is
   provided. GitHub Actions provides that token to workflows, but the
   workflow needs to explicitly make it available to Semgrep via an
   environment variable like this:
     env:
     GH_TOKEN: ${{ github.token }}
     This will allow Semgrep to make this API request even for private
   repositories."
*)
let find_branchoff_point_from_github_api (caps : < Cap.network ; Cap.exec >)
    repo_name env : Digestif.SHA1.t option Lwt.t =
  let base_branch_hash = get_base_branch_hash (caps :> < Cap.exec >) env in
  let head_branch_hash = get_head_branch_hash (caps :> < Cap.exec >) env in

  match (env._GH_TOKEN, env._GITHUB_API_URL, head_branch_hash) with
  | Some str_token, Some api_url, Some head_branch_hash ->
      let gh_token = Auth.unsafe_token_of_string str_token in
      Github_API.find_branchoff_point_async
        (caps :> < Cap.network >)
        ~gh_token ~api_url ~repo_name ~base_branch_hash head_branch_hash
  | __else__ -> Lwt.return_none

(* from meta.py:
   "GithubActions is a shallow clone and the "base" that github sends
   is not the merge base. We must fetch and get the merge-base ourselves"
*)
let rec find_branchoff_point (caps : < Cap.exec ; Cap.network >)
    ?(attempt_count = 0) repo_name env =
  let base_branch_hash = get_base_branch_hash (caps :> < Cap.exec >) env
  and head_branch_hash =
    Option.get (get_head_branch_hash (caps :> < Cap.exec >) env)
  in

  let base_branch_name = Option.get (get_base_branch_ref env)
  and head_branch_name = Option.get (get_head_branch_ref env) in

  let fetch_depth = 4. ** Float.of_int attempt_count |> Float.to_int in
  let fetch_depth = fetch_depth + !Semgrep_envvars.v.min_fetch_depth in
  let fetch_depth =
    if attempt_count > _MAX_FETCH_ATTEMPT_COUNT then
      Float.to_int (2. ** 31.) - 1
    else fetch_depth
  in
  if attempt_count =|= 0 then
    (* TODO let%lwt base = find_xxx in
     * Option.iter shallow_fetch_commit base;
     *)
    find_branchoff_point_from_github_api caps repo_name env
  else
    (* XXX(dinosaure): we safely can use [Option.get]. This information is
       required to [get_base_branch_ref]. *)
    let _ =
      Git_wrapper.command
        (caps :> < Cap.exec >)
        [
          "fetch";
          "origin";
          "--force";
          "--update-head-ok";
          "--depth";
          string_of_int fetch_depth;
          spf "%s:%s" base_branch_name base_branch_name;
        ]
    in
    let _ =
      Git_wrapper.command
        (caps :> < Cap.exec >)
        [
          "fetch";
          "origin";
          "--force";
          "--update-head-ok";
          "--depth";
          string_of_int fetch_depth;
          Digestif.SHA1.to_hex head_branch_hash;
        ]
    in

    let cmd =
      ( Cmd.Name "git",
        [
          "merge-base";
          Digestif.SHA1.to_hex base_branch_hash;
          Digestif.SHA1.to_hex head_branch_hash;
        ] )
    in
    match CapExec.string_of_run caps#exec ~trim:true cmd with
    | Ok (merge_base, (_, `Exited 0)) ->
        Lwt.return (Digestif.SHA1.of_hex_opt merge_base)
    | Ok (_, _) when attempt_count < _MAX_FETCH_ATTEMPT_COUNT ->
        find_branchoff_point caps ~attempt_count:(succ attempt_count) repo_name
          env
    | Ok (_, _) ->
        failwith
          (spf
             "Could not find branch-off point between the baseline tip %s@%s \
              and current head %s@%s"
             base_branch_name
             (Fmt_.to_show Digestif.SHA1.pp base_branch_hash)
             head_branch_name
             (Fmt_.to_show Digestif.SHA1.pp head_branch_hash))
    | Error (`Msg err) -> failwith err
[@@warning "-32"]
(* TODO: why unused? *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

class meta caps ~baseline_ref env gha_env =
  object (self)
    inherit
      Git_metadata.meta
        caps ~scan_environment:"github-actions" ~baseline_ref env as super

    method! project_metadata =
      {
        (super#project_metadata) with
        commit_author_username =
          Glom.(
            get_and_coerce_opt string gha_env._GITHUB_EVENT_JSON
              [ k "sender"; k "login" ]);
        commit_author_image_url =
          Glom.(
            get_and_coerce_opt string gha_env._GITHUB_EVENT_JSON
              [ k "sender"; k "avatar_url" ])
          |> Option.map Uri.of_string;
        pull_request_author_username =
          Glom.(
            get_and_coerce_opt string gha_env._GITHUB_EVENT_JSON
              [ k "pull_request"; k "user"; k "login" ]);
        pull_request_author_image_url =
          Glom.(
            get_and_coerce_opt string gha_env._GITHUB_EVENT_JSON
              [ k "pull_request"; k "user"; k "avatar_url" ])
          |> Option.map Uri.of_string;
        repo_id = gha_env._GITHUB_REPOSITORY_ID;
        org_id = gha_env._GITHUB_REPOSITORY_OWNER_ID;
      }

    method! repo_name =
      let err = "Could not get repo_name when running in GitHub Action" in
      if Option.is_none env._SEMGREP_REPO_NAME then
        match gha_env._GITHUB_REPOSITORY with
        | Some repo_name -> repo_name
        | None -> failwith err
      else failwith err

    method! repo_url =
      match (env._SEMGREP_REPO_URL, self#repo_name) with
      | (Some _ as v), _ -> v
      | None, repo_name ->
          Some (Uri.with_path gha_env._GITHUB_SERVER_URL repo_name)

    method private is_pull_request_event =
      match super#event_name with
      | "pull_request"
      | "pull_request_target" ->
          true
      | _else_ -> false

    method! commit_sha =
      if self#is_pull_request_event then
        Option.bind
          (Glom.get_and_coerce_opt Glom.string gha_env._GITHUB_EVENT_JSON
             Glom.[ k "pull_request"; k "head"; k "sha" ])
          Digestif.SHA1.of_hex_opt
      else if super#event_name =*= "push" then gha_env._GITHUB_SHA
      else env._SEMGREP_COMMIT

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
    method! branch =
      if super#event_name =*= "pull_request_target" then
        gha_env._GITHUB_HEAD_REF
      else
        match (env._SEMGREP_BRANCH, gha_env._GITHUB_REF) with
        | Some branch, _ -> Some branch
        | None, Some branch -> Some branch
        | None, None -> super#branch

    method! pr_id =
      match super#pr_id with
      | Some _ as value -> value
      | None ->
          Glom.(
            get_and_coerce_opt int gha_env._GITHUB_EVENT_JSON
              [ k "pull_request"; k "number" ])
          |> Option.map string_of_int

    method! pr_title =
      match super#pr_title with
      | Some _ as value -> value
      | None ->
          Glom.(
            get_and_coerce_opt string gha_env._GITHUB_EVENT_JSON
              [ k "pull_request"; k "title" ])

    method! ci_job_url =
      match super#ci_job_url with
      | Some _ as value -> value
      | None -> (
          match (super#repo_url, gha_env._GITHUB_RUN_ID) with
          | Some repo_url, Some value ->
              Some (Uri.with_path repo_url (spf "/actions/runs/%s" value))
          | _ -> None)

    method! event_name =
      match gha_env._GITHUB_EVENT_NAME with
      | Some x -> x
      | None -> super#event_name

    (* TODO
        method! merge_base_ref =
          match (self#is_pull_request_event, get_head_branch_hash gha_env) with
          | true, Some _ -> find_branchoff_point_from_github_api self#repo_name gha_env
          | _ -> Lwt.return_none
    *)
  end
