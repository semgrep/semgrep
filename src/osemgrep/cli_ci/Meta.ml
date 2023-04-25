(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type 'e t = {
  repository : string;
  repo_url : Uri.t option;
  branch : string option;
  ci_job_url : Uri.t option;
  commit : Digestif.SHA1.t option;
  commit_author_email : Emile.mailbox option;
  commit_author_name : string option;
  commit_author_username : string option;
  commit_author_image_url : Uri.t option;
  commit_title : string option;
  on : string option;
  pull_request_author_username : string option;
  pull_request_author_image_url : Uri.t option;
  pull_request_id : string option;
  pull_request_title : string option;
  scan_environment : string option;
  is_full_scan : bool;
  extension : 'e;
}

module type S = sig
  type env
  type extension

  (** Accessors. *)

  val get_event_name : env -> string option
  val get_repo_name : env -> string
  val get_repo_url : env -> Uri.t option
  val get_commit_sha : env -> Digestif.SHA1.t option
  val get_ci_job_url : env -> Uri.t option
  val get_pr_id : env -> string option
  val get_pr_title : env -> string option
  val get_branch : env -> string option
  val get_merge_base_ref : env -> Digestif.SHA1.t option Lwt.t

  (** Cmdliner helpers. *)

  val env : env Cmdliner.Term.t
  val make : env -> extension t
  val term : extension t Cmdliner.Term.t
end

let _MAX_FETCH_ATTEMPT_COUNT = 10
(* A limit of how many fetch we should do until we find the common commit
   between two branches. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(** Gets regular url from sstp url.

    We use repo urls on semgrep-app to link to files, so we need to make sure
    they are in the right format to be appended to. We do this by parsing the
    url with a git url parser and rebuilding it into an HTTP/S url. *)
let get_url_from_sstp_url = function
  | None -> None
  | Some uri -> (
      let uri = Uri.of_string uri in
      match
        (Uri.scheme uri, Uri.host uri, Uri.path uri |> String.split_on_char '/')
      with
      | _, Some resource, ([ ""; _owner; _name ] as path) ->
          (* XXX(dinosaure): [path] with or without [""] at the beginning
             produces the same result. *)
          Uri.make ~scheme:"https" ~host:resource ~path:(String.concat "/" path)
            ()
          |> Option.some
      | Some ("http" | "https"), _, _ ->
          Uri.with_scheme uri (Some "https") |> Option.some
      | _ -> Some uri)

let _get_repo_name_from_repo_url value =
  Option.bind value @@ fun str ->
  let uri = Uri.of_string str in
  match Uri.path uri |> String.split_on_char '/' with
  | [ ""; owner; name ] -> String.concat "/" [ owner; name ] |> Option.some
  | _ -> None

let git_check_output cmd =
  let out = Bos.OS.Cmd.run_out cmd in
  match Bos.OS.Cmd.out_string ~trim:true out with
  | Ok (str, (_, `Exited 0)) -> str
  | Ok _
  | Error (`Msg _) ->
      let fmt : _ format4 =
        {|Command failed.
-----
Failed to run %a. Possible reasons:
- the git binary is not available
- the current working directory is not a git repository
- the current working directory is not marked as safe
  (fix with `git config --global --add safe.directory $(pwd)`)

Try running the command yourself to debug the issue.|}
      in
      Logs.warn (fun m -> m fmt Bos.Cmd.pp cmd);
      failwith "Error when we run a git command"

(* XXX(dinosaure): [meta.py] uses a library [glom] which is able to get values
   from a JSON value. This module implements what we need to get some values
   from a JSON file. It's like a poor lense for JSON... *)
module Glom = struct
  [@@@warning "-34"]
  [@@@warning "-32"]

  type t = Yojson.Basic.t

  let cli =
    let parser str =
      if Sys.file_exists str then
        try Ok (str, Yojson.Basic.from_file ~fname:str str) with
        | Yojson.Json_error _ ->
            Error (`Msg (Fmt.str "Invalid JSON file: %s" str))
      else Error (`Msg (Fmt.str "%s does not exist" str))
    in
    let pp ppf (filename, _) = Fmt.string ppf filename in
    Cmdliner.Arg.conv ~docv:"<JSON file>" (parser, pp)

  let default = (String.empty, `Null)

  type elt = K of string | N of int

  type value =
    [ `String of string | `Bool of bool | `Float of float | `Int of int | `Null ]

  type 'a k =
    | String : string k
    | Bool : bool k
    | Float : float k
    | Int : int k
    | Unit : unit k

  let k key = K key
  let n n = N n
  let string = String
  let bool = Bool
  let float = Float
  let int = Int
  let unit = Unit

  let coerce : type a. a k -> value -> a option =
   fun k v ->
    match (k, v) with
    | String, `String v -> Some v
    | Bool, `Bool v -> Some v
    | Float, `Float v -> Some v
    | Int, `Int v -> Some v
    | Unit, `Null -> Some ()
    | __else__ -> None

  let rec get json path =
    match (json, path) with
    | (#value as value), [] -> value
    | `Assoc lst, K key :: rest -> (
        match List.assoc_opt key lst with
        | Some json -> get json rest
        | None -> raise Not_found)
    | `List lst, N n :: rest -> (
        match List.nth_opt lst n with
        | Some json -> get json rest
        | None -> raise Not_found)
    | __else__ -> raise Not_found

  let get_opt json path =
    try Some (get json path) with
    | Not_found -> None

  let get_and_coerce_opt ty json path =
    Option.bind (get_opt json path) (coerce ty)

  [@@@warning "+32"]
  [@@@warning "+34"]
end

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

module Git = struct
  type extension = unit

  type env = {
    _SEMGREP_REPO_NAME : string option;
    _SEMGREP_REPO_URL : Uri.t option;
    _SEMGREP_COMMIT : Digestif.SHA1.t option;
    _SEMGREP_JOB_URL : Uri.t option;
    _SEMGREP_PR_ID : string option;
    _SEMGREP_PR_TITLE : string option;
    _SEMGREP_BRANCH : string option;
  }

  let env =
    let open Cmdliner in
    let _semgrep_repo_name =
      let doc = "The name of the Git repository." in
      let env = Cmd.Env.info "SEMGREP_REPO_NAME" in
      Arg.(value & opt (some string) None & info [] ~env ~doc)
    in
    let _semgrep_repo_url =
      let doc = "The URL of the Git repository." in
      let env = Cmd.Env.info "SEMGREP_REPO_URL" in
      Arg.(value & opt (some Cmdliner_helpers.uri) None & info [] ~env ~doc)
    in
    let _semgrep_commit =
      let doc = "The commit of the Git repository." in
      let env = Cmd.Env.info "SEMGREP_COMMIT" in
      Arg.(value & opt (some Cmdliner_helpers.sha1) None & info [] ~env ~doc)
    in
    let _semgrep_job_url =
      let doc = "The job URL." in
      let env = Cmd.Env.info "SEMGREP_JOB_URL" in
      Arg.(value & opt (some Cmdliner_helpers.uri) None & info [] ~env ~doc)
    in
    let _semgrep_pr_id =
      let doc = "The PR/MR ID." in
      let env = Cmd.Env.info "SEMGREP_PR_ID" in
      Arg.(value & opt (some string) None & info [] ~env ~doc)
    in
    let _semgrep_pr_title =
      let doc = "The PR/MR title." in
      let env = Cmd.Env.info "SEMGREP_PR_TITLE" in
      Arg.(value & opt (some string) None & info [] ~env ~doc)
    in
    let _semgrep_branch =
      let doc = "The Git branch." in
      let env = Cmd.Env.info "SEMGREP_BRANCH" in
      Arg.(value & opt (some string) None & info [] ~env ~doc)
    in
    let run _SEMGREP_REPO_NAME _SEMGREP_REPO_URL _SEMGREP_COMMIT
        _SEMGREP_JOB_URL _SEMGREP_PR_ID _SEMGREP_PR_TITLE _SEMGREP_BRANCH =
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

  let get_event_name env =
    match env._SEMGREP_PR_ID with
    | Some _ -> Some "pull_request"
    | None -> None

  let is_pull_request_event env =
    match get_event_name env with
    | Some ("pull_request" | "pull_request_target") -> true
    | _ -> false

  let get_repo_name env =
    match env._SEMGREP_REPO_NAME with
    | Some repo_name -> repo_name
    | None ->
        let str =
          git_check_output Bos.Cmd.(v "git" % "rev-parse" % "--show-toplevel")
        in
        Fpath.basename (Fpath.v str)

  let get_repo_url env =
    match env._SEMGREP_REPO_URL with
    | Some repo_url -> Some repo_url
    | None -> (
        let cmd = Bos.Cmd.(v "git" % "remote" % "get-url" % "origin") in
        let out = Bos.OS.Cmd.run_out cmd in
        match Bos.OS.Cmd.out_string ~trim:true out with
        | Ok (str, _status) -> get_url_from_sstp_url (Some str)
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
          | _ -> Error (`Msg "Invalid status")
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

  let make env =
    let commit_title =
      git_check_output Bos.Cmd.(v "git" % "show" % "-s" % "--format=%B")
    in
    let commit_author_email =
      git_check_output Bos.Cmd.(v "git" % "show" % "-s" % "--format=%ae")
      |> Emile.of_string |> Result.get_ok
    in
    let commit_author_name =
      git_check_output Bos.Cmd.(v "git" % "show" % "-s" % "--format=%an")
    in
    let on =
      if Option.is_some env._SEMGREP_PR_ID then Some "pull_request" else None
    in
    {
      repository = get_repo_name env;
      repo_url = get_repo_url env;
      branch = get_branch env;
      ci_job_url = get_ci_job_url env;
      commit = get_commit_sha env;
      commit_author_email = Some commit_author_email;
      commit_author_name = Some commit_author_name;
      commit_author_username = None;
      commit_author_image_url = None;
      commit_title = Some commit_title;
      on;
      pull_request_author_username = None;
      pull_request_author_image_url = None;
      pull_request_id = env._SEMGREP_PR_ID;
      pull_request_title = env._SEMGREP_PR_TITLE;
      scan_environment = Some "git";
      is_full_scan = true;
      extension = ();
    }

  let term = Cmdliner.Term.(const make $ env)
end

module GitHub = struct
  type extension = unit

  type env = {
    git : Git.env;
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

  let get_repo_name env =
    let err = "Could not get repo_name when running in GitHub Action" in
    if Option.is_none env.git.Git._SEMGREP_REPO_NAME then
      match env._GITHUB_REPOSITORY with
      | Some repo_name -> repo_name
      | None -> failwith err
    else failwith err

  let get_repo_url env =
    match (env.git.Git._SEMGREP_REPO_URL, get_repo_name env) with
    | (Some _ as v), _ -> v
    | None, repo_name -> Some (Uri.with_path env._GITHUB_SERVER_URL repo_name)

  let get_commit_sha env =
    if Git.is_pull_request_event env.git then
      Option.bind
        (Glom.get_and_coerce_opt Glom.string env._GITHUB_EVENT_JSON
           Glom.[ k "pull_request"; k "head"; k "sha" ])
        Digestif.SHA1.of_hex_opt
    else if Git.get_event_name env.git = Some "push" then env._GITHUB_SHA
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
    if Git.get_event_name env.git = Some "pull_request_target" then
      env._GITHUB_HEAD_REF
    else
      match (env.git._SEMGREP_BRANCH, env._GITHUB_REF) with
      | Some branch, _ -> Some branch
      | None, Some branch -> Some branch
      | None, None -> Git.get_branch env.git

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
    match Git.get_ci_job_url env.git with
    | Some _ as value -> value
    | None -> (
        match (Git.get_repo_url env.git, env._GITHUB_RUN_ID) with
        | Some repo_url, Some value ->
            Some (Uri.with_path repo_url (Fmt.str "/actions/runs/%s" value))
        | _ -> None)

  let get_event_name env = env._GITHUB_EVENT_NAME

  (* Split out shallow fetch so we can mock it away in tests. *)
  let _shallow_fetch_branch branch_name =
    let _ =
      git_check_output
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
      git_check_output
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
    git_check_output Bos.Cmd.(v "git" % "rev-parse" % branch_name)
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
          git_check_output
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
        Http_lwt_client.request ~meth:`GET ~headers
          (Fmt.str "%a/repos/%s/compare/%a...%a" Uri.pp api_url repo_name
             Digestif.SHA1.pp base_branch_hash Digestif.SHA1.pp head_branch_hash)
          (fun _resp buf str ->
            Buffer.add_string buf str;
            Lwt.return buf)
          (Buffer.create 0x100)
        >>= function
        | Ok ({ Http_lwt_client.status = `OK; _ }, buf) ->
            let body = Buffer.contents buf |> Yojson.Basic.from_string in
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
    let fetch_depth = fetch_depth + Semgrep_envvars.env.min_fetch_depth in
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
        git_check_output
          Bos.Cmd.(
            v "git" % "fetch" % "origin" % "--force" % "--update-head-ok"
            % "--depth" % string_of_int fetch_depth
            % Fmt.str "%s:%s" base_branch_name base_branch_name)
      in
      let _ =
        git_check_output
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
            "Could not find branch-off point between the baseline tip %s@%a \
             and current head %s@%a"
            base_branch_name Digestif.SHA1.pp base_branch_hash head_branch_name
            Digestif.SHA1.pp head_branch_hash
      | Error (`Msg err) -> failwith err

  let get_merge_base_ref env =
    match (Git.is_pull_request_event env.git, get_head_branch_hash env) with
    | true, Some _ -> _find_branchoff_point_from_github_api env
    | _ -> Lwt.return_none

  let env =
    let open Cmdliner in
    let _github_event_path =
      let doc = "The GitHub event path." in
      let env = Cmd.Env.info "GITHUB_EVENT_PATH" in
      Arg.(value & opt Glom.cli Glom.default & info [] ~env ~doc)
    in
    let _github_sha =
      let doc = "The GitHub commit." in
      let env = Cmd.Env.info "GITHUB_SHA" in
      Arg.(value & opt (some Cmdliner_helpers.sha1) None & info [] ~env ~doc)
    in
    let gh_token =
      let doc = "The GitHub token." in
      let env = Cmd.Env.info "GH_TOKEN" in
      Arg.(value & opt (some string) None & info [] ~env ~doc)
    in
    let _github_repository =
      let doc = "The GitHub repository." in
      let env = Cmd.Env.info "GITHUB_REPOSITORY" in
      Arg.(value & opt (some string) None & info [] ~env ~doc)
    in
    let _github_server_url =
      let doc = "The GitHub server URL." in
      let env = Cmd.Env.info "GITHUB_SERVER_URL" in
      Arg.(
        value
        & opt Cmdliner_helpers.uri (Uri.of_string "https://github.com")
        & info [] ~doc ~env)
    in
    let _github_api_url =
      let doc = "The GitHub API URL." in
      let env = Cmd.Env.info "GITHUB_API_URL" in
      Arg.(value & opt (some Cmdliner_helpers.uri) None & info [] ~doc ~env)
    in
    let _github_run_id =
      let doc = "The GitHub run ID." in
      let env = Cmd.Env.info "GITHUB_RUN_ID" in
      Arg.(value & opt (some string) None & info [] ~doc ~env)
    in
    let _github_event_name =
      let doc = "The GitHub event name." in
      let env = Cmd.Env.info "GITHUB_EVENT_NAME" in
      Arg.(value & opt (some string) None & info [] ~doc ~env)
    in
    let _github_ref =
      let doc = "The GitHub ref." in
      let env = Cmd.Env.info "GITHUB_REF" in
      Arg.(value & opt (some string) None & info [] ~doc ~env)
    in
    let _github_head_ref =
      let doc = "The GitHub HEAD ref." in
      let env = Cmd.Env.info "GITHUB_HEAD_REF" in
      Arg.(value & opt (some string) None & info [] ~doc ~env)
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
      const run $ Git.env $ _github_event_path $ _github_sha
      $ _github_repository $ _github_server_url $ _github_api_url
      $ _github_run_id $ _github_event_name $ _github_ref $ _github_head_ref
      $ gh_token)

  let make env =
    let _SEMGREP_REPO_NAME = Some (get_repo_name env) in
    let _SEMGREP_REPO_URL = get_repo_url env in
    let commit_author_username =
      Glom.(
        get_and_coerce_opt string env._GITHUB_EVENT_JSON
          [ k "sender"; k "login" ])
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
      Git.make { env.git with _SEMGREP_REPO_NAME; _SEMGREP_REPO_URL }
    in
    {
      value with
      commit_author_username;
      commit_author_image_url;
      pull_request_author_username;
      pull_request_author_image_url;
    }

  let term = Cmdliner.Term.(const make $ env)
end
