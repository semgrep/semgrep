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

(* TODO: use atd to serialize (and the same in the Semgrep App) *)
let to_dict t =
  let open JSON in
  let or_null = Option.value ~default:Null in
  Object
    [
      ("semgrep_version", String Version.version);
      (* REQUIRED for semgrep-app backend *)
      ("repository", String t.repository);
      (* OPTIONAL for semgrep-app backend *)
      ( "repo_url",
        or_null (Option.map (fun url -> String (Uri.to_string url)) t.repo_url)
      );
      ("branch", or_null (Option.map (fun branch -> String branch) t.branch));
      ( "ci_job_url",
        or_null
          (Option.map (fun url -> String (Uri.to_string url)) t.ci_job_url) );
      ( "commit",
        or_null
          (Option.map (fun sha -> String (Digestif.SHA1.to_hex sha)) t.commit)
      );
      ( "commit_author_email",
        or_null
          (Option.map
             (fun email -> String (Emile.to_string email))
             t.commit_author_email) );
      ( "commit_author_name",
        or_null (Option.map (fun name -> String name) t.commit_author_name) );
      ( "commit_author_username",
        or_null (Option.map (fun name -> String name) t.commit_author_username)
      );
      ( "commit_author_image_url",
        or_null
          (Option.map
             (fun url -> String (Uri.to_string url))
             t.commit_author_image_url) );
      ( "commit_title",
        or_null (Option.map (fun name -> String name) t.commit_title) );
      ("on", or_null (Option.map (fun name -> String name) t.on));
      ( "pull_request_author_username",
        or_null
          (Option.map (fun name -> String name) t.pull_request_author_username)
      );
      ( "pull_request_author_image_url",
        or_null
          (Option.map
             (fun url -> String (Uri.to_string url))
             t.pull_request_author_image_url) );
      ( "pull_request_id",
        or_null (Option.map (fun pr_id -> String pr_id) t.pull_request_id) );
      ( "pull_request_title",
        or_null (Option.map (fun title -> String title) t.pull_request_title) );
      ( "scan_environment",
        or_null (Option.map (fun env -> String env) t.scan_environment) );
      ("is_full_scan", Bool t.is_full_scan);
    ]

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
      | __else__ -> Some uri)

let get_repo_name_from_repo_url value =
  Option.bind value @@ fun str ->
  let uri = Uri.of_string str in
  match Uri.path uri |> String.split_on_char '/' with
  | [ ""; owner; name ] -> String.concat "/" [ owner; name ] |> Option.some
  | __else__ -> None

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
