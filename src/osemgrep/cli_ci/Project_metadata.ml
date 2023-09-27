(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Collect information about the project from the environment.
 *
 * This module is a translation of [meta.py] renamed to [Project_metadata.ml]
 * and split in other modules (e.g., [Git_metadata.ml], [Github_metadata.ml]).
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the type is now defined in ATD so it can be reused with our backend *)
type t = Semgrep_output_v1_t.project_metadata

module type S = sig
  type env

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
  val make : env -> t
  val term : t Cmdliner.Term.t
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
