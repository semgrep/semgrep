(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Collect information about the project from the environment.
 *
 * This module is a translation of meta.py renamed to Project_metadata.ml
 * and split in other modules (e.g., Git_metadata.ml, Github_metadata.ml).
 *
 * This file could be moved in cli_ci/ because it is used only for semgrep ci,
 * but it is also used in networking/Semgrep_app.ml (which could also
 * arguably be moved under cli_ci/) so simpler to leave it in core/ for now.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* the type is now defined in ATD so it can be reused with our backend *)
type t = Semgrep_output_v1_t.project_metadata

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
