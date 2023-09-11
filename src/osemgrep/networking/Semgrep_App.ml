open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Gather Semgrep app related code.
 *
 * TODO? split some code in Auth.ml?
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

let semgrep_app_config_path = "api/agent/deployments/scans/config"
let semgrep_app_deployment_path = "api/agent/deployments/current"

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Returns the deployment id and name if the token is valid, otherwise None *)
let get_deployment_from_token ~token =
  match
    Http_helpers.get
      ~headers:[ ("authorization", "Bearer " ^ token) ]
      (Uri.with_path !Semgrep_envvars.v.semgrep_url semgrep_app_deployment_path)
  with
  | Error msg ->
      Logs.debug (fun m -> m "error while retrieving deployment: %s" msg);
      None
  | Ok body -> (
      try
        let json = Yojson.Basic.from_string body in
        let open Yojson.Basic.Util in
        match json |> member "deployment" |> member "name" with
        | `String name ->
            let id = json |> member "deployment" |> member "id" |> to_int in
            Some (id, name)
        | `Null
        | _ ->
            Logs.debug (fun m ->
                m "could not destructure deployment response: %s"
                  (json |> to_string));
            None
      with
      | Yojson.Json_error msg ->
          Logs.debug (fun m -> m "failed to parse json %s: %s" msg body);
          None)

let scan_config ?(sca = false) ?(dry_run = true) ?(full_scan = true) repo_name =
  let json_bool_to_string b = JSON.(string_of_json (Bool b)) in
  Uri.(
    add_query_params'
      (with_path !Semgrep_envvars.v.semgrep_url semgrep_app_config_path)
      [
        ("sca", json_bool_to_string sca);
        ("dry_run", json_bool_to_string dry_run);
        ("full_scan", json_bool_to_string full_scan);
        ("repo_name", repo_name);
        ("semgrep_version", Version.version);
      ])

(* Returns a url with scan config encoded via search params based on a magic environment variable *)
let url_for_policy ~token =
  match get_deployment_from_token ~token with
  | None ->
      Error.abort
        (spf "Invalid API Key. Run `semgrep logout` and `semgrep login` again.")
  | Some (_id, _name) -> (
      (* NOTE: This logic is ported directly from python but seems very sus *)
      match Sys.getenv_opt "SEMGREP_REPO_NAME" with
      | None ->
          Error.abort
            (spf
               "Need to set env var SEMGREP_REPO_NAME to use `--config policy`")
      | Some repo_name -> scan_config repo_name)
