open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Gather Semgrep app related code.
 *
 * TODO? split some code in Auth.ml?
 *)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let semgrep_app_config_route = "api/agent/deployments/scans/config"
let semgrep_app_deployment_route = "api/agent/deployments/current"

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* TODO? specify this with atd and have both app + osemgrep use it *)
(* Pulled from cli/src/semgrep/app/scans.py *)
(* coupling: response from semgrep app (e.g. deployment_id as int vs string ) *)
type deployment_config = {
  deployment_id : int;
  deployment_name : string;
  policy_names : string list;
  rule_config_raw : string; [@key "rule_config"]
  autofix : bool; [@default false]
  deepsemgrep : bool; [@default false]
  dependency_query : bool; [@default false]
  (* Assuming skipped syntactic IDs has the equivalent effect as match_based *)
  skipped_match_based_ids : string list;
      [@key "triage_ignored_match_based_ids"] [@default []]
  enabled_products : string list; [@default []]
  ignore_files : string list; [@default []]
}
[@@deriving yojson]

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Returns the deployment config if the token is valid, otherwise None *)
let get_deployment_from_token_async ~token =
  let%lwt response =
    Http_helpers.get_async
      ~headers:[ ("authorization", "Bearer " ^ token) ]
      (Uri.with_path !Semgrep_envvars.v.semgrep_url semgrep_app_deployment_route)
  in
  let deployment_opt =
    match response with
    | Error msg ->
        Logs.debug (fun m -> m "error while retrieving deployment: %s" msg);
        None
    | Ok json -> (
        try
          let yojson = Yojson.Safe.from_string json in
          let config = deployment_config_of_yojson yojson in
          match config with
          | Ok config -> Some config
          | Error msg -> raise (Yojson.Json_error msg)
        with
        | Yojson.Json_error msg ->
            Logs.debug (fun m -> m "failed to parse json %s: %s" msg json);
            None)
  in
  Lwt.return deployment_opt

(* from auth.py *)
let get_deployment_from_token ~token =
  Lwt_main.run (get_deployment_from_token_async ~token)

let scan_config_uri ?(sca = false) ?(dry_run = true) ?(full_scan = true)
    repo_name =
  let json_bool_to_string b = JSON.(string_of_json (Bool b)) in
  Uri.(
    add_query_params'
      (with_path !Semgrep_envvars.v.semgrep_url semgrep_app_config_route)
      [
        ("sca", json_bool_to_string sca);
        ("dry_run", json_bool_to_string dry_run);
        ("full_scan", json_bool_to_string full_scan);
        ("repo_name", repo_name);
        ("semgrep_version", Version.version);
      ])

(* Returns a url with scan config encoded via search params based on a magic environment variable *)
let url_for_policy ~token =
  let deployment_config = get_deployment_from_token ~token in
  match deployment_config with
  | None ->
      Error.abort
        (spf "Invalid API Key. Run `semgrep logout` and `semgrep login` again.")
  | Some _deployment_config -> (
      (* NOTE: This logic is ported directly from python but seems very brittle
         as we have helper functions to infer the repo name from the git remote
         information.
      *)
      match Sys.getenv_opt "SEMGREP_REPO_NAME" with
      | None ->
          Error.abort
            (spf
               "Need to set env var SEMGREP_REPO_NAME to use `--config policy`")
      | Some repo_name -> scan_config_uri repo_name)
