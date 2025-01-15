(* Yoann Padioleau
 *
 * Copyright (C) 2023-2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Gather Semgrep App (backend) related code.
 *
 * See semgrep_output_v1.atd section on comms with the backend to
 * learn about the sequence of HTTP requests used by semgrep ci.
 *
 * invariant: this module and directory should be the only places where we
 * call Http_helpers. This module provides an abstract and typed interface to
 * our Semgrep backend.
 * alt: maybe grpc was better than ATD for the CLI<->backend comms?
 * TODO: write a (embedded) semgrep rule for it
 *
 * invariant: this module (and Semgrep_login.ml) should be the only place where
 * we use !Semgrep_envvars.v.semgrep_url
 * TODO: write a (embedded) semgrep rule for it
 *
 * Partially translated from auth.py and scans.py.
 * TODO? move some code in Auth.ml?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* LATER: declare this in semgrep_output_v1.atd instead? *)
type scan_id = int
type app_block_override = string (* reason *) option
type pro_engine_arch = Osx_arm64 | Osx_x86_64 | Manylinux_x86_64

(*****************************************************************************)
(* Routes *)
(*****************************************************************************)

(* used by semgrep ci *)
let deployment_route = "/api/agent/deployments/current"

(* old: was "/api/agent/deployments/scans" *)
let start_scan_route = "/api/cli/scans"
let results_route scan_id = spf "/api/agent/scans/%d/results" scan_id
let complete_route scan_id = spf "/api/agent/scans/%d/complete" scan_id
let error_route scan_id = spf "/api/agent/scans/%d/error" scan_id

(* used by semgrep lsp: TODO: diff with api/agent/scans/<scan_id>/config? *)
let scan_config_route = "/api/agent/deployments/scans/config"

(* used by ? *)
let identity_route = "/api/agent/identity"

(* used by semgrep publish *)
let registry_rule_route = "/api/registry/rules"

(* used by semgrep install-semgrep-pro *)
let pro_binary_route (platform_kind : pro_engine_arch) =
  let arch_str =
    match platform_kind with
    | Osx_arm64 -> "osx-arm64"
    | Osx_x86_64 -> "osx-x86"
    | Manylinux_x86_64 -> "manylinux"
  in
  "api/agent/deployments/deepbinary/" ^ arch_str

(*****************************************************************************)
(* Extractors *)
(*****************************************************************************)

(* the server reply when POST to "scans/<scan_id>/results"  *)
let extract_errors (data : string) : string list =
  match Out.ci_scan_results_response_of_string data with
  | { errors; task_id = _ } as response ->
      Logs.debug (fun m ->
          m "results response = %s" (Out.show_ci_scan_results_response response));
      errors
      |> List_.map (fun (x : Out.ci_scan_results_response_error) -> x.message)
  | exception exn ->
      Logs.err (fun m ->
          m "Failed to decode server reply as json %s: %s"
            (Printexc.to_string exn) data);
      []

(* the server reply when POST to "scans/<scan_id>/complete" *)
let extract_block_override (data : string) : (app_block_override, string) result
    =
  match Out.ci_scan_complete_response_of_string data with
  | {
      success = _;
      app_block_override;
      app_block_reason;
      app_blocking_match_based_ids = _TODO;
    } as response ->
      Logs.debug (fun m ->
          m "complete response = %s"
            (Out.show_ci_scan_complete_response response));
      if app_block_override then Ok (Some app_block_reason)
        (* TODO? can we have a app_block_reason set when override is false? *)
      else Ok None
  | exception exn ->
      Error
        (spf "Failed to decode server reply as json %s: %s"
           (Printexc.to_string exn) data)

(*****************************************************************************)
(* Step1: deployment config *)
(*****************************************************************************)

(* Returns the deployment config if the token is valid, otherwise None *)
let get_deployment_from_token_async caps : Out.deployment_config option Lwt.t =
  let headers =
    [
      (* The agent is needed by many endpoints in our backend guarded by
       * @require_supported_cli_version()
       * alt: use Metrics_.string_of_user_agent()
       *)
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url deployment_route in
  let%lwt response = Http_helpers.get ~headers caps#network url in
  let deployment_opt =
    match response with
    | Ok { body = Ok body; _ } ->
        let x = Out.deployment_response_of_string body in
        Some x.deployment
    | Ok { body = Error msg; code; _ } ->
        Logs.err (fun m ->
            m "error while retrieving deployment, %s returned %u: %s"
              (Uri.to_string url) code msg);
        None
    | Error e ->
        Logs.err (fun m -> m "error while retrieving deployment: %s" e);
        None
  in
  Lwt.return deployment_opt

(* from auth.py *)
let get_deployment_from_token token =
  Lwt_platform.run (get_deployment_from_token_async token)

(*****************************************************************************)
(* Step2 : start scan *)
(*****************************************************************************)

let start_scan_async caps (request : Out.scan_request) :
    (Out.scan_response, string) result Lwt.t =
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url start_scan_route in
  let body = Out.string_of_scan_request request in
  let pretty_body =
    body |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string
  in
  Logs.debug (fun m -> m "Starting scan: %s" pretty_body);
  let%lwt response = Http_helpers.post ~body ~headers caps#network url in
  let res =
    match response with
    | Ok { body = Ok body; _ } ->
        let x = Out.scan_response_of_string body in
        Ok x
    | Ok { body = Error err; code; _ } ->
        (* TODO: handle code 401 and exit with invalid_api_key error *)
        let pre_msg =
          if code =|= 404 then
            {|Failed to create a scan with given token and deployment_id.
Please make sure they have been set correctly.
|}
          else ""
        in
        let msg =
          spf "%sAPI server at %s returned this error: %s" pre_msg
            (Uri.to_string url) err
        in
        Error msg
    | Error e -> Error (spf "Failed to start scan: %s" e)
  in
  Lwt.return res

let start_scan caps request = Lwt_platform.run (start_scan_async caps request)

(*****************************************************************************)
(* Step3 : upload findings *)
(*****************************************************************************)

(* python: was called report_findings *)
let upload_findings_async caps ~scan_id ~results ~complete :
    (app_block_override, string) result Lwt.t =
  let results = Out.string_of_ci_scan_results results in
  let complete = Out.string_of_ci_scan_complete complete in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url (results_route scan_id)
  in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  Logs.debug (fun m -> m "Sending findings and ignores blob");
  let body = results in
  let%lwt () =
    match%lwt Http_helpers.post ~body ~headers caps#network url with
    | Ok { body = Ok body; _ } ->
        let errors = extract_errors body in
        errors
        |> List.iter (fun s ->
               Logs.warn (fun m -> m "Server returned following warning: %s" s));
        Lwt.return_unit
    | Ok { body = Error msg; code; _ } ->
        Logs.warn (fun m -> m "API server returned %u, this error: %s" code msg);
        Lwt.return_unit
    | Error e ->
        Logs.warn (fun m -> m "Failed to upload findings: %s" e);
        Lwt.return_unit
  in
  (* mark as complete *)
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url (complete_route scan_id)
  in
  Logs.debug (fun m -> m "Sending complete blob");
  let body = complete in
  match%lwt Http_helpers.post ~body ~headers caps#network url with
  | Ok { body = Ok body; _ } -> Lwt.return (extract_block_override body)
  | Ok { body = Error msg; code; _ } ->
      let msg =
        spf "Failed to upload findings, API server returned %u, this error: %s"
          code msg
      in
      Lwt.return_error msg
  | Error e -> Lwt.return_error (spf "Failed to upload findings: %s" e)

let upload_findings caps ~scan_id ~results ~complete =
  Lwt_platform.run (upload_findings_async caps ~scan_id ~results ~complete)

(*****************************************************************************)
(* Error reporting to the backend *)
(*****************************************************************************)

(* report a failure for [scan_id] to Semgrep App *)
let report_failure_async caps ~scan_id (exit_code : Exit_code.t) : unit Lwt.t =
  let int_code = Exit_code.to_int exit_code in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url (error_route scan_id)
  in
  let failure : Out.ci_scan_failure =
    { exit_code = int_code; (* TODO *)
                            stderr = "" }
  in
  let body = Out.string_of_ci_scan_failure failure in
  match%lwt Http_helpers.post ~body ~headers caps#network url with
  | Ok { body = Ok _; _ } -> Lwt.return_unit
  | Ok { body = Error msg; code; _ } ->
      Logs.warn (fun m -> m "API server returned %u, this error: %s" code msg);
      Lwt.return_unit
  | Error e ->
      Logs.warn (fun m -> m "Failed to report failure: %s" e);
      Lwt.return_unit

let report_failure caps ~scan_id exit_code =
  Lwt_platform.run (report_failure_async caps ~scan_id exit_code)

(*****************************************************************************)
(* Other ways to fetch a config (deprecated?) *)
(*****************************************************************************)

(* deprecated? *)
let scan_config_uri ?(sca = false) ?(dry_run = true) ?(full_scan = true)
    repo_name =
  let json_bool_to_string b = JSON.(string_of_json (Bool b)) in
  Uri.(
    add_query_params'
      (with_path !Semgrep_envvars.v.semgrep_url scan_config_route)
      [
        ("sca", json_bool_to_string sca);
        ("dry_run", json_bool_to_string dry_run);
        ("full_scan", json_bool_to_string full_scan);
        ("repo_name", repo_name);
        ("semgrep_version", Version.version);
      ])

(* Returns a url with scan config encoded via search params based on a magic
 * environment variable *)
let url_for_policy caps =
  let deployment_config = get_deployment_from_token caps in
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

(* used by semgrep lsp *)
let fetch_scan_config_string_async ~dry_run ~sca ~full_scan ~repository caps :
    (string, string) result Lwt.t =
  (* TODO? seems like there are 2 ways to get a config, with the scan_params
   * or with a scan_id.
   * python:
   *   if self.dry_run:
   *    app_get_config_url = f"{state.env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{self._scan_params}"
   *   else:
   *    app_get_config_url = f"{state.env.semgrep_url}/api/agent/deployments/scans/{self.scan_id}/config"
   *)
  let url = scan_config_uri ~sca ~dry_run ~full_scan repository in
  let headers =
    [
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let%lwt conf_string =
    let%lwt response = Http_helpers.get ~headers caps#network url in
    let results =
      match response with
      | Ok { body = Ok body; _ } -> Ok body
      | Ok { body = Error msg; code; _ } ->
          Error
            (Printf.sprintf "Failed to download config, %s returned %u: %s"
               (Uri.to_string url) code msg)
      | Error e ->
          Error
            (Printf.sprintf "Failed to download config from %s: %s"
               (Uri.to_string url) e)
    in
    Lwt.return results
  in
  Logs.debug (fun m -> m "finished downloading from %s" (Uri.to_string url));
  Lwt.return conf_string

(*****************************************************************************)
(* Other endpoints *)
(*****************************************************************************)

let fetch_pro_binary caps platform_kind =
  let uri =
    Uri.(
      add_query_params'
        (with_path !Semgrep_envvars.v.semgrep_url
           (pro_binary_route platform_kind))
        [ ("version", Version.version) ])
  in
  let headers = [ Auth.auth_header_of_token caps#token ] in
  Http_helpers.get ~headers caps#network uri

(* for semgrep show identity *)
let get_identity_async caps =
  let headers =
    [
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url identity_route in
  let%lwt res = Http_helpers.get ~headers caps#network url in
  match res with
  | Ok { body = Ok body; _ } -> Lwt.return body
  | Ok { body = Error msg; code; _ } ->
      Logs.warn (fun m ->
          m "Failed to download identity, %s returned %u: %s"
            (Uri.to_string url) code msg);
      Lwt.return ""
  | Error e ->
      Logs.warn (fun m ->
          m "Failed to download identity from %s: %s" (Uri.to_string url) e);
      Lwt.return ""

(* for semgrep publish *)
let upload_rule_to_registry_async caps json =
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url registry_rule_route in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let body = JSON.string_of_json (JSON.from_yojson json) in
  match%lwt Http_helpers.post ~body ~headers caps#network url with
  | Ok { body = Ok body; _ } -> Lwt.return_ok body
  | Ok { body = Error msg; code; _ } ->
      let msg =
        spf
          "Failed to upload rule to registry, API server returned %u, this \
           error: %s"
          code msg
      in
      Lwt.return_error msg
  | Error e -> Lwt.return_error (spf "Failed to upload rule to registry: %s" e)

let upload_rule_to_registry caps json =
  Lwt_platform.run (upload_rule_to_registry_async caps json)
