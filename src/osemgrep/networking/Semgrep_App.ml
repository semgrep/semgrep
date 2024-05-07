(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Gather Semgrep App (backend) related code.
 *
 * This module and directory should be the only places where we
 * call Http_helpers. This module provides an abstract and typed interface to
 * our Semgrep backend.
 * alt: maybe grpc was better than ATD for the CLI<->backend comms?
 *
 * This module (and Semgrep_login.ml) should be the only place where we use
 * !Semgrep_envvars.v.semgrep_url
 *
 * TODO? move some code in Auth.ml?
 *
 * Partially translated from auth.py and scans.py.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* LATER: declared this in semgrep_output_v1.atd instead? *)
type scan_id = string
type app_block_override = string (* reason *) option
type pro_engine_arch = Osx_arm64 | Osx_x86_64 | Manylinux_x86_64

(*****************************************************************************)
(* Routes *)
(*****************************************************************************)

let identity_route = "/api/agent/identity"
let deployment_route = "/api/agent/deployments/current"
let start_scan_route = "/api/agent/deployments/scans"
let registry_rule_route = "/api/registry/rules"

(* TODO: diff with api/agent/scans/<scan_id>/config? *)
let scan_config_route = "/api/agent/deployments/scans/config"
let results_route scan_id = "/api/agent/scans/" ^ scan_id ^ "/results"
let complete_route scan_id = "/api/agent/scans/" ^ scan_id ^ "/complete"

(*****************************************************************************)
(* Scan config version 1 (used by LS) *)
(*****************************************************************************)

(* Returns the scan config if the token is valid, otherwise None *)
let get_scan_config_from_token_async
    (caps : < Auth.cap_token ; Cap.network ; .. >) :
    OutJ.scan_config option Lwt.t =
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url scan_config_route in
  let headers = [ Auth.auth_header_of_token caps#token ] in
  let%lwt response = Http_helpers.get ~headers caps#network url in
  let scan_config_opt =
    match response with
    | Ok { body = Ok body; _ } -> (
        try Some (OutJ.scan_config_of_string body) with
        | Yojson.Json_error msg ->
            Logs.debug (fun m ->
                m "failed to parse body as scan_config %s: %s" msg body);
            None)
    | Ok { body = Error err; code; _ } ->
        Logs.debug (fun m ->
            m "error while retrieving scan config %u: %s" code err);
        None
    | Error e ->
        Logs.debug (fun m -> m "error while retrieving scan config %s" e);
        None
  in
  Lwt.return scan_config_opt

let get_scan_config_from_token caps =
  Lwt_platform.run (get_scan_config_from_token_async caps)

(*****************************************************************************)
(* Extractors *)
(*****************************************************************************)

(* TODO: specify as ATD the reply of api/agent/deployments/scans *)
let extract_scan_id (data : string) : (scan_id, string) result =
  try
    let json = JSON.json_of_string data in
    match json with
    | Object xs -> (
        match List.assoc_opt "scan" xs with
        | Some (Object dd) -> (
            match List.assoc_opt "id" dd with
            | Some (Int i) -> Ok (string_of_int i)
            | Some (String s) -> Ok s
            | _else ->
                Error
                  ("Bad json in body when looking for scan id: no id: " ^ data))
        | _else ->
            Error
              ("Bad json in body when trying to find scan id: no scan: " ^ data)
        )
    | _else -> Error ("Bad json in body when asking for scan id: " ^ data)
  with
  | Yojson.Json_error msg ->
      Error (spf "Couldn't parse json, err = %s, data was %s" msg data)

(* the server reply when POST to "/api/agent/scans/<scan_id>/results"  *)
let extract_errors (data : string) : string list =
  match OutJ.ci_scan_results_response_of_string data with
  | { errors; task_id = _ } as response ->
      Logs.debug (fun m ->
          m "results response = %s"
            (OutJ.show_ci_scan_results_response response));
      errors
      |> List_.map (fun (x : OutJ.ci_scan_results_response_error) -> x.message)
  | exception exn ->
      Logs.err (fun m ->
          m "Failed to decode server reply as json %s: %s"
            (Printexc.to_string exn) data);
      []

(* the server reply when POST to "/api/agent/scans/<scan_id>/complete" *)
let extract_block_override (data : string) : (app_block_override, string) result
    =
  match OutJ.ci_scan_complete_response_of_string data with
  | { success = _; app_block_override; app_block_reason } as response ->
      Logs.debug (fun m ->
          m "complete response = %s"
            (OutJ.show_ci_scan_complete_response response));
      if app_block_override then Ok (Some app_block_reason)
        (* TODO? can we have a app_block_reason set when override is false? *)
      else Ok None
  | exception exn ->
      Error
        (spf "Failed to decode server reply as json %s: %s"
           (Printexc.to_string exn) data)

(*****************************************************************************)
(* Step0: deployment config *)
(*****************************************************************************)

(* Returns the deployment config if the token is valid, otherwise None *)
let get_deployment_from_token_async caps : OutJ.deployment_config option Lwt.t =
  let headers =
    [
      ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url deployment_route in
  let%lwt response = Http_helpers.get ~headers caps#network url in
  let deployment_opt =
    match response with
    | Ok { body = Ok body; _ } ->
        let x = OutJ.deployment_response_of_string body in
        Some x.deployment
    | Ok { body = Error msg; code; _ } ->
        Logs.debug (fun m ->
            m "error while retrieving deployment, %s returned %u: %s"
              (Uri.to_string url) code msg);
        None
    | Error e ->
        Logs.debug (fun m -> m "error while retrieving deployment: %s" e);
        None
  in
  Lwt.return deployment_opt

(* from auth.py *)
let get_deployment_from_token token =
  Lwt_platform.run (get_deployment_from_token_async token)

(*****************************************************************************)
(* Step1 : start scan *)
(*****************************************************************************)

(* TODO: pass project_config *)
let start_scan_async ~dry_run caps (prj_meta : Project_metadata.t)
    (scan_meta : OutJ.scan_metadata) : (scan_id, string) result Lwt.t =
  if dry_run then (
    Logs.app (fun m -> m "Would have sent POST request to create scan");
    Lwt.return_ok "")
  else
    let headers =
      [
        ("Content-Type", "application/json");
        (* The agent is needed by many endpoints in our backend guarded by
         * @require_supported_cli_version()
         * alt: use Metrics_.string_of_user_agent()
         *)
        ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
        Auth.auth_header_of_token caps#token;
      ]
    in
    let url = Uri.with_path !Semgrep_envvars.v.semgrep_url start_scan_route in
    (* deprecated from 1.43 *)
    (* TODO: should concatenate with raw_json project_config *)
    let meta =
      (* ugly: would be good for ATDgen to generate also a json_of_xxx *)
      prj_meta |> OutJ.string_of_project_metadata |> Yojson.Basic.from_string
    in
    let request : OutJ.scan_request =
      {
        meta;
        scan_metadata = Some scan_meta;
        project_metadata = Some prj_meta;
        (* TODO *)
        project_config = None;
      }
    in
    let body = OutJ.string_of_scan_request request in
    let pretty_body =
      body |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string
    in
    Logs.debug (fun m -> m "Starting scan: %s" pretty_body);
    match%lwt Http_helpers.post ~body ~headers caps#network url with
    | Ok { body = Ok body; _ } -> Lwt.return (extract_scan_id body)
    | Ok { body = Error err; code; _ } ->
        let pre_msg =
          if code =|= 404 then
            {|Failed to create a scan with given token and deployment_id.
Please make sure they have been set correctly.
|}
          else ""
        in
        let msg =
          Fmt.str "%sAPI server at %a returned this error: %s" pre_msg Uri.pp
            url err
        in
        Lwt.return_error msg
    | Error e -> Lwt.return_error (spf "Failed to start scan: %s" e)

let start_scan ~dry_run caps prj_meta scan_meta =
  Lwt_platform.run (start_scan_async ~dry_run caps prj_meta scan_meta)

(*****************************************************************************)
(* Step2 : fetch scan config (version 2) *)
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

(* Returns a url with scan config encoded via search params based on a magic environment variable *)
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

let fetch_scan_config_string ~dry_run ~sca ~full_scan ~repository caps :
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
      ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
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

let fetch_scan_config_async ~dry_run ~sca ~full_scan ~repository caps :
    (OutJ.scan_config, string) result Lwt.t =
  let%lwt scan_config_string =
    fetch_scan_config_string ~dry_run ~sca ~full_scan ~repository caps
  in
  let scan_config_opt =
    Result.bind scan_config_string (fun c -> Ok (OutJ.scan_config_of_string c))
  in
  Lwt.return scan_config_opt

let fetch_scan_config ~dry_run ~sca ~full_scan ~repository caps =
  Lwt_platform.run
    (fetch_scan_config_async ~sca ~dry_run ~full_scan ~repository caps)

(*****************************************************************************)
(* Step3 : upload findings *)
(*****************************************************************************)

(* python: was called report_findings *)
let upload_findings_async ~dry_run ~scan_id ~results ~complete caps :
    (app_block_override, string) result Lwt.t =
  let results = OutJ.string_of_ci_scan_results results in
  let complete = OutJ.string_of_ci_scan_complete complete in
  if dry_run then (
    Logs.app (fun m ->
        m "Would have sent findings and ignores blob: %s" results);
    Logs.app (fun m -> m "Would have sent complete blob: %s" complete);
    Lwt.return_ok None)
  else (
    Logs.debug (fun m -> m "Sending findings and ignores blob: %s" results);
    Logs.debug (fun m -> m "Sending complete blob: %s" complete);

    let url =
      Uri.with_path !Semgrep_envvars.v.semgrep_url (results_route scan_id)
    in
    let headers =
      [
        ("Content-Type", "application/json");
        ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
        Auth.auth_header_of_token caps#token;
      ]
    in
    let body = results in
    let%lwt () =
      match%lwt Http_helpers.post ~body ~headers caps#network url with
      | Ok { body = Ok body; _ } ->
          let errors = extract_errors body in
          errors
          |> List.iter (fun s ->
                 Logs.warn (fun m ->
                     m "Server returned following warning: %s" s));
          Lwt.return_unit
      | Ok { body = Error msg; code; _ } ->
          Logs.warn (fun m ->
              m "API server returned %u, this error: %s" code msg);
          Lwt.return_unit
      | Error e ->
          Logs.warn (fun m -> m "Failed to upload findings: %s" e);
          Lwt.return_unit
    in
    (* mark as complete *)
    let url =
      Uri.with_path !Semgrep_envvars.v.semgrep_url (complete_route scan_id)
    in
    let body = complete in
    match%lwt Http_helpers.post ~body ~headers caps#network url with
    | Ok { body = Ok body; _ } -> Lwt.return (extract_block_override body)
    | Ok { body = Error msg; code; _ } ->
        let msg =
          Fmt.str
            "Failed to upload findings, API server returned %u, this error: %s"
            code msg
        in
        Lwt.return_error msg
    | Error e -> Lwt.return_error (spf "Failed to upload findings: %s" e))

let upload_findings ~dry_run ~scan_id ~results ~complete caps =
  Lwt_platform.run
    (upload_findings_async ~dry_run ~scan_id ~results ~complete caps)

(*****************************************************************************)
(* Installing Pro Engine *)
(*****************************************************************************)

let fetch_pro_binary caps platform_kind =
  let arch_str =
    match platform_kind with
    | Osx_arm64 -> "osx-arm64"
    | Osx_x86_64 -> "osx-x86"
    | Manylinux_x86_64 -> "manylinux"
  in
  let uri =
    Uri.(
      add_query_params'
        (with_path !Semgrep_envvars.v.semgrep_url
           (Common.spf "api/agent/deployments/deepbinary/%s" arch_str))
        [ ("version", Version.version) ])
  in
  let headers = [ Auth.auth_header_of_token caps#token ] in
  Http_helpers.get ~headers caps#network uri

(*****************************************************************************)
(* Error reporting to the backend *)
(*****************************************************************************)

(* report a failure for [scan_id] to Semgrep App *)
let report_failure_async ~dry_run ~scan_id caps (exit_code : Exit_code.t) :
    unit Lwt.t =
  let int_code = Exit_code.to_int exit_code in
  if dry_run then (
    Logs.app (fun m ->
        m "Would have reported failure to semgrep.dev: %u" int_code);
    Lwt.return_unit)
  else
    let headers =
      [
        ("Content-Type", "application/json");
        ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
        Auth.auth_header_of_token caps#token;
      ]
    in
    let url =
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/error")
    in
    let failure : OutJ.ci_scan_failure =
      { exit_code = int_code; (* TODO *)
                              stderr = "" }
    in
    let body = OutJ.string_of_ci_scan_failure failure in
    match%lwt Http_helpers.post ~body ~headers caps#network url with
    | Ok { body = Ok _; _ } -> Lwt.return_unit
    | Ok { body = Error msg; code; _ } ->
        Logs.warn (fun m -> m "API server returned %u, this error: %s" code msg);
        Lwt.return_unit
    | Error e ->
        Logs.warn (fun m -> m "Failed to report failure: %s" e);
        Lwt.return_unit

let report_failure ~dry_run ~scan_id caps exit_code =
  Lwt_platform.run (report_failure_async ~dry_run ~scan_id caps exit_code)

(*****************************************************************************)
(* Other endpoints *)
(*****************************************************************************)

(* for semgrep show identity *)
let get_identity_async caps =
  let headers =
    [
      ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
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
      ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
      Auth.auth_header_of_token caps#token;
    ]
  in
  let body = JSON.string_of_json (JSON.from_yojson json) in
  match%lwt Http_helpers.post ~body ~headers caps#network url with
  | Ok { body = Ok body; _ } -> Lwt.return_ok body
  | Ok { body = Error msg; code; _ } ->
      let msg =
        Fmt.str
          "Failed to upload rule to registry, API server returned %u, this \
           error: %s"
          code msg
      in
      Lwt.return_error msg
  | Error e -> Lwt.return_error (spf "Failed to upload rule to registry: %s" e)

let upload_rule_to_registry caps json =
  Lwt_platform.run (upload_rule_to_registry_async caps json)
