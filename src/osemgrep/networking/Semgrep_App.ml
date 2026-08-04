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
(* Gather code to communicate with the semgrep App backend.
 *
 * See semgrep_output_v1.atd section on "comms with the backend" to
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

(*****************************************************************************)
(* Routes *)
(*****************************************************************************)

(* routes used by semgrep ci
 * old: was "/api/agent/deployments/scans"
 *)
let start_scan_route = "/api/cli/scans"
let results_route scan_id = spf "/api/agent/scans/%d/results" scan_id
let complete_route scan_id = spf "/api/agent/scans/%d/complete" scan_id
let error_route scan_id = spf "/api/agent/scans/%d/error" scan_id

(* used by semgrep login and semgrep show deployment *)
let deployment_route = "/api/agent/deployments/current"

(* used by semgrep lsp
 * TODO: diff with api/agent/scans/<scan_id>/config?
 *)
let scan_config_route = "/api/agent/deployments/scans/config"

(* routes to be used by most of the semgrep commands
 * to kick off scans asynchronously and download the config
 * when it's ready
 *)
let start_scan_v2_route = "/api/cli/v2/scans"

let get_config_v2_route scan_request_id =
  spf "/api/cli/v2/scans/%s/config" scan_request_id

(* used by semgrep show identity *)
let identity_route = "/api/agent/identity"

(* used by semgrep publish *)
let registry_rule_route = "/api/registry/rules"

let symbol_analysis_route scan_id =
  spf "/api/agent/scans/%d/symbols_upload_url" scan_id

let subproject_symbol_analysis_route scan_id =
  spf "/api/agent/scans/%d/subproject_symbols_upload_url" scan_id

(* Transitive reachability caching routes *)
let tr_cache_route = "/api/cli/tr_cache"
let tr_cache_lookup_route = "/api/cli/tr_cache/lookup"

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
      |> List.map (fun (x : Out.ci_scan_results_response_error) -> x.message)
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
(* Step1 : start scan *)
(*****************************************************************************)

let start_scan_async token (request : Out.scan_request) :
    (Out.scan_response, string * Exit_code.t option) result Lwt.t =
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url start_scan_route in
  let body = Out.string_of_scan_request request in
  let pretty_body =
    body |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string
  in
  Logs.debug (fun m -> m "Starting scan: %s" pretty_body);
  let%lwt response = Http_helpers.post ~body ~headers url in
  let res =
    match response with
    | Ok { body = Ok body; _ } ->
        let x = Out.scan_response_of_string body in
        Ok x
    | Ok { body = Error err; code; _ } ->
        let pre_msg, exit_code_opt =
          match code with
          | 401 ->
              ( "API token not valid. Try to run `semgrep logout` and `semgrep \
                 login` again. Or in CI, ensure your SEMGREP_APP_TOKEN \
                 variable is set correctly.",
                Some (Exit_code.invalid_api_key ~__LOC__) )
          | 404 ->
              ( {|Failed to create a scan with given token and deployment_id.
Please make sure they have been set correctly.
|},
                None )
          | _else_ -> ("", None)
        in
        let msg =
          spf "%sAPI server at %s returned this error: %s" pre_msg
            (Uri.to_string url) err
        in
        Error (msg, exit_code_opt)
    | Error e -> Error (spf "Failed to start scan: %s" e, None)
  in
  Lwt.return res

let start_scan token request = Lwt_platform.run (start_scan_async token request)

(*****************************************************************************)
(* Step2 : upload findings *)
(*****************************************************************************)

(* python: was called report_findings *)
let upload_findings_async token ~scan_id ~results ~complete :
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
      Auth.auth_header_of_token token;
    ]
  in
  Logs.debug (fun m -> m "Sending findings and ignores blob");
  let body = results in
  let%lwt () =
    match%lwt Http_helpers.post ~body ~headers url with
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
  match%lwt Http_helpers.post ~body ~headers url with
  | Ok { body = Ok body; _ } -> Lwt.return (extract_block_override body)
  | Ok { body = Error msg; code; _ } ->
      let msg =
        spf "Failed to upload findings, API server returned %u, this error: %s"
          code msg
      in
      Lwt.return_error msg
  | Error e -> Lwt.return_error (spf "Failed to upload findings: %s" e)

let upload_findings token ~scan_id ~results ~complete =
  Lwt_platform.run (upload_findings_async token ~scan_id ~results ~complete)

(*****************************************************************************)
(* Error reporting to the backend *)
(*****************************************************************************)

(* report a failure for [scan_id] to Semgrep App *)
let report_failure_async token ~scan_id (exit_code : Exit_code.t) : unit Lwt.t =
  let int_code = Exit_code.to_int exit_code in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url (error_route scan_id)
  in
  let failure : Out.ci_scan_failure =
    {
      exit_code = int_code;
      (* TODO *)
      stderr = "";
    }
  in
  let body = Out.string_of_ci_scan_failure failure in
  match%lwt Http_helpers.post ~body ~headers url with
  | Ok { body = Ok _; _ } -> Lwt.return_unit
  | Ok { body = Error msg; code; _ } ->
      Logs.warn (fun m -> m "API server returned %u, this error: %s" code msg);
      Lwt.return_unit
  | Error e ->
      Logs.warn (fun m -> m "Failed to report failure: %s" e);
      Lwt.return_unit

let report_failure token ~scan_id exit_code =
  Lwt_platform.run (report_failure_async token ~scan_id exit_code)

(*****************************************************************************)
(* Other ways to fetch a config (deprecated?) *)
(*****************************************************************************)

(* coupling(eio-port): if you change this you must change the eio version *)
let deployment_config_result_async token :
    (Out.deployment_config, [ `Unauthorized | `Other of string ]) result Lwt.t =
  let headers =
    [
      (* The agent is needed by many endpoints in our backend guarded by
       * @require_supported_cli_version()
       * alt: use Metrics_.string_of_user_agent()
       *)
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url deployment_route in
  let%lwt response = Http_helpers.get ~headers url in
  Lwt.return
    (match response with
    | Ok { body = Ok body; _ } ->
        let x = Out.deployment_response_of_string body in
        Ok x.deployment
    | Ok { body = Error msg; response = _; code } ->
        Logs.err (fun m ->
            m "error while retrieving deployment, %s returned %u: %s"
              (Uri.to_string url) code msg);
        if code =|= 401 then Error `Unauthorized
        else Error (`Other (spf "HTTP %u: %s" code msg))
    | Error e ->
        Logs.err (fun m -> m "error while retrieving deployment: %s" e);
        Error (`Other e))

(* coupling(eio-port): if you change this you must change the lwt version *)
let deployment_config_result_eio token :
    (Out.deployment_config, [ `Unauthorized | `Other of string ]) result =
  let headers =
    [
      (* The agent is needed by many endpoints in our backend guarded by
       * @require_supported_cli_version()
       * alt: use Metrics_.string_of_user_agent()
       *)
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url deployment_route in
  let response = Http_helpers.get_eio ~headers url in
  match response with
  | Ok { body = Ok body; _ } ->
      let x = Out.deployment_response_of_string body in
      Ok x.deployment
  | Ok { body = Error msg; code; _ } ->
      Logs.err (fun m ->
          m "error while retrieving deployment, %s returned %u: %s"
            (Uri.to_string url) code msg);
      if code =|= 401 then Error `Unauthorized
      else Error (`Other (spf "HTTP %u: %s" code msg))
  | Error e ->
      Logs.err (fun m -> m "error while retrieving deployment: %s" e);
      Error (`Other e)

(* Returns the deployment config if the token is valid, otherwise None.
 * This is mostly used by 'semgrep login' to sanity check whether the
 * token is valid before saving it.
 * old: this endpoint used to be one of the three HTTP requests of 'semgrep ci'
 * to start a scan but now everything is done in one via start_scan().
 * pysemgrep: called get_deployment_from_token
 *)
(* coupling(eio-port): if you change this you must change the eio version *)
let deployment_config_async token : Out.deployment_config option Lwt.t =
  let%lwt result = deployment_config_result_async token in
  Lwt.return (Result.to_option result)

(* coupling(eio-port): if you change this you must change the lwt version *)
let deployment_config_eio token : Out.deployment_config option =
  Result.to_option (deployment_config_result_eio token)

(* from auth.py *)
let deployment_config token = Lwt_platform.run (deployment_config_async token)

(* used by policy and MCP hooks *)
let scan_config_uri ?(secrets = false) ?(sca = false) ?(dry_run = true)
    ?(full_scan = true) repo_name =
  let json_bool_to_string b = JSON.(string_of_json (Bool b)) in
  let query_params =
    [
      ("is_secrets_scan", json_bool_to_string secrets);
      ("sca", json_bool_to_string sca);
      ("dry_run", json_bool_to_string dry_run);
      ("full_scan", json_bool_to_string full_scan);
      ("semgrep_version", Version.version);
      (* We will always fetch code rules for now. This may change in the future,
         e.g. if we want to run a supply-chain-only scan, but for most purposes of
         Semgrep scanning, code rules are implied.
         Because the App requires that we specify some product positively, this
         also ensures that `sca: false` works properly.
       *)
      ("is_code_scan", json_bool_to_string true);
    ]
    @ if repo_name = "" then [] else [ ("repo_name", repo_name) ]
  in
  Uri.(
    add_query_params'
      (with_path !Semgrep_envvars.v.semgrep_url scan_config_route)
      query_params)

(* Returns a url with scan config encoded via search params based on a magic
 * environment variable *)
let url_for_policy ?(from_hooks = false) token =
  let depl_config_opt = deployment_config token in
  match depl_config_opt with
  | None ->
      Error.abort
        (spf "Invalid API Key. Run `semgrep logout` and `semgrep login` again.")
  | Some _deployment_config -> (
      if
        (* NOTE: This logic is ported directly from python but seems very brittle
         as we have helper functions to infer the repo name from the git remote
         information. *)
        from_hooks
      then scan_config_uri ""
      else
        match Sys.getenv_opt "SEMGREP_REPO_NAME" with
        | None ->
            Error.abort
              (spf
                 "Need to set env var SEMGREP_REPO_NAME to use `--config \
                  policy`")
        | Some repo_name -> scan_config_uri repo_name)

(* coupling(eio-port): if you change this you must change the eio version *)
let fetch_scan_config_string_async ~dry_run ~secrets ~sca ~full_scan ~repository
    token : (string, string) result Lwt.t =
  (* TODO? seems like there are 2 ways to get a config, with the scan_params
   * or with a scan_id.
   * python:
   *   if self.dry_run:
   *    app_get_config_url = f"{state.env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{self._scan_params}"
   *   else:
   *    app_get_config_url = f"{state.env.semgrep_url}/api/agent/deployments/scans/{self.scan_id}/config"
   *)
  Metrics_.add_feature ~category:"config_download" ~name:"legacy_config_lwt";
  let url = scan_config_uri ~secrets ~sca ~dry_run ~full_scan repository in
  let headers =
    [
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let%lwt conf_string =
    let%lwt response = Http_helpers.get ~headers url in
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

(* coupling(eio-port): if you change this you must change the eio version *)
let fetch_scan_config_string_eio ~dry_run ~secrets ~sca ~full_scan ~repository
    token : (string, string) result =
  (* TODO? seems like there are 2 ways to get a config, with the scan_params
   * or with a scan_id.
   * python:
   *   if self.dry_run:
   *    app_get_config_url = f"{state.env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{self._scan_params}"
   *   else:
   *    app_get_config_url = f"{state.env.semgrep_url}/api/agent/deployments/scans/{self.scan_id}/config"
   *)
  Metrics_.add_feature ~category:"config_download" ~name:"legacy_config_eio";
  let url = scan_config_uri ~secrets ~sca ~dry_run ~full_scan repository in
  let headers =
    [
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let conf_string =
    let response = Http_helpers.get_eio ~headers url in
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
    results
  in
  Logs.debug (fun m -> m "finished downloading from %s" (Uri.to_string url));
  conf_string
[@@trace]

(*****************************************************************************)
(* Scan config v2 (start-then-poll) *)
(*****************************************************************************)

(* Builds a minimal v2 scan request and returns it
 * together with the client-generated scan_request_id used for polling. *)
let make_scan_request_v2 ?(secrets = false) ?(sca = false) () :
    Out.create_scan_request_v2 * string =
  let unique_id = Uuidm.v4_gen (Stdlib.Random.State.make_self_init ()) () in
  let scan_request_id = Uuidm.to_string unique_id in
  (* Mirror legacy product selection: always request SAST and conditionally add
   * Secrets/SCA based on caller flags. The server returns the intersection with
   * the deployment's available_products, so requesting an unlicensed product is
   * safe (no 400 as long as SAST is available). *)
  let requested_products =
    [ `SAST ]
    @ (if secrets then [ `Secrets ] else [])
    @ if sca then [ `SCA ] else []
  in
  let request : Out.create_scan_request_v2 =
    {
      project_metadata =
        {
          scan_environment = "config-generation";
          (* Repository identity fields: omitted so the server falls back to
           * deployment-level config rather than a specific repo's config. *)
          repository = "";
          project_id = None;
          repo_url = None;
          repo_id = None;
          org_id = None;
          repo_display_name = None;
          (* Git commit context: not available outside a CI environment.
           * These fields are used by the server to record scan history and
           * to compute merge-base for diff scans, neither of which applies
           * to a config-only fetch. *)
          branch = None;
          commit = None;
          commit_title = None;
          commit_timestamp = None;
          commit_author_email = None;
          commit_author_name = None;
          commit_author_username = None;
          commit_author_image_url = None;
          (* CI and pull-request context: not present outside a CI pipeline.
           * ci_job_url and on describe the CI environment; the PR fields
           * describe the triggering pull request. None of these are relevant
           * to a config-only fetch. *)
          ci_job_url = None;
          on = "unknown";
          pull_request_author_username = None;
          pull_request_author_image_url = None;
          pull_request_id = None;
          pull_request_title = None;
          (* Diff-scan fields: not relevant for config generation.
           * So is_full_scan is always true. base_branch_head_commit, base_sha,
           * and start_sha are only meaningful for incremental diff scans. *)
          base_branch_head_commit = None;
          base_sha = None;
          start_sha = None;
          is_full_scan = true;
          (* Deprecated product flags: the v2 server ignores these in favour of
           * scan_metadata.requested_products. All set to None. *)
          is_sca_scan = None;
          is_code_scan = None;
          is_secrets_scan = None;
        };
      scan_metadata =
        {
          cli_version = Version.version;
          unique_id;
          requested_products;
          (* Always a dry run: the purpose of calling this endpoint is to fetch the
           * config, not to persist a scan record. *)
          dry_run = true;
          sms_scan_id = None;
          ecosystems = [];
          packages = [];
          enable_mal_deps = None;
          partial_scan_rule_ids = None;
        };
      project_config = None;
    }
  in
  (request, scan_request_id)

type poll_outcome =
  | Poll_success of Out.scan_configuration
  | Poll_failure of string
  | Poll_pending of float (* server-provided poll interval in seconds *)

(* Parses a poll response body, updates the mutable deadline/interval refs
 * from any server-provided [polling] hint, and returns a poll_outcome. *)
let handle_poll_response ~scan_request_id ~server_deadline ~server_poll_interval
    body : (poll_outcome, string) result =
  let parsed =
    match Out.get_config_response_v2_of_string body with
    | exception exn ->
        Error
          (spf "Failed to parse v2 scan config response: %s"
             (Printexc.to_string exn))
    | r -> Ok r
  in
  match parsed with
  | Error _ as e -> e
  | Ok { status; polling; config; _ } -> (
      (match polling with
      | Some { recommended_wait_seconds; seconds_until_timeout } ->
          server_poll_interval := Float.of_int recommended_wait_seconds;
          server_deadline :=
            Unix.gettimeofday () +. Float.of_int seconds_until_timeout
      | None -> ());
      match (status, config) with
      | Failure, _ ->
          Ok
            (Poll_failure
               (spf "v2 scan config generation failed (scan_request_id=%s)"
                  scan_request_id))
      | Pending, _ -> Ok (Poll_pending !server_poll_interval)
      | Success, None ->
          Error "v2 scan config status is Success but config is missing"
      | Success, Some sc ->
          Logs.debug (fun m ->
              m "Received v2 scan config (scan_request_id=%s)" scan_request_id);
          Ok (Poll_success sc))

(* coupling(backend): if you change this you must change start_scan_v2 in scans.py *)
(* coupling(eio-port): if you change this you must change poll_scan_config_v2_eio *)
(* poll for the v2 scan config after the scan has been created *)
let poll_scan_config_v2_async ~scan_request_id ~headers :
    (Out.scan_configuration, string) result Lwt.t =
  let get_config_url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url
      (get_config_v2_route scan_request_id)
  in
  let start_time = Unix.gettimeofday () in
  (* informed bounds overridable by the server *)
  let server_deadline = ref (start_time +. 180.) in
  let server_poll_interval = ref 5. in
  (* hard bounds for safety *)
  let maximum_deadline = start_time +. 300. in
  let minimum_poll_interval = 1. in
  let maximum_poll_interval = 60. in
  let poll_attempts = ref 0 in
  let rec poll () =
    if Unix.gettimeofday () >= Float.min !server_deadline maximum_deadline then
      let elapsed = Unix.gettimeofday () -. start_time in
      Lwt.return_error
        (spf
           "Config generation timed out after %.0f seconds \
            (scan_request_id=%s, %d attempts)"
           elapsed scan_request_id !poll_attempts)
    else begin
      incr poll_attempts;
      match%lwt Http_helpers.get ~headers get_config_url with
      | Error e -> Lwt.return_error (spf "Failed to poll v2 scan config: %s" e)
      | Ok { body = Error msg; code; _ } ->
          Lwt.return_error (spf "v2 scan config poll returned %u: %s" code msg)
      | Ok { body = Ok body; _ } -> (
          match
            handle_poll_response ~scan_request_id ~server_deadline
              ~server_poll_interval body
          with
          | Error e -> Lwt.return_error e
          | Ok (Poll_success sc) -> Lwt.return_ok sc
          | Ok (Poll_failure msg) -> Lwt.return_error msg
          | Ok (Poll_pending interval) ->
              (* Never wait less than minimum poll interval to avoid hammering
               * the server *)
              let wait =
                Float.min
                  (Float.max interval minimum_poll_interval)
                  maximum_poll_interval
              in
              let%lwt () = Lwt_platform.sleep wait in
              poll ())
    end
  in
  poll ()

(* coupling(eio-port): if you change this you must change poll_scan_config_v2_async *)
(* poll for the v2 scan config after the scan has been created *)
let poll_scan_config_v2_eio ~scan_request_id ~headers :
    (Out.scan_configuration, string) result =
  let get_config_url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url
      (get_config_v2_route scan_request_id)
  in
  let start_time = Unix.gettimeofday () in
  (* informed bounds overridable by the server *)
  let server_deadline = ref (start_time +. 180.) in
  let server_poll_interval = ref 5. in
  (* hard bounds for safety *)
  let maximum_deadline = start_time +. 300. in
  let minimum_poll_interval = 1. in
  let maximum_poll_interval = 60. in
  let poll_attempts = ref 0 in
  let rec poll () =
    if Unix.gettimeofday () >= Float.min !server_deadline maximum_deadline then
      let elapsed = Unix.gettimeofday () -. start_time in
      Error
        (spf
           "Config generation timed out after %.0f seconds \
            (scan_request_id=%s, %d attempts)"
           elapsed scan_request_id !poll_attempts)
    else begin
      incr poll_attempts;
      match Http_helpers.get_eio ~headers get_config_url with
      | Error e -> Error (spf "Failed to poll v2 scan config: %s" e)
      | Ok { body = Error msg; code; _ } ->
          Error (spf "v2 scan config poll returned %u: %s" code msg)
      | Ok { body = Ok body; _ } -> (
          match
            handle_poll_response ~scan_request_id ~server_deadline
              ~server_poll_interval body
          with
          | Error e -> Error e
          | Ok (Poll_success sc) -> Ok sc
          | Ok (Poll_failure msg) -> Error msg
          | Ok (Poll_pending interval) ->
              (* Never wait less than minimum poll interval to avoid hammering
               * the server *)
              let wait =
                Float.min
                  (Float.max interval minimum_poll_interval)
                  maximum_poll_interval
              in
              Unix.sleepf wait;
              poll ())
    end
  in
  poll ()

(* coupling(eio-port): if you change this you must change fetch_scan_config_v2_eio *)
(* Fetch a config using the v2 endpoints *)
let fetch_scan_config_v2_async ?(secrets = false) ?(sca = false) token :
    (Out.scan_configuration, string) result Lwt.t =
  Metrics_.add_feature ~category:"config_download" ~name:"v2_config_lwt";
  let request, scan_request_id = make_scan_request_v2 ~secrets ~sca () in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let start_scan_url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url start_scan_v2_route
  in
  let request_body = Out.string_of_create_scan_request_v2 request in
  Logs.debug (fun m ->
      m "Starting v2 scan config fetch (scan_request_id=%s)" scan_request_id);
  match%lwt Http_helpers.post ~body:request_body ~headers start_scan_url with
  | Error e -> Lwt.return_error (spf "Failed to create v2 scan: %s" e)
  | Ok { body = Error msg; code; _ } ->
      Lwt.return_error
        (spf "Failed to create v2 scan, server returned %u: %s" code msg)
  | Ok { body = Ok body; _ } ->
      (match Out.create_scan_response_v2_of_string body with
      | { info = { id; deployment_id; deployment_name; _ } } ->
          Logs.debug (fun m ->
              m
                "v2 scan created: scan_id=%s deployment_id=%d \
                 deployment_name=%s"
                (Option.fold ~none:"null" ~some:string_of_int id)
                deployment_id deployment_name)
      | exception _ -> ());
      poll_scan_config_v2_async ~scan_request_id ~headers

(* coupling(eio-port): if you change this you must change the lwt version *)
(* Fetch a config using the v2 endpoints *)
let fetch_scan_config_v2_eio ?(secrets = false) ?(sca = false) token :
    (Out.scan_configuration, string) result =
  Metrics_.add_feature ~category:"config_download" ~name:"v2_config_eio";
  let request, scan_request_id = make_scan_request_v2 ~secrets ~sca () in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let start_scan_url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url start_scan_v2_route
  in
  let request_body = Out.string_of_create_scan_request_v2 request in
  Logs.debug (fun m ->
      m "Starting v2 scan config fetch (scan_request_id=%s)" scan_request_id);
  match Http_helpers.post_eio ~body:request_body ~headers start_scan_url with
  | Error e -> Error (spf "Failed to create v2 scan: %s" e)
  | Ok { body = Error msg; code; _ } ->
      Error (spf "Failed to create v2 scan, server returned %u: %s" code msg)
  | Ok { body = Ok body; _ } ->
      (match Out.create_scan_response_v2_of_string body with
      | { info = { id; deployment_id; deployment_name; _ } } ->
          Logs.debug (fun m ->
              m
                "v2 scan created: scan_id=%s deployment_id=%d \
                 deployment_name=%s"
                (Option.fold ~none:"null" ~some:string_of_int id)
                deployment_id deployment_name)
      | exception _ -> ());
      poll_scan_config_v2_eio ~scan_request_id ~headers
[@@trace]

(*****************************************************************************)
(* Other endpoints *)
(*****************************************************************************)

(* Query the TR cache for matches *)
let query_tr_cache_async token (request : Out.tr_query_cache_request) :
    (Out.tr_query_cache_response, string) result Lwt.t =
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url tr_cache_lookup_route
  in
  let body = Out.string_of_tr_query_cache_request request in

  match%lwt Http_helpers.post ~body ~headers url with
  | Ok { body = Ok body; _ } -> (
      try
        let response = Out.tr_query_cache_response_of_string body in
        Lwt.return_ok response
      with
      | exn ->
          Lwt.return_error
            (spf "Failed to parse cache response: %s" (Printexc.to_string exn)))
  | Ok { body = Error msg; code; _ } ->
      Lwt.return_error
        (spf "Failed to query TR cache, API server returned %u: %s" code msg)
  | Error e -> Lwt.return_error (spf "Failed to query TR cache: %s" e)

(* Add entries to the TR cache *)
let add_to_tr_cache_async token (request : Out.tr_add_cache_request) :
    (unit, string) result Lwt.t =
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url tr_cache_route in
  let body = Out.string_of_tr_add_cache_request request in

  match%lwt Http_helpers.post ~body ~headers url with
  | Ok { body = Ok _; _ } -> Lwt.return_ok ()
  | Ok { body = Error msg; code; _ } ->
      Lwt.return_error
        (spf "Failed to add to TR cache, API server returned %u: %s" code msg)
  | Error e -> Lwt.return_error (spf "Failed to add to TR cache: %s" e)

let query_tr_cache token request =
  Lwt_platform.run (query_tr_cache_async token request)

let add_to_tr_cache token request =
  Lwt_platform.run (add_to_tr_cache_async token request)

(* for semgrep show identity *)
let get_identity_async token =
  let headers =
    [
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url identity_route in
  let%lwt res = Http_helpers.get ~headers url in
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
let upload_rule_to_registry_async token json =
  let url = Uri.with_path !Semgrep_envvars.v.semgrep_url registry_rule_route in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let body = JSON.string_of_json (JSON.from_yojson json) in
  match%lwt Http_helpers.post ~body ~headers url with
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

let upload_rule_to_registry token json =
  Lwt_platform.run (upload_rule_to_registry_async token json)

(* TODO: (2025-12-11) we can remove this once we fully switch over to
   the new style of per-subproject symbol analysis *)
let get_symbol_analysis_s3_url ~token ~scan_id : (Uri.t, string) result Lwt.t =
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url (symbol_analysis_route scan_id)
  in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  match%lwt Http_helpers.get ~headers url with
  | Ok { body = Ok body_json; _ } -> (
      (* Parse the JSON to extract the upload_url *)
      match Out.symbol_analysis_upload_response_of_string body_json with
      | { upload_url } -> Lwt.return_ok upload_url)
  | Ok { body = Error msg; code; _ } ->
      let msg =
        spf
          "Failed to get symbol analysis upload url, API server returned %u, \
           this error: %s"
          code msg
      in
      Lwt.return_error msg
      (* Handle the case where the server rejects the connection *)
  | Error e ->
      let msg = spf "Failed to get symbol analysis upload url: %s" e in
      Lwt.return_error msg

let get_subproject_symbol_analysis_s3_url ~token ~scan_id ~manifest ~lockfile :
    (Uri.t, string) result Lwt.t =
  let url =
    Uri.with_path !Semgrep_envvars.v.semgrep_url
      (subproject_symbol_analysis_route scan_id)
  in
  let headers =
    [
      ("Content-Type", "application/json");
      ("User-Agent", spf "Semgrep/%s" Version.version);
      Auth.auth_header_of_token token;
    ]
  in
  let body =
    Out.string_of_subproject_symbol_analysis_url_request
      { manifest_path = manifest; lockfile_path = lockfile }
  in
  match%lwt Http_helpers.post ~body ~headers url with
  | Ok { body = Ok body_json; _ } -> (
      (* Parse the JSON to extract the upload_url *)
      match Out.symbol_analysis_upload_response_of_string body_json with
      | { upload_url } -> Lwt.return_ok upload_url)
  | Ok { body = Error msg; code; _ } ->
      let msg =
        spf
          "Failed to get subproject symbol analysis upload url, API server \
           returned %u, this error: %s"
          code msg
      in
      Lwt.return_error msg
      (* Handle the case where the server rejects the connection *)
  | Error e ->
      let msg =
        spf "Failed to get subproject symbol analysis upload url: %s" e
      in
      Lwt.return_error msg

let upload_symbol_analysis_to_s3 ~upload_url symbol_analysis :
    (string, string) result Lwt.t =
  try
    Logs.debug (fun m ->
        m "Uploading symbol analysis for %d symbols"
          (List.length symbol_analysis));
    let upload_headers =
      [
        ("Content-Type", "application/json");
        ("User-Agent", spf "Semgrep/%s" Version.version);
      ]
    in
    let body = Out.string_of_symbol_analysis symbol_analysis in
    (* Upload the symbol analysis to the S3 upload_url *)
    match%lwt Http_helpers.put ~body ~headers:upload_headers upload_url with
    (* Handle the good case *)
    | Ok { body = Ok _; _ } -> Lwt.return_ok "Symbol analysis uploaded"
    | Ok { body = Error msg; code; _ } ->
        (* Handle the case where the server returns an error code*)
        let msg =
          spf
            "Failed to upload symbol analysis to S3, S3 returned %u, this \
             error: %s"
            code msg
        in
        Lwt.return_error msg
    | Error e ->
        (* Handle the case where the server rejects the connection *)
        let msg = spf "Failed to upload symbol analysis to S3: %s" e in
        Lwt.return_error msg
  with
  | exn ->
      let msg =
        spf
          "Got show-stopping exception %s while trying to upload symbol \
           analysis."
          (Printexc.to_string exn)
      in
      Lwt.return_error msg

(* TODO: (2025-12-11) we can remove this once we fully switch over to
   the new style of per-subproject symbol analysis *)
let upload_symbol_analysis_async ~token ~scan_id symbol_analysis :
    (string, string) result Lwt.t =
  match%lwt get_symbol_analysis_s3_url ~token ~scan_id with
  | Ok upload_url -> upload_symbol_analysis_to_s3 ~upload_url symbol_analysis
  | Error msg -> Lwt.return_error msg

let upload_subproject_symbol_analysis_async ~token ~scan_id ~manifest ~lockfile
    symbol_analysis : (string, string) result Lwt.t =
  match%lwt
    get_subproject_symbol_analysis_s3_url ~token ~scan_id ~manifest ~lockfile
  with
  | Ok upload_url -> upload_symbol_analysis_to_s3 ~upload_url symbol_analysis
  | Error msg -> Lwt.return_error msg

let upload_symbol_analysis ~token ~scan_id symbol_analysis =
  Lwt_platform.run
    (upload_symbol_analysis_async ~token ~scan_id symbol_analysis)

let upload_subproject_symbol_analysis ~token ~scan_id ~manifest ~lockfile
    symbol_analysis =
  Lwt_platform.run
    (upload_subproject_symbol_analysis_async ~token ~scan_id ~manifest ~lockfile
       symbol_analysis)
