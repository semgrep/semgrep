open Common
module Out = Semgrep_output_v1_j
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Gather Semgrep App (backend) related code.
 *
 * TODO? split some code in Auth.ml?
 *
 * Partially translated from auth.py and scans.py.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* LATER: declared this in semgrep_output_v1.atd instead? *)
type scan_id = string
type app_block_override = string (* reason *) option

(*****************************************************************************)
(* Routes *)
(*****************************************************************************)

let deployment_route = "/api/agent/deployments/current"
let start_scan_route = "/api/agent/deployments/scans"

(* TODO: diff with api/agent/scans/<scan_id>/config? *)
let scan_config_route = "/api/agent/deployments/scans/config"
let results_route scan_id = "/api/agent/scans/" ^ scan_id ^ "/results"
let complete_route scan_id = "/api/agent/scans/" ^ scan_id ^ "/complete"

(*****************************************************************************)
(* Scan config version 1 (used by LS) *)
(*****************************************************************************)

(* Returns the scan config if the token is valid, otherwise None *)
let get_scan_config_from_token_async ~token : Out.scan_config option Lwt.t =
  let%lwt response =
    Http_helpers.get_async
      ~headers:[ ("authorization", "Bearer " ^ token) ]
      (Uri.with_path !Semgrep_envvars.v.semgrep_url scan_config_route)
  in
  let scan_config_opt =
    match response with
    | Error msg ->
        Logs.debug (fun m -> m "error while retrieving scan config: %s" msg);
        None
    | Ok body -> (
        try Some (Out.scan_config_of_string body) with
        | Yojson.Json_error msg ->
            Logs.debug (fun m ->
                m "failed to parse body as scan_config %s: %s" msg body);
            None)
  in
  Lwt.return scan_config_opt

let get_scan_config_from_token ~token =
  Lwt_platform.run (get_scan_config_from_token_async ~token)

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
  | e ->
      Error ("Couldn't parse json, error: " ^ Printexc.to_string e ^ ": " ^ data)

(* the server reply when POST to "/api/agent/scans/<scan_id>/results"  *)
let extract_errors (data : string) : string list =
  match Out.ci_scan_results_response_of_string data with
  | { errors; task_id = _ } as response ->
      Logs.debug (fun m ->
          m "results response = %s" (Out.show_ci_scan_results_response response));
      errors
      |> Common.map (fun (x : Out.ci_scan_results_response_error) -> x.message)
  | exception exn ->
      Logs.err (fun m ->
          m "Failed to decode server reply as json %s: %s"
            (Printexc.to_string exn) data);
      []

(* the server reply when POST to "/api/agent/scans/<scan_id>/complete" *)
let extract_block_override (data : string) : (app_block_override, string) result
    =
  match Out.ci_scan_complete_response_of_string data with
  | { success = _; app_block_override; app_block_reason } as response ->
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
(* Step0: deployment config *)
(*****************************************************************************)

(* Returns the deployment config if the token is valid, otherwise None *)
let get_deployment_from_token_async ~token : Out.deployment_config option Lwt.t
    =
  let headers = [ ("authorization", "Bearer " ^ token) ] in
  let%lwt response =
    Http_helpers.get_async ~headers
      (Uri.with_path !Semgrep_envvars.v.semgrep_url deployment_route)
  in
  let deployment_opt =
    match response with
    | Error msg ->
        Logs.debug (fun m -> m "error while retrieving deployment: %s" msg);
        None
    | Ok body ->
        let x = Out.deployment_response_of_string body in
        Some x.deployment
  in
  Lwt.return deployment_opt

(* from auth.py *)
let get_deployment_from_token ~token =
  Lwt_platform.run (get_deployment_from_token_async ~token)

(*****************************************************************************)
(* Step1 : start scan *)
(*****************************************************************************)

(* TODO: pass project_config *)
let start_scan ~dry_run ~token (prj_meta : Project_metadata.t)
    (scan_meta : Out.scan_metadata) : (scan_id, string) result =
  if dry_run then (
    Logs.app (fun m -> m "Would have sent POST request to create scan");
    Ok "")
  else
    let headers =
      [
        ("Content-Type", "application/json");
        (* The agent is needed by many endpoints in our backend guarded by
         * @require_supported_cli_version()
         * alt: use Metrics_.string_of_user_agent()
         *)
        ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
        ("Authorization", "Bearer " ^ token);
      ]
    in
    let scan_endpoint =
      Uri.with_path !Semgrep_envvars.v.semgrep_url start_scan_route
    in
    (* deprecated from 1.43 *)
    (* TODO: should concatenate with raw_json project_config *)
    let meta =
      (* ugly: would be good for ATDgen to generate also a json_of_xxx *)
      prj_meta |> Out.string_of_project_metadata |> Yojson.Basic.from_string
    in
    let request : Out.scan_request =
      {
        meta;
        scan_metadata = Some scan_meta;
        project_metadata = Some prj_meta;
        (* TODO *)
        project_config = None;
      }
    in
    let body = Out.string_of_scan_request request in
    let pretty_body =
      body |> Yojson.Basic.from_string |> Yojson.Basic.pretty_to_string
    in
    Logs.debug (fun m -> m "Starting scan: %s" pretty_body);
    match Http_helpers.post ~body ~headers scan_endpoint with
    | Ok body -> extract_scan_id body
    | Error (status, msg) ->
        let pre_msg =
          if status =|= 404 then
            {|Failed to create a scan with given token and deployment_id.
Please make sure they have been set correctly.
|}
          else ""
        in
        let msg =
          Fmt.str "%sAPI server at %a returned this error: %s" pre_msg Uri.pp
            scan_endpoint msg
        in
        Error msg

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

let fetch_scan_config_async ~dry_run ~token ~sca ~full_scan ~repository :
    (Out.scan_config, string) result Lwt.t =
  (* TODO? seems like there are 2 ways to get a config, with the scan_params
   * or with a scan_id.
   * python:
   *   if self.dry_run:
   *    app_get_config_url = f"{state.env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{self._scan_params}"
   *   else:
   *    app_get_config_url = f"{state.env.semgrep_url}/api/agent/deployments/scans/{self.scan_id}/config"
   *)
  let url = scan_config_uri ~sca ~dry_run ~full_scan repository in
  let%lwt content =
    let headers =
      [
        ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
        ("Authorization", Fmt.str "Bearer %s" token);
      ]
    in
    let%lwt response = Http_helpers.get_async ~headers url in
    let results =
      match response with
      | Ok _ as r -> r
      | Error msg ->
          Error
            (Printf.sprintf "Failed to download config from %s: %s"
               (Uri.to_string url) msg)
    in
    Lwt.return results
  in
  Logs.debug (fun m -> m "finished downloading from %s" (Uri.to_string url));
  (* TODO? use Result.map? or a let*? *)
  let conf =
    match content with
    | Error _ as e -> e
    | Ok content -> Ok (Out.scan_config_of_string content)
  in
  Lwt.return conf

let fetch_scan_config ~dry_run ~token ~sca ~full_scan ~repository =
  Lwt_platform.run
    (fetch_scan_config_async ~token ~sca ~dry_run ~full_scan ~repository)

(*****************************************************************************)
(* Step3 : upload findings *)
(*****************************************************************************)

(* python: was called report_findings *)
let upload_findings ~dry_run ~token ~scan_id ~results ~complete :
    (app_block_override, string) result =
  let results = Out.string_of_ci_scan_results results in
  let complete = Out.string_of_ci_scan_complete complete in
  if dry_run then (
    Logs.app (fun m ->
        m "Would have sent findings and ignores blob: %s" results);
    Logs.app (fun m -> m "Would have sent complete blob: %s" complete);
    Ok None)
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
        ("Authorization", "Bearer " ^ token);
      ]
    in
    let body = results in
    (match Http_helpers.post ~body ~headers url with
    | Ok body ->
        let errors = extract_errors body in
        errors
        |> List.iter (fun s ->
               Logs.warn (fun m -> m "Server returned following warning: %s" s))
    | Error (code, msg) ->
        Logs.warn (fun m -> m "API server returned %u, this error: %s" code msg));
    (* mark as complete *)
    let url =
      Uri.with_path !Semgrep_envvars.v.semgrep_url (complete_route scan_id)
    in
    let body = complete in
    match Http_helpers.post ~body ~headers url with
    | Ok body -> extract_block_override body
    | Error (code, msg) ->
        Error
          ("API server returned " ^ string_of_int code ^ ", this error: " ^ msg))

(*****************************************************************************)
(* Error reporting to the backend *)
(*****************************************************************************)

(* report a failure for [scan_id] to Semgrep App *)
let report_failure ~dry_run ~token ~scan_id (exit_code : Exit_code.t) : unit =
  let int_code = Exit_code.to_int exit_code in
  if dry_run then
    Logs.app (fun m ->
        m "Would have reported failure to semgrep.dev: %u" int_code)
  else
    let headers =
      [
        ("content-type", "application/json");
        ("authorization", "Bearer " ^ token);
      ]
    in
    let uri =
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/error")
    in
    let failure : Out.ci_scan_failure =
      { exit_code = int_code; (* TODO *)
                              stderr = "" }
    in
    let body = Out.string_of_ci_scan_failure failure in
    match Http_helpers.post ~body ~headers uri with
    | Ok _ -> ()
    | Error (code, msg) ->
        Logs.err (fun m -> m "API server returned %u, this error: %s" code msg)
