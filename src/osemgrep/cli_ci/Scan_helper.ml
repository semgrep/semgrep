(* translated from scans.py *)
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* LATER: declared this in semgrep_output_v1.atd instead? *)
type scan_id = string
type app_block_override = string (* reason *) option

(*****************************************************************************)
(* Extractors (should use ATD) *)
(*****************************************************************************)

(* TODO: specify as ATD the reply of api/agent/deployments/scans *)
let extract_scan_id data =
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

(* TODO the server reply when POST to
   "/api/agent/scans/<scan_id>/findings_and_ignores" should be specified ATD *)
let extract_errors data =
  try
    match JSON.json_of_string data with
    | JSON.Object xs -> (
        match List.assoc_opt "errors" xs with
        | Some (JSON.Array errs) ->
            List.iter
              (fun err ->
                match err with
                | JSON.Object xs -> (
                    match List.assoc_opt "message" xs with
                    | Some (String s) ->
                        Logs.warn (fun m ->
                            m "Server returned following warning: %s" s)
                    | _else ->
                        Logs.err (fun m ->
                            m "Couldn't find message in %s"
                              (JSON.string_of_json err)))
                | _else ->
                    Logs.err (fun m ->
                        m "Couldn't find message in %s"
                          (JSON.string_of_json err)))
              errs
        | _else ->
            Logs.err (fun m ->
                m "Couldn't find errors in %s"
                  (JSON.string_of_json (JSON.Object xs))))
    | json ->
        Logs.err (fun m -> m "Not a json object %s" (JSON.string_of_json json))
  with
  | e ->
      Logs.err (fun m ->
          m "Failed to decode server reply as json %s: %s"
            (Printexc.to_string e) data)

(* TODO the server reply when POST to
   "/api/agent/scans/<scan_id>/complete" should be specified in ATD
*)
let extract_block_override data : (app_block_override, string) result =
  try
    match JSON.json_of_string data with
    | JSON.Object xs ->
        let app_block_override =
          match List.assoc_opt "app_block_override" xs with
          | Some (Bool b) -> b
          | _else -> false
        and app_block_reason =
          match List.assoc_opt "app_block_reason" xs with
          | Some (String s) -> s
          | _else -> ""
        in
        if app_block_override then Ok (Some app_block_reason)
          (* TODO? can we have a app_block_reason set when override is false? *)
        else Ok None
    | json ->
        Error
          (Fmt.str "Failed to understand the server reply: %s"
             (JSON.string_of_json json))
  with
  | e ->
      Error
        (Fmt.str "Failed to decode server reply as json %s: %s"
           (Printexc.to_string e) data)

(*****************************************************************************)
(* Step1 : start scan *)
(*****************************************************************************)

(* TODO: pass project_config *)
let start_scan ~dry_run ~token (url : Uri.t) (prj_meta : Project_metadata.t)
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
    let scan_endpoint = Uri.with_path url "/api/agent/deployments/scans" in
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
          if status = 404 then
            {|Failed to create a scan with given token and deployment_id.
Please make sure they have been set correctly.
|}
          else ""
        in
        let msg =
          Fmt.str "%sAPI server at %a returned this error: %s" pre_msg Uri.pp
            url msg
        in
        Error msg

(*****************************************************************************)
(* Step2 : fetch scan config *)
(*****************************************************************************)

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
  let url = Semgrep_App.scan_config_uri ~sca ~dry_run ~full_scan repository in
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
  Lwt_main.run
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
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/results")
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
    | Ok body -> extract_errors body
    | Error (code, msg) ->
        Logs.warn (fun m -> m "API server returned %u, this error: %s" code msg));
    (* mark as complete *)
    let url =
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/complete")
    in
    let body = complete in
    match Http_helpers.post ~body ~headers url with
    | Ok body -> extract_block_override body
    | Error (code, msg) ->
        Error
          ("API server returned " ^ string_of_int code ^ ", this error: " ^ msg))
