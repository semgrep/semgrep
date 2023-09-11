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

let start_scan ~dry_run ~token url meta =
  if dry_run then (
    Logs.app (fun m -> m "Would have sent POST request to create scan");
    Ok "")
  else (
    Logs.debug (fun m -> m "Starting scan");
    let headers =
      [
        ("content-type", "application/json");
        ("authorization", "Bearer " ^ token);
      ]
    in
    let scan_endpoint = Uri.with_path url "api/agent/deployments/scans" in
    let body = JSON.(string_of_json (Object [ ("meta", meta) ])) in
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
        Error msg)

(* TODO: specify as ATD the reply to api/agent/deployments/scans/config *)
let extract_rule_config data =
  try
    match Yojson.Basic.from_string data with
    | `Assoc e -> (
        match List.assoc "rule_config" e with
        | `String e -> Ok e
        | _else -> Error ("Couldn't retrieve config: no rule_config in " ^ data)
        )
    | _else -> Error ("Couldn't retrieve config: not an json object: " ^ data)
  with
  | e -> Error ("Failed to decode config: " ^ Printexc.to_string e ^ ": " ^ data)

let fetch_scan_config_async ~token ~sca ~dry_run ~full_scan repository =
  let url = Semgrep_App.scan_config ~sca ~dry_run ~full_scan repository in
  let%lwt content =
    let headers =
      [
        ("Authorization", Fmt.str "Bearer %s" token);
        ("User-Agent", Fmt.str "Semgrep/%s" Version.version);
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
  let conf =
    match content with
    | Error _ as e -> e
    | Ok content -> extract_rule_config content
  in
  Lwt.return conf

let fetch_scan_config ~token ~sca ~dry_run ~full_scan repository =
  (* TODO (see below): once we have the CLI logic in place to ignore findings that are from old rule versions
     if self.dry_run:
       app_get_config_url = f"{state.env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{self._scan_params}"
     else:
       app_get_config_url = f"{state.env.semgrep_url}/{DEFAULT_SEMGREP_APP_CONFIG_URL}?{self._scan_params}"
       # TODO: uncomment the line below to replace the old endpoint with the new one once we have the
       # CLI logic in place to ignore findings that are from old rule versions
       # app_get_config_url = f"{state.env.semgrep_url}/api/agent/deployments/scans/{self.scan_id}/config"
  *)
  Lwt_main.run
    (fetch_scan_config_async ~token ~sca ~dry_run ~full_scan repository)

let report_failure ~dry_run ~token ~scan_id exit_code =
  if dry_run then (
    Logs.app (fun m ->
        m "Would have reported failure to semgrep.dev: %u" exit_code);
    Ok ())
  else
    let uri =
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/error")
    in
    let headers =
      [
        ("content-type", "application/json");
        ("authorization", "Bearer " ^ token);
      ]
    in
    let body =
      JSON.(
        string_of_json
          (Object [ ("exit_code", Int exit_code); ("stderr", String "") ]))
    in
    match Http_helpers.post ~body ~headers uri with
    | Ok _ -> Ok ()
    | Error (code, msg) ->
        Error
          ("API server returned " ^ string_of_int code ^ ", this error: " ^ msg)

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
   "/api/agent/scans/<scan_id>/complete" should be specified in ATD *)
let extract_block_override data =
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
        Ok (app_block_override, app_block_reason)
    | json ->
        Error
          (Fmt.str "Failed to understand the server reply: %s"
             (JSON.string_of_json json))
  with
  | e ->
      Error
        (Fmt.str "Failed to decode server reply as json %s: %s"
           (Printexc.to_string e) data)

let report_findings ~token ~scan_id ~dry_run ~findings_and_ignores ~complete =
  if dry_run then (
    Logs.app (fun m ->
        m "Would have sent findings and ignores blob: %s"
          (JSON.string_of_json findings_and_ignores));
    Logs.app (fun m ->
        m "Would have sent complete blob: %s" (JSON.string_of_json complete));
    Ok (false, ""))
  else (
    Logs.debug (fun m ->
        m "Sending findings and ignores blob: %s"
          (JSON.string_of_json findings_and_ignores));
    Logs.debug (fun m ->
        m "Sending complete blob: %s" (JSON.string_of_json complete));

    let url =
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/findings_and_ignores")
    in
    let headers =
      [
        ("content-type", "application/json");
        ("authorization", "Bearer " ^ token);
      ]
    in
    let body = JSON.string_of_json findings_and_ignores in
    (match Http_helpers.post ~body ~headers url with
    | Ok body -> extract_errors body
    | Error (code, msg) ->
        Logs.warn (fun m -> m "API server returned %u, this error: %s" code msg));
    (* mark as complete *)
    let url =
      Uri.with_path !Semgrep_envvars.v.semgrep_url
        ("/api/agent/scans/" ^ scan_id ^ "/complete")
    in
    let body = JSON.string_of_json complete in
    match Http_helpers.post ~body ~headers url with
    | Ok body -> extract_block_override body
    | Error (code, msg) ->
        Error
          ("API server returned " ^ string_of_int code ^ ", this error: " ^ msg))
