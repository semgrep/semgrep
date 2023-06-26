let start_scan ~dry_run ~token url meta =
  if dry_run then (
    Logs.app (fun m -> m "Would have sent POST request to create scan");
    Ok None)
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
    match Http.post ~body ~headers scan_endpoint with
    | Ok body -> (
        let json = JSON.json_of_string body in
        match json with
        | Object xs -> (
            match List.assoc_opt "scan" xs with
            | Some (Object dd) -> (
                match List.assoc_opt "id" dd with
                | Some (Int i) -> Ok (Some (string_of_int i))
                | _else -> Ok None)
            | _else -> Ok None)
        | _else -> Ok None)
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
  let url = Semgrep_App.scan_config ~sca ~dry_run ~full_scan repository in
  let content =
    let headers = [ ("authorization", "Bearer " ^ token) ] in
    match Http.get ~headers url with
    | Ok body -> body
    | Error msg ->
        (* was raise Semgrep_error, but equivalent to abort now *)
        Error.abort
          (Printf.sprintf "Failed to download config from %s: %s"
             (Uri.to_string url) msg)
  in
  Logs.debug (fun m -> m "finished downloading from %s" (Uri.to_string url));
  try
    match Yojson.Basic.from_string content with
    | `Assoc e -> (
        match List.assoc "rule_config" e with
        | `String e -> e
        | _else -> invalid_arg "couldn't retrieve config")
    | _else -> invalid_arg "couldn't retrieve config"
  with
  | _failure -> invalid_arg "couldn't retrieve config"
