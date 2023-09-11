(* internally rely on api_token in ~/.semgrep/settings.yml and SEMGREP_REPO_NAME *)
val url_for_policy : token:Auth.token -> Uri.t

(* construct the Uri where to retrieve the scan configuration, depending on
   the parameters and the repository name *)
val scan_config :
  ?sca:bool -> ?dry_run:bool -> ?full_scan:bool -> string -> Uri.t

(* retrieves the deployment name from the provided token. *)
val get_deployment_from_token : token:Auth.token -> (int * string) option
