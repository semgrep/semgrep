(* internally rely on api_token in ~/.settings and SEMGREP_REPO_NAME *)
val url_for_policy : token_opt:Auth.token option -> Uri.t

val scan_config :
  ?sca:bool -> ?dry_run:bool -> ?full_scan:bool -> string -> Uri.t

(* *)
val get_deployment_from_token : token:Auth.token -> string option
