type deployment_config = {
  id : int;
  name : string;
  display_name : string; [@default ""]
  slug : string; [@default ""]
  source_type : string; [@default ""]
  has_autofix : bool; [@default false]
  has_deepsemgrep : bool; [@default false]
  has_triage_via_comment : bool; [@default false]
  has_dependency_query : bool; [@default false]
  default_user_role : string; [@default ""]
  organization_id : int; [@default 0]
  scm_name : string; [@default ""]
}
[@@deriving yojson]
(** [type deployment_config] is what the app returns for deployment config. *)

type scan_config = {
  deployment_id : int;
  deployment_name : string;
  policy_names : string list;
  rule_config_raw : string;
  autofix : bool;
  deepsemgrep : bool;
  dependency_query : bool;
  skipped_match_based_ids : string list;
  enabled_products : string list;
  ignore_files : string list;
}
(** [type scan_config] is what the app returns for scan config for deployments. *)

(* retrieves the deployment config from the provided token. *)
val get_deployment_from_token : token:Auth.token -> deployment_config option

(* retrieves the deployment name from the provided token asynchronously *)
val get_deployment_from_token_async :
  token:Auth.token -> deployment_config option Lwt.t

(* retrieves the scan config from the provided token. *)
val get_scan_config_from_token : token:Auth.token -> scan_config option

(* retrieves the scan config from the provided token asynchronously *)
val get_scan_config_from_token_async :
  token:Auth.token -> scan_config option Lwt.t

(* internally rely on api_token in ~/.settings and SEMGREP_REPO_NAME *)
val url_for_policy : token:Auth.token -> Uri.t

(* construct the Uri where to retrieve the scan configuration, depending on
   the parameters and the repository name *)
val scan_config_uri :
  ?sca:bool -> ?dry_run:bool -> ?full_scan:bool -> string -> Uri.t
