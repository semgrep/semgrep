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

type scan_id = string
type app_block_override = string (* reason *) option

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

val start_scan :
  dry_run:bool ->
  token:Auth.token ->
  Uri.t ->
  Project_metadata.t ->
  Semgrep_output_v1_t.scan_metadata ->
  (scan_id, string) result
(** [start_scan ~dry_run ~token url prj] informs the Semgrep App that a scan
    is about to be started, and returns the scan id from the server. If
    [dry_run] is [true], the empty string will be returned ([Ok ""]). *)

val fetch_scan_config :
  dry_run:bool ->
  token:string ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  (Semgrep_output_v1_t.scan_config, string) result
(** [fetch_scan_config ~token ~sca ~dry_run ~full_scan repo] returns the rules
    (as a RAW string containing JSON data) for the provided configuration. *)

(* upload both the scan_results and complete *)
val upload_findings :
  dry_run:bool ->
  token:string ->
  scan_id:scan_id ->
  results:Semgrep_output_v1_t.ci_scan_results ->
  complete:Semgrep_output_v1_t.ci_scan_complete ->
  (app_block_override, string) result
(** [upload_findings ~dry_run ~token ~scan_id ~results ~complete]
    reports the findings to Semgrep App. *)

(* lwt-friendly versions for the language-server *)
val fetch_scan_config_async :
  dry_run:bool ->
  token:Auth.token ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  (Semgrep_output_v1_t.scan_config, string) result Lwt.t
(** [fetch_scan_config_async ~token ~sca ~dry_run ~full_scan repo] returns a
     promise of the rules for the provided configuration. *)
