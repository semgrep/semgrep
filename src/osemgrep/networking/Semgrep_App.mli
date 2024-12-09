(* This module provides helpers to communicate with our Semgrep backend
 * in a typed and abstract way. Internally, it relies on HTTP requests
 * but this is mostly invisible in the types below thanks to
 * semgrep_output_v1.atd
 *)

(* The architecture of the Pro Engine binary to install. *)
type pro_engine_arch = Osx_arm64 | Osx_x86_64 | Manylinux_x86_64
type app_block_override = string (* reason *) option

(*****************************************************************************)
(* Sync *)
(*****************************************************************************)
(* retrieves the deployment config from the provided token. *)
val get_deployment_from_token :
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.deployment_config option

(* retrieves the scan config from the provided token. *)
val get_scan_config_from_token :
  < Cap.network ; Auth.cap_token > -> Semgrep_output_v1_t.scan_config option

(* Internally rely on api_token in ~/.settings and SEMGREP_REPO_NAME
 * Need the network to get the deployment info from the token.
 *)
val url_for_policy : < Cap.network ; Auth.cap_token > -> Uri.t

(* construct the Uri where to retrieve the scan configuration, depending on
   the parameters and the repository name *)
val scan_config_uri :
  ?sca:bool -> ?dry_run:bool -> ?full_scan:bool -> string -> Uri.t

type scan_id = string

val start_scan :
  dry_run:bool ->
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.scan_request ->
  (scan_id, string) result
(** [start_scan ~dry_run caps req] informs the Semgrep App that a scan
    is about to be started, and returns the scan id from the server. If
    [dry_run] is [true], the empty string will be returned ([Ok ""]). *)

(* TODO: diff with get_scan_config_from_token? *)
val fetch_scan_config :
  dry_run:bool ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  < Cap.network ; Auth.cap_token ; .. > ->
  (Semgrep_output_v1_t.scan_config, string) result
(** [fetch_scan_config ~token ~sca ~dry_run ~full_scan repo] returns the rules
    (as a RAW string containing JSON data) for the provided configuration. *)

(* upload both the scan_results and complete *)
val upload_findings :
  dry_run:bool ->
  scan_id:scan_id ->
  results:Semgrep_output_v1_t.ci_scan_results ->
  complete:Semgrep_output_v1_t.ci_scan_complete ->
  < Cap.network ; Auth.cap_token ; .. > ->
  (app_block_override, string) result
(** [upload_findings ~dry_run ~token ~scan_id ~results ~complete]
    reports the findings to Semgrep App. *)

(* report a failure for [scan_id] to Semgrep App *)
val report_failure :
  dry_run:bool ->
  scan_id:scan_id ->
  < Cap.network ; Auth.cap_token ; .. > ->
  Exit_code.t ->
  unit

(* could be in Semgrep_Registry.ml but actually the request interact
 * with the Semgrep backend, not with the registry.
 * TODO: pass an ATD construct instead of JSON below
 *)
val upload_rule_to_registry :
  < Cap.network ; Auth.cap_token ; .. > ->
  JSON.yojson ->
  (string, string) result

(* content of binary is in the body of response (get_info) *)
val fetch_pro_binary :
  < Cap.network ; Auth.cap_token ; .. > ->
  pro_engine_arch ->
  Http_helpers.client_result Lwt.t

(*****************************************************************************)
(* Async *)
(*****************************************************************************)

val get_identity_async : < Cap.network ; Auth.cap_token ; .. > -> string Lwt.t

val get_deployment_from_token_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.deployment_config option Lwt.t

val get_scan_config_from_token_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.scan_config option Lwt.t

val fetch_scan_config_string :
  dry_run:bool ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  < Cap.network ; Auth.cap_token ; .. > ->
  (string, string) result Lwt.t
(** [fetch_scan_config_string ~token ~sca ~dry_run ~full_scan repo] returns the
    rules (as a RAW string containing JSON data) for the provided
    configuration. *)

val fetch_scan_config_async :
  dry_run:bool ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  < Cap.network ; Auth.cap_token ; .. > ->
  (Semgrep_output_v1_t.scan_config, string) result Lwt.t

val report_failure_async :
  dry_run:bool ->
  scan_id:scan_id ->
  < Cap.network ; Auth.cap_token ; .. > ->
  Exit_code.t ->
  unit Lwt.t

val start_scan_async :
  dry_run:bool ->
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.scan_request ->
  (scan_id, string) result Lwt.t

val upload_findings_async :
  dry_run:bool ->
  scan_id:scan_id ->
  results:Semgrep_output_v1_t.ci_scan_results ->
  complete:Semgrep_output_v1_t.ci_scan_complete ->
  < Cap.network ; Auth.cap_token ; .. > ->
  (app_block_override, string) result Lwt.t

val upload_rule_to_registry_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  JSON.yojson ->
  (string, string) result Lwt.t
