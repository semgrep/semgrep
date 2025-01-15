(* This module provides helpers to communicate with our Semgrep backend in a
 * typed and abstract way. Internally, it relies on HTTP requests but this is
 * mostly invisible in the types below thanks to semgrep_output_v1.atd
 *)

(*****************************************************************************)
(* CLI<->backend comms for semgrep ci *)
(*****************************************************************************)

(* retrieves the deployment config from the provided token. *)
val get_deployment_from_token :
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.deployment_config option

(* [start_scan caps req] informs the Semgrep App that a scan
 * is about to be started, and returns the scan_response from the server.
 *)
val start_scan :
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.scan_request ->
  (Semgrep_output_v1_t.scan_response, string) result

(* the scan_id was a field returned in scan_response from start_scan() *)
type scan_id = int
type app_block_override = string (* reason *) option

(* upload both the scan results and complete *)
val upload_findings :
  < Cap.network ; Auth.cap_token ; .. > ->
  scan_id:scan_id ->
  results:Semgrep_output_v1_t.ci_scan_results ->
  complete:Semgrep_output_v1_t.ci_scan_complete ->
  (app_block_override, string) result

val report_failure :
  < Cap.network ; Auth.cap_token ; .. > ->
  scan_id:scan_id ->
  Exit_code.t ->
  unit

(*****************************************************************************)
(* Other (semgrep lsp, semgrep install-semgrep-pro comms) *)
(*****************************************************************************)

(* Used by 'semgrep scan --config policy'.
 * Internally relies on api_token in ~/.settings and SEMGREP_REPO_NAME
 * Need the network to get the deployment info from the token.
 *)
val url_for_policy : < Cap.network ; Auth.cap_token > -> Uri.t

(* Used by 'semgrep publish'.
 * alt: could be in Semgrep_Registry.ml but actually the request interact
 * with the Semgrep backend, not with the registry.
 * TODO: pass an ATD construct instead of JSON below
 *)
val upload_rule_to_registry :
  < Cap.network ; Auth.cap_token ; .. > ->
  JSON.yojson ->
  (string, string) result

(* The architecture of the Pro Engine binary to install. *)
type pro_engine_arch = Osx_arm64 | Osx_x86_64 | Manylinux_x86_64

(* used by 'semgrep install-semgrep-pro'
 * content of binary is in the body of response (get_info)
 *)
val fetch_pro_binary :
  < Cap.network ; Auth.cap_token ; .. > ->
  pro_engine_arch ->
  Http_helpers.client_result Lwt.t

(* used by 'semgrep show whomai' *)
val get_identity_async : < Cap.network ; Auth.cap_token ; .. > -> string Lwt.t

(* used by 'semgrep lsp' *)
val fetch_scan_config_string_async :
  dry_run:bool ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  < Cap.network ; Auth.cap_token ; .. > ->
  (string, string) result Lwt.t
(** [fetch_scan_config_string ~token ~sca ~dry_run ~full_scan repo] returns the
    rules (as a RAW string containing JSON data) for the provided
    configuration. *)

(*****************************************************************************)
(* Async variants of functions above *)
(*****************************************************************************)

val get_deployment_from_token_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.deployment_config option Lwt.t

val start_scan_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  Semgrep_output_v1_t.scan_request ->
  (Semgrep_output_v1_t.scan_response, string) result Lwt.t

val upload_findings_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  scan_id:scan_id ->
  results:Semgrep_output_v1_t.ci_scan_results ->
  complete:Semgrep_output_v1_t.ci_scan_complete ->
  (app_block_override, string) result Lwt.t

val report_failure_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  scan_id:scan_id ->
  Exit_code.t ->
  unit Lwt.t

val upload_rule_to_registry_async :
  < Cap.network ; Auth.cap_token ; .. > ->
  JSON.yojson ->
  (string, string) result Lwt.t
