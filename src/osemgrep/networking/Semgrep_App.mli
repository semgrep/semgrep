(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* This module provides helpers to communicate with our Semgrep backend in a
 * typed and abstract way. Internally, it relies on HTTP requests but this is
 * mostly invisible in the types below thanks to semgrep_output_v1.atd
 *)

(*****************************************************************************)
(* CLI<->backend comms for semgrep ci *)
(*****************************************************************************)

(* [start_scan req] informs the Semgrep App that a scan
 * is about to be started, and returns the scan_response from the server.
 *)
val start_scan :
  Auth.token ->
  Semgrep_output_v1_t.scan_request ->
  (Semgrep_output_v1_t.scan_response, string * Exit_code.t option) result

(* the scan_id was a field returned in scan_response from start_scan() *)
type scan_id = int
type app_block_override = string (* reason *) option

(* upload both the scan results and complete *)
val upload_findings :
  Auth.token ->
  scan_id:scan_id ->
  results:Semgrep_output_v1_t.ci_scan_results ->
  complete:Semgrep_output_v1_t.ci_scan_complete ->
  (app_block_override, string) result

val report_failure : Auth.token -> scan_id:scan_id -> Exit_code.t -> unit

val query_tr_cache :
  Auth.token ->
  Semgrep_output_v1_t.tr_query_cache_request ->
  (Semgrep_output_v1_t.tr_query_cache_response, string) result
(** Query the transitive reachability cache for matches *)

val add_to_tr_cache :
  Auth.token ->
  Semgrep_output_v1_t.tr_add_cache_request ->
  (unit, string) result
(** Add entries to the transitive reachability cache *)

(*****************************************************************************)
(* Other (semgrep lsp, semgrep install-semgrep-pro comms) *)
(*****************************************************************************)

(* Used by 'semgrep scan --config policy'.
 * Internally relies on api_token in ~/.settings and SEMGREP_REPO_NAME
 * Need the network to get the deployment info from the token.
 *)
val url_for_policy : ?from_hooks:bool -> Auth.token -> Uri.t

(* Used by 'semgrep publish'.
 * alt: could be in Semgrep_Registry.ml but actually the request interact
 * with the Semgrep backend, not with the registry.
 * TODO: pass an ATD construct instead of JSON below
 *)
val upload_rule_to_registry :
  Auth.token -> JSON.yojson -> (string, string) result

(* used by 'semgrep show deployment' and 'semgrep login' *)
val deployment_config :
  Auth.token -> Semgrep_output_v1_t.deployment_config option

val deployment_config_eio :
  Auth.token -> Semgrep_output_v1_t.deployment_config option

(* used by 'semgrep show identity' *)
val get_identity_async : Auth.token -> string Lwt.t

(* used by 'semgrep lsp' *)
val fetch_scan_config_string_async :
  dry_run:bool ->
  secrets:bool ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  Auth.token ->
  (string, string) result Lwt.t

val fetch_scan_config_v2_async :
  ?secrets:bool ->
  ?sca:bool ->
  Auth.token ->
  (Semgrep_output_v1_t.scan_configuration, string) result Lwt.t
(** Fetch scan config via start-then-poll using the v2 endpoint (LWT). *)

val fetch_scan_config_v2_eio :
  ?secrets:bool ->
  ?sca:bool ->
  Auth.token ->
  (Semgrep_output_v1_t.scan_configuration, string) result
(** Fetch scan config via start-then-poll using the v2 endpoint. *)

val fetch_scan_config_string_eio :
  dry_run:bool ->
  secrets:bool ->
  sca:bool ->
  full_scan:bool ->
  repository:string ->
  Auth.token ->
  (string, string) result
(** [fetch_scan_config_string_eio ~token ~sca ~dry_run ~full_scan repo] returns the
    rules (as a RAW string containing JSON data) for the provided
    configuration. *)

val upload_symbol_analysis :
  token:Auth.token ->
  scan_id:int ->
  Semgrep_output_v1_t.symbol_analysis ->
  (string, string) result

val upload_subproject_symbol_analysis :
  token:Auth.token ->
  scan_id:int ->
  manifest:Fpath.t option ->
  lockfile:Fpath.t option ->
  Semgrep_output_v1_t.symbol_analysis ->
  (string, string) result

(*****************************************************************************)
(* Async variants of functions above *)
(*****************************************************************************)

val start_scan_async :
  Auth.token ->
  Semgrep_output_v1_t.scan_request ->
  (Semgrep_output_v1_t.scan_response, string * Exit_code.t option) result Lwt.t

val upload_findings_async :
  Auth.token ->
  scan_id:scan_id ->
  results:Semgrep_output_v1_t.ci_scan_results ->
  complete:Semgrep_output_v1_t.ci_scan_complete ->
  (app_block_override, string) result Lwt.t

val report_failure_async :
  Auth.token -> scan_id:scan_id -> Exit_code.t -> unit Lwt.t

val deployment_config_async :
  Auth.token -> Semgrep_output_v1_t.deployment_config option Lwt.t

val upload_rule_to_registry_async :
  Auth.token -> JSON.yojson -> (string, string) result Lwt.t

val upload_symbol_analysis_async :
  token:Auth.token ->
  scan_id:int ->
  Semgrep_output_v1_t.symbol_analysis ->
  (string, string) result Lwt.t

val upload_subproject_symbol_analysis_async :
  token:Auth.token ->
  scan_id:int ->
  manifest:Fpath.t option ->
  lockfile:Fpath.t option ->
  Semgrep_output_v1_t.symbol_analysis ->
  (string, string) result Lwt.t
