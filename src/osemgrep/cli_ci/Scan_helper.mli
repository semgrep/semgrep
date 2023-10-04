type scan_id = string
type app_block_override = string (* reason *) option

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
