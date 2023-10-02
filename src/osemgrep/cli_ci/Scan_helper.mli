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

val fetch_scan_config_async :
  dry_run:bool ->
  token:Auth.token ->
  sca:bool ->
  full_scan:bool ->
  string ->
  (string, string) result Lwt.t
(** [fetch_scan_config_async ~token ~sca ~dry_run ~full_scan repo] returns a
     promise of the rules for the provided configuration. *)

val fetch_scan_config :
  dry_run:bool ->
  token:string ->
  sca:bool ->
  full_scan:bool ->
  string ->
  (string, string) result
(** [fetch_scan_config ~token ~sca ~dry_run ~full_scan repo] returns the rules
    for the provided configuration. *)

val report_findings :
  dry_run:bool ->
  token:string ->
  scan_id:scan_id ->
  (* TODO: use proper types from semgrep_output_v1.atd *)
  findings_and_ignores:JSON.t ->
  complete:JSON.t ->
  (app_block_override, string) result
(** [report_findings ~dry_run ~token ~scan_id ~findings_and_ignores ~complete]
    reports the findings to Semgrep App. *)

val report_failure :
  dry_run:bool ->
  token:string ->
  scan_id:scan_id ->
  int ->
  (unit, string) result
(** [report_failure ~dry_run ~token ~scan_id exit_code] reports the failure
    for [scan_id] to Semgrep App. *)
