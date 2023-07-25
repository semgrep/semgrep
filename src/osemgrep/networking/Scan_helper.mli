(* from scans.py *)

val start_scan :
  dry_run:bool -> token:string -> Uri.t -> JSON.t -> (string, string) result
(** [start_scan ~dry_run ~token url meta] informs the Semgrep App that a scan
    is about to be started, and returns the scan id from the server. If
    [dry_run] is [true], the empty string will be returned ([Ok ""]). *)

val fetch_scan_config_async :
  token:string ->
  sca:bool ->
  dry_run:bool ->
  full_scan:bool ->
  string ->
  (string, string) result Lwt.t
(** [fetch_scan_config_async ~token ~sca ~dry_run ~full_scan repo] returns a promise of the rules
    for the provided configuration. *)

val fetch_scan_config :
  token:string ->
  sca:bool ->
  dry_run:bool ->
  full_scan:bool ->
  string ->
  (string, string) result
(** [fetch_scan_config ~token ~sca ~dry_run ~full_scan repo] returns the rules
    for the provided configuration. *)

val report_failure :
  dry_run:bool -> token:string -> scan_id:string -> int -> (unit, string) result
(** [report_failure ~dry+run ~token ~scan_id exit_code] reports the failure
    for [scan_id] to Semgrep App. *)

val report_findings :
  token:string ->
  scan_id:string ->
  dry_run:bool ->
  findings_and_ignores:JSON.t ->
  complete:JSON.t ->
  (bool * string, string) result
(** [report_findings ~token ~scan_id ~dry_run ~findings_and_ignores ~complete]
    reports the findings to Semgrep App. *)
