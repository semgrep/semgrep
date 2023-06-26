(* from scans.py *)

(** [start_scan ~dry_run ~token url meta] informs the Semgrep App that a scan
    is about to be started, and returns the scan id from the server. *)
val start_scan :
  dry_run:bool ->
  token:string ->
  Uri.t ->
  JSON.t ->
  (string option, string) result

(** [fetch_scan_config ~token ~sca ~dry_run ~full_scan repo] returns the rules
    for the provided configuration. *)
val fetch_scan_config :
  token:string -> sca:bool -> dry_run:bool -> full_scan:bool -> string -> string
