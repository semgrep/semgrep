(* from scans.py *)

val start_scan :
  dry_run:bool ->
  token:string ->
  Uri.t ->
  JSON.t ->
  (string option, string) result

val fetch_scan_config :
  token:string -> sca:bool -> dry_run:bool -> full_scan:bool -> string -> string
