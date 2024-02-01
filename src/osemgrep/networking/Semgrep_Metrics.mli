(* Code to interact with the metrics.semgrep.dev endpoint *)

(* will also print Logs *)
val send_async : < Cap.network ; .. > -> unit Lwt.t
val send : < Cap.network ; .. > -> unit
