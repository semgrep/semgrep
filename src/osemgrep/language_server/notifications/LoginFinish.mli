val meth : string
(** method to match on: semgrep/loginFinish *)

val on_notification :
  RPC_server.t -> Jsonrpc.Structured.t option -> RPC_server.t option
