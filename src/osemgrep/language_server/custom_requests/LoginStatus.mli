val meth : string
(** method to match on: semgrep/loginStatus *)

val on_request :
  RPC_server.t -> Jsonrpc.Structured.t option -> Yojson.Safe.t option
