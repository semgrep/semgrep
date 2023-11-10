val meth : string
(** method to match on: semgrep/login *)

val on_request :
  RPC_server.t -> Jsonrpc.Structured.t option -> Yojson.Safe.t option
(** Called by client to login to Semgrep App, returning the [id] of the
      login session and the [uri] to log in to. Returns None if already logged in **)
