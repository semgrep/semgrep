val meth : string
(** method to match on: [semgrep/loginStart] *)

val on_request :
  Session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Session.t * Lsp_.Reply.t
(** Called by client to login to Semgrep App, returning the [id] of the
      login session and the [uri] to log in to. Returns None if already logged in **)
