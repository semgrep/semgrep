val meth : string
(** method to match on: [semgrep/loginStatus] *)

val on_request :
  Legacy_session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Legacy_session.t * Legacy_lsp_.Reply.t
