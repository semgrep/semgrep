val meth : string
(** method to match on: [semgrep/loginStatus] *)

val on_request :
  Session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Session.t * Lsp_.Reply.t
