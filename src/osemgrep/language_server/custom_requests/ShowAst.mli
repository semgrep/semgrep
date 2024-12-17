val meth : string

val on_request :
  Session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Session.t * Lsp_.Reply.t
