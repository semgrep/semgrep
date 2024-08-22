val on_request :
  RPC_server.t ->
  Jsonrpc.Id.t ->
  'a Lsp.Client_request.t ->
  RPC_server.t * Lsp_.Reply.t
(** [on_request request server] handles any LSP request, and returns a
  * JSONRPC response, and a new server state.
  *)
