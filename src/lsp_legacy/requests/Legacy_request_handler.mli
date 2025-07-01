val on_request :
  Legacy_rpc_server.t ->
  Jsonrpc.Id.t ->
  'a Lsp.Client_request.t ->
  Legacy_rpc_server.t * Legacy_lsp_.Reply.t
(** [on_request request server] handles any LSP request, and returns a
  * JSONRPC response, and a new server state.
  *)
