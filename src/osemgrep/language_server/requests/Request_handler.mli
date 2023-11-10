val on_request :
  _ Lsp.Client_request.t -> RPC_server.t -> Jsonrpc.Json.t option * RPC_server.t
(** [on_request request server] handles any LSP request, and returns a
  * JSONRPC response, and a new server state.
  *)
