val on_request :
  RPCServer.t ->
  Lsp.Types.CodeActionParams.t ->
  Lsp.Types.CodeActionResult.t * RPCServer.t
