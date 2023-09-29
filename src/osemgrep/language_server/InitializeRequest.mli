val on_request :
  RPCServer.t ->
  Lsp.Types.InitializeParams.t ->
  Lsp.Types.InitializeResult.t * RPCServer.t
