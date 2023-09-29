val on_request :
  RPCServer.t -> Lsp.Types.HoverParams.t -> Yojson.Safe.t option * RPCServer.t
