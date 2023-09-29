(* does not return a Hover.t because we handle the case where we return the
   nullary response
*)
val on_request :
  RPCServer.t -> Lsp.Types.HoverParams.t -> Yojson.Safe.t option * RPCServer.t
