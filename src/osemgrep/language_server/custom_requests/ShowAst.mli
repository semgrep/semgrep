val meth : string

val on_request :
  RPC_server.t -> Jsonrpc.Structured.t option -> Yojson.Safe.t option
