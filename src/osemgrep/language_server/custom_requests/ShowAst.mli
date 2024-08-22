val meth : string

val on_request :
  Session.t -> Jsonrpc.Structured.t option -> Yojson.Safe.t option
