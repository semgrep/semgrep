val handle_execute_request :
  RPC_server.t ->
  string ->
  Yojson.Safe.t list ->
  Yojson.Safe.t option * RPC_server.t
