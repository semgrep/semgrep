val meth : string
(** method to match on: semgrep/search *)

val mk_params :
  lang:Xlang.t option -> fix:string option -> string -> Jsonrpc.Structured.t

val on_request :
  RPC_server.t -> Jsonrpc.Structured.t option -> Yojson.Safe.t option
(** [on_request runner request] will run [runner] on the given pattern and optional
    language params. If the request is None, we return None. Otherwise, we return
    [Some (JSON response)] that contains the ranges of all matches *)
