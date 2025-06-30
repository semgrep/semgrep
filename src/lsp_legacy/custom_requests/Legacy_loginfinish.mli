val meth : string
(** method to match on: [semgrep/loginFinish] *)

val on_request :
  Legacy_session.t ->
  Jsonrpc.Id.t ->
  Jsonrpc.Structured.t option ->
  Legacy_session.t * Legacy_lsp_.Reply.t
(** [on_request] will start an asynchronous job to process the
    session information and complete the authentication process for login
    *)
