val meth : string
(** method to match on: semgrep/loginFinish *)

val on_notification : Session.t -> Jsonrpc.Structured.t option -> Lsp_.Reply.t
(** [on_notification] will start an asynchronous job to process the
      session information and complete the authentication process for login
    *)
