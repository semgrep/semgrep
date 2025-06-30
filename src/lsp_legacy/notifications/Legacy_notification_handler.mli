val on_notification :
  Legacy_rpc_server.t ->
  Lsp.Client_notification.t ->
  Legacy_rpc_server.t * Legacy_lsp_.Reply.t option
(** [on_notification request server] handles any LSP notification, and returns
  * a new server state and possilby some notifications to send back to the client.
  *)
