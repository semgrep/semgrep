val on_notification :
  RPC_server.t ->
  Lsp.Client_notification.t ->
  RPC_server.t * Lsp_.Reply.t option
(** [on_notification request server] handles any LSP notification, and returns
  * a new server state and possilby some notifications to send back to the client.
  *)
