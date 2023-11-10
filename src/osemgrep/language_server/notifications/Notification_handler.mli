val on_notification : Lsp.Client_notification.t -> RPC_server.t -> RPC_server.t
(** [on_notification request server] handles any LSP notification, and returns
  * a new server state.
  *)
