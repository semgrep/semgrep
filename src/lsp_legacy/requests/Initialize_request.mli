val on_request :
  RPC_server.t ->
  Lsp.Types.InitializeParams.t ->
  RPC_server.t * Lsp.Types.InitializeResult.t
(** [on_request server params] is the result of handling the initialize request
  * [params] on the server [server].
  *
  * See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#initialize
  *)
