val on_request :
  RPC_server.t ->
  Lsp.Types.CodeActionParams.t ->
  RPC_server.t * Lsp.Types.CodeActionResult.t
(** [on_request server params] is the result of the code action request [params]
  * on the server [server].
  *
  * See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction
  *)
