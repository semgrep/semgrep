(* does not return a Hover.t because we handle the case where we return the
   nullary response
*)
val on_request :
  RPC_server.t -> Lsp.Types.HoverParams.t -> RPC_server.t * Yojson.Safe.t option
(** [on_request server params] is the response to a hover request.
  *
  * See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover
  *)
