val start : < Cap.random ; Cap.network ; Cap.tmp > -> unit Lwt.t
(** Entry point of the language server. This will start the server, and communicate over stdin/out using the Language Server Protocol *)

(* exposed for testing purposes *)
module LanguageServer : sig
  val start : RPC_server.t -> unit Lwt.t

  val handle_client_message :
    Jsonrpc.Packet.t ->
    RPC_server.t ->
    (RPC_server.t * Jsonrpc.Packet.t option) Lwt.t

  val create : < Cap.random ; Cap.network ; Cap.tmp > -> RPC_server.t
end
