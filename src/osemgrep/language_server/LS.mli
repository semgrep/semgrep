val start : unit -> unit
(** Entry point of the language server. This will start the server, and communicate over stdin/out using the Language Server Protocol *)

(* exposed for testing purposes *)
module LanguageServer : sig
  val start : RPC_server.t -> unit
  val start_async : RPC_server.t -> unit Lwt.t
  val create : unit -> RPC_server.t
end
