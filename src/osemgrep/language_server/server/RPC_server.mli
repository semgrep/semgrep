(** All Language Servers have a lifecycle according to the specs as follows:
    Uninitialzed -> must ignore all requests until the initialize request is received.
    Running -> normal state until shutdown is called
    Stopped -> exit process

    This struct keeps track of the actual server lifecycle
    See:
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages
 *)
module State : sig
  type t = Uninitialized | Running | Stopped
end

type t = { session : Session.t; state : State.t }

val batch_notify : t -> Lsp.Server_notification.t list -> unit
(** [batch_notify t notifs] sends a batch of notifications to the client. *)

val notify_show_message : t -> kind:Lsp.Types.MessageType.t -> string -> unit
(** [notify_show_message] server ~kind s sends the string [s] as a message with
    type [kind] as a window to the extension
  *)

val create_progress : t -> string -> string -> Lsp.Types.ProgressToken.t
(** [create_progress t title message] creates a progress token. *)

val end_progress : t -> Lsp.Types.ProgressToken.t -> unit
(** [end_progress t token] ends a progress token. *)

(** [Make] creates a server from a message handler. *)
module Make (MessageHandler : sig
  val on_request : _ Lsp.Client_request.t -> t -> Jsonrpc.Json.t option * t
  val on_notification : Lsp.Client_notification.t -> t -> t
  val capabilities : Lsp.Types.ServerCapabilities.t
end) : sig
  val start : t -> unit
  val start_async : t -> unit Lwt.t
  val create : unit -> t
end

(* exposed just for testing purposes *)
module Io : sig
  val read : Lwt_io.input_channel -> Jsonrpc.Packet.t option Lwt.t
  val write : Lwt_io.output_channel -> Jsonrpc.Packet.t -> unit Lwt.t
end
