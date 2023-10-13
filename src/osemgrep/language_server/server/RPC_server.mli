module type LSIO = sig
  val read : unit -> Jsonrpc.Packet.t option Lwt.t
  val write : Jsonrpc.Packet.t -> unit Lwt.t
end

module MakeLSIO (I : sig
  type input
  type output

  val stdin : input
  val stdout : output
  val read_line : input -> string option Lwt.t
  val write : output -> string -> unit Lwt.t
  val read_exactly : input -> int -> string option Lwt.t
end) : LSIO

val io_ref : (module LSIO) ref

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

val batch_notify : Lsp.Server_notification.t list -> unit
(** [batch_notify t notifs] sends a batch of notifications to the client. *)

val notify_show_message : kind:Lsp.Types.MessageType.t -> string -> unit
(** [notify_show_message] server ~kind s sends the string [s] as a message with
    type [kind] as a window to the extension
  *)

val create_progress : string -> string -> Lsp.Types.ProgressToken.t
(** [create_progress t title message] creates a progress token. *)

val end_progress : Lsp.Types.ProgressToken.t -> unit
(** [end_progress t token] ends a progress token. *)

(** [Make] creates a server from a message handler. *)
module Make (MessageHandler : sig
  val on_request : _ Lsp.Client_request.t -> t -> Jsonrpc.Json.t option * t
  val on_notification : Lsp.Client_notification.t -> t -> t
  val capabilities : Lsp.Types.ServerCapabilities.t
end) : sig
  val start : t -> unit Lwt.t
  val create : unit -> t
end
