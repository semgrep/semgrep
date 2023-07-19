module State : sig
  type t = Uninitialized | Running | Stopped
end

type t = { session : Session.t; state : State.t }

val batch_notify : t -> Lsp.Server_notification.t list -> unit
(** [batch_notify t notifs] sends a batch of notifications to the client. *)

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
  val create : unit -> t
end
