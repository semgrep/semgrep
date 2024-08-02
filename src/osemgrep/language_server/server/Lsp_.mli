(** The reply module forces any sort of IO (anything that requires LWT) to be in
    a monad, that will only be executed after a response has been fully
    processed. Ocaml LSP does this, and it ensures that the LWT monad doesn't
    need to be passed everywhere. Instead, users are only allowed to use LWT
    when they know they have to do something blocking, and are therefore forced
    to construct the code in such a way that the server can execute it
    asynchronously, instead of blocking the main message processing loop, or
    using [Lwt.async], which makes error handling difficult, and IO
    unpredictable, and hard to test and debug *)
module Reply : sig
  type send = Jsonrpc.Packet.t -> unit Lwt.t
  (** [send] is some function that returns a promise, that will perform IO with
       [Jsonrpc.Packet.t]. Usually this will be writing it to stdout *)

  (** [Now] means that there is no LWT needed to respond to the client, and will
      immediately be sent once the Reply is returned to the server from any
      handler. [Later] Is similar to [lazy], it's meant to be run later, but it
      takes in a function that allows the handler to send a packet. This means
      that a handler can perform some blocking IO (such as networking, a scan,
      etc.), and then later send some sort of response/notification to the
      client. This means we can avoid using [Lwt.async] when we want to perform
      IO asynchronously. *)
  type t = Now of Jsonrpc.Packet.t | Later of (send -> unit Lwt.t)

  val now : Jsonrpc.Packet.t -> t
  (** [now packet] will send a packet immediately once the function handling a client
      message returns to the main server loop *)

  val later : (send -> unit Lwt.t) -> t
  (** [later f] will provide a function [send] to [f], to allow [f] to perform
      some LWT IO, and then send a packet, or send a packet, then perform some
      LWT IO. Once this reply is passed back to the main server loop, [f] will
      be applied to [send] and that promise will be resolved immediately. *)

  val empty : t
  (** [empty] is simply a [Later] reply who's [f] does nothing, and returns a
      unit promise *)

  val apply : send -> t -> unit Lwt.t
  (** [apply send reply] will apply [send] to [Now packet], or pass it to [Later
      f]. The promise returned from this function will perform the IO in
      [send], and possibly other side effects in [Later f], once resolved *)

  val both : t -> t -> t
  (** [both first second] creates a reply [Later f], where [f] when passed some
      [send], will [apply] it to [first], resolve that promise, then apply it
      to [second]. Essentially, this sequences two replies *)
end

(** All Language Servers have a lifecycle according to the specs as follows:
    Uninitialzed -> must ignore all requests until the initialize request is received.
    Running -> normal state until shutdown is called
    Stopped -> exit process

    See:
    https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#lifeCycleMessages
 *)
module State : sig
  type t = Uninitialized | Running | Stopped
end

val packet_of_notification : Jsonrpc.Notification.t -> Jsonrpc.Packet.t
(** [packet_of_notification notif] turns a notification into a packet*)

val packet_of_response : Jsonrpc.Response.t -> Jsonrpc.Packet.t
(** [packet_of_response notif] turns a response into a packet*)

val packet_of_request : Jsonrpc.Request.t -> Jsonrpc.Packet.t
(** [packet_of_request notif] turns a request into a packet*)

val respond_json : Jsonrpc.Id.t -> Yojson.Safe.t -> Jsonrpc.Packet.t
(** [respond_json id json] creates a response packet with the given id and json
    payload. This is useful for responding to requests that don't have a
    specific response type, such as an initialize error *)

val respond : Jsonrpc.Id.t -> 'a Lsp.Client_request.t -> 'a -> Jsonrpc.Packet.t
(** [respond id request response] creates a response packet with the given id,
    and the response payload. This is useful for responding to requests that
    have a specific response type, such as an exit request *)

val request : 'a Lsp.Server_request.t -> Jsonrpc.Packet.t * string
(** [request request] creates a request packet with the given request, and
    returns the packet and the ID used for the request. This is useful for
    sending requests to the client, such as a progress create request *)

val notify : Lsp.Server_notification.t -> Jsonrpc.Packet.t
(** [notify notif] creates a notification packet with the given notification.
    This is useful for sending a notification to the client such as show message *)

val notify_custom :
  ?params:Jsonrpc.Structured.t -> string -> (Jsonrpc.Packet.t, string) result
(** [notify_custom ?params method_name ] creates a notifcation with a custom
    method and params, and returns an error if the params are malformed. This is
    useful for sending notifications that may not have a built in type in the
    LSP library *)

val batch_notify : Lsp.Server_notification.t list -> Jsonrpc.Packet.t list
(** [batch_notify notif] applies [notify] to a list of notifications. *)

val notify_show_message :
  kind:Lsp.Types.MessageType.t -> string -> Jsonrpc.Packet.t
(** [notify_show_message kind message] creates a packet that shows some sort of
    message to the client *)

val create_progress :
  string -> string -> Jsonrpc.Packet.t list * Lsp.Types.ProgressToken.t
(** [create_progress title message] creates a progress notification with the
    given title and message, and returns the packet and the progress token. The
    progress token is needed to end progress later *)

val end_progress : Lsp.Types.ProgressToken.t -> Jsonrpc.Packet.t
(** [end_progress token] creates a packet that ends a progress notification *)

val log_error_to_client : string -> exn -> Jsonrpc.Packet.t
(** [log_error_to_client message exn] creates a packet that logs an error to the
    client, specifically as a telemetry event. This is not normally visible to
    the user *)

val notify_and_log_error : string -> exn -> Jsonrpc.Packet.t list
(** [notify_and_log_error message exn] creates packets that will log an error to
    the client's telemetry, but will also display the error to the user in a
    window *)

val error_response_of_exception : Jsonrpc.Id.t -> exn -> Jsonrpc.Packet.t
(** [error_response_of_exception req_id exn] will create an error response to
    the client request *)
