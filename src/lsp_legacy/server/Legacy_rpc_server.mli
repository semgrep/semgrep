(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
module type LSIO = sig
  (** This is a signature for the IO module to be used by the language server's
    RPC server. This is abstracted away so we can use different IO for
    different implementations (JS vs Unix), and in the future different
    different communication methods (stdout vs sockets)
  *)

  val read : unit -> Jsonrpc.Packet.t option Lwt.t
  (** [read ()] returns a promise that will read the next JSONRPC packet*)

  val write : Jsonrpc.Packet.t -> unit Lwt.t
  (** [write p] writes the JSONRPC packet [p] to the output channel *)

  val flush : unit -> unit Lwt.t
  (** [flush ()] flushes the output channel *)
end

type t = { session : Legacy_session.t; state : Legacy_lsp_.State.t }
(** This struct keeps track of the actual server lifecycle. [state] is whether
    the server is running, and [session] holds things such as targets, parsed
    rules, etc. *)

type handler = {
  on_request :
    'a. t -> Jsonrpc.Id.t -> 'a Lsp.Client_request.t -> t * Legacy_lsp_.Reply.t;
      (** [on_request server req_id request] will process a client request, and
          return a server with a modified [state] and/or [state]. The [Reply.t]
          will possibly send responses, requests, or notifications to the
          client, and any IO needed to create these messages. [on_request] is
          guaranteed to perform no IO, asynchronous actions, or heavy
          computation except on resolution of the reply, and therefore can be
          called and expected to return quickly. The reply may only modify
          [session.session_cache]. *)
  on_notification :
    t -> Lsp.Client_notification.t -> t * Legacy_lsp_.Reply.t option;
      (** [on_notification server notif] is similar to [on_request], except it may
      or may not reply to the client*)
}

val send : Jsonrpc.Packet.t -> unit Lwt.t
(** [send p] sends the JSONRPC packet [p] to the client *)

val set_async_exception_hook : unit -> unit
(** [set_async_exception_hook ()] sets up a hook to catch uncaught exceptions in
    async code and log them to the client. This is useful for debugging, as
    otherwise exceptions in async code are silently ignored. Async exceptions
    are guaranteed to only happen at the end of an RPC_server loop, when IO is
    attempted. *)

val start : handler:handler -> t -> unit Lwt.t
(** [start ~handler server] will begin an IO loop processing client messages
    using [Io], and the returned promise will not resolve until the client
    asks the server to exit, or a unrecoverable error happens *)

(* Exposed so if we want to use the handler directly, and forgo
   IO we can *)
val handle_client_message :
  handler:handler ->
  t ->
  Jsonrpc.Packet.t ->
  t * (Legacy_lsp_.Reply.t, string) result
(** [handle_client_message ~handler server message] will process a message, and
    return a reply if handled ok, or an error if there was something wrong with
    the message structure itself (but not if the message is valid but something
    went wrong while handling it)  *)

val create : Lsp.Types.ServerCapabilities.t -> t
