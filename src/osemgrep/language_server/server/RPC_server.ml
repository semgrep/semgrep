(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* Commentary *)
(* This contains all networking/jsonrpc related functionality of the *)
(* language server. This means that our main event loop is here, and *)
(* it's as follows: *)
(* Handle server state (uninitialized, running, stopped etc.) *)
(*  -> Read STDIN *)
(*  -> parse message to notification or request *)
(*  -> Let Semgrep LS handle notif/req *)
(*  -> If request, get response from Semgrep LS *)
(*  -> Try and complete any LWT promises *)
(*  -> Respond if needed *)
(*  -> loop *)
(*  *)
(* The hope here is that anything that isn't Semgrep *)
(* specific, but Language Server related, goes in here. The other goal *)
(* is that this contains as much of the Lwt code possible *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Jsonrpc
open Lsp
open Lsp_
module SR = Server_request
module SN = Server_notification
module CR = Client_request
module CN = Client_notification

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Server IO *)
module type LSIO = sig
  val read : unit -> Jsonrpc.Packet.t option Lwt.t
  val write : Jsonrpc.Packet.t -> unit Lwt.t
  val flush : unit -> unit Lwt.t
end

let unset_io : (module LSIO) =
  (module struct
    let read () =
      failwith
        "IO not set. This is a bug in the language server. Please report it \
         with the command you ran to get this error"

    let write _ =
      failwith
        "IO not set. This is a bug in the language server. Please report it \
         with the command you ran to get this error"

    let flush () =
      failwith
        "IO not set. This is a bug in the language server. Please report it \
         with the command you ran to get this error"
  end)

type t = { session : Session.t; state : State.t }

type handler = {
  on_request : 'a. t -> Id.t -> 'a CR.t -> t * Reply.t;
  on_notification : t -> CN.t -> t * Reply.t option;
}
(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let io_ref : (module LSIO) ref = ref unset_io

(* Why the atomic writes below? The LSP library we use does something weird, *)
(* it writes the jsonrpc header then body with seperate calls to write, which *)
(* means there's a race condition there. The below atomic calls ensures that *)
(* the ENTIRE packet is written at the same time *)
let send packet =
  let module Io = (val !io_ref : LSIO) in
  Logs.debug (fun m ->
      m "Sending response %s"
        (Packet.yojson_of_t packet |> Yojson.Safe.pretty_to_string));
  let%lwt () = Io.write packet in
  Io.flush ()

let read () =
  let module Io = (val !io_ref : LSIO) in
  Io.read ()

(* The handler is expected to take a properly parsed packet, so we handle any
   sort of parsing issues here, say if the message is a valid JSONRPC message
   but has malformed parameters etc. *)
let to_jsonrpc (state : t) (handler : handler) =
  let on_request req =
    match CR.of_jsonrpc req with
    | Ok (CR.E r) -> handler.on_request state req.id r
    | Error message ->
        let code = Jsonrpc.Response.Error.Code.InvalidParams in
        let error = Jsonrpc.Response.Error.make ~code ~message () in
        let resp = Jsonrpc.Response.error req.id error in
        (state, Reply.now (packet_of_response resp))
  in
  let on_notification notif =
    match CN.of_jsonrpc notif with
    | Ok notif -> handler.on_notification state notif
    | Error message ->
        let log = log_error_to_client message (Invalid_argument message) in
        (state, Some (Reply.now log))
  in
  (on_request, on_notification)

let handle_client_message ~(handler : handler) server (msg : Packet.t) =
  let on_request, on_notification = to_jsonrpc server handler in
  match msg with
  | Notification n -> (
      try
        let state, reply_opt = on_notification n in
        let reply = Option.value ~default:Reply.empty reply_opt in
        (state, Ok reply)
      with
      | e ->
          (* If something went wrong let's explicitly tell the client *)
          let msg = Printf.sprintf "Error handling notification %s" n.method_ in
          let log = notify_and_log_error msg e in
          let log_packet = Reply.later (fun send -> Lwt_list.iter_s send log) in
          (server, Ok log_packet))
  | Request req -> (
      try
        let state, reply = on_request req in
        (state, Ok reply)
      with
      | e ->
          (* Don't notify since the client will once we respond *)
          let msg = Printf.sprintf "Error handling request %s" req.method_ in
          let log = log_error_to_client msg e in
          let error_repsonse = error_response_of_exception req.id e in
          let reply =
            Reply.later (fun send ->
                Lwt_list.iter_s send [ log; error_repsonse ])
          in
          (* Client will handle showing error message *)
          (server, Ok reply))
  | _ ->
      (* There are batch notifications and batch requests, and most notably
         responses. that we don't handle above. Batch notifications/requests
         aren't well supported across the ecosystem, and we don't make any
         requests to the client that we would care about the response, so we
         don't worry about any of them *)
      let msg =
        Printf.sprintf "Unhandled message:\n%s"
          (msg |> Packet.yojson_of_t |> Yojson.Safe.pretty_to_string)
      in
      (server, Error msg)

(*****************************************************************************)
(* Server *)
(*****************************************************************************)

let notify_and_log_error_sync msg e =
  let error = notify_and_log_error msg e in
  Lwt.dont_wait
    (fun () -> Lwt_list.iter_s send error)
    (fun exn ->
      Logs.err (fun m -> m "%s: %s" msg (Printexc.to_string exn));
      let backtrace = Printexc.get_backtrace () in
      Logs.err (fun m -> m "Backtrace: %s" backtrace))

(* Set async exception hook so we error handle better *)

let set_async_exception_hook () =
  Lwt.async_exception_hook :=
    notify_and_log_error_sync "Uncaught exception in async_exception_hook"

(* NOTE: this function is only used by the native version of the extension,
   but not LSP.js. [handle_client_message] is used by LSP.js though. *)
let start ~(handler : handler) server =
  let rec rpc_loop server =
    match server.state with
    | State.Stopped ->
        Logs.app (fun m -> m "Server stopped");
        Lwt.return_unit
    | _ -> (
        let%lwt client_msg = read () in
        match client_msg with
        | Some msg ->
            let%lwt server =
              let server, reply_result =
                handle_client_message ~handler server msg
              in
              (* Lwt.async is ok ONLY here, because we don't actually care to
                 wait for the current promise holding a possible reply to
                 resolve before processing more messages. For example, the reply
                 may run a semgrep scan (in a separate thread), before sending
                 the diagnostic results and then resolving. Anything that needs
                 to mutate the server AFTER sending a response/entering an LWT
                 promise can mutate the session cache which is protected by a
                 mutex and is therefore safe across promises/threads, and
                 protects a bunch of refs *)
              (* TODO: Really we should resolve these in a task of some sort,
                 and keep track of them, so if the client wants to cancel them
                 they can *)
              (* THINK: Should these be resolved in a separate thread? Or should
                 the expectation be that Replys will spawn their own only if
                 they need it? *)
              Lwt.dont_wait
                (fun () ->
                  match reply_result with
                  | Ok reply -> Reply.apply send reply
                  | Error e ->
                      Logs.err (fun m -> m "%s" e);
                      Lwt.return_unit)
                (notify_and_log_error_sync "Error resolving reply");
              Lwt.return server
            in
            rpc_loop server
        | None ->
            Logs.app (fun m -> m "Client disconnected");
            Lwt.return_unit)
  in
  (* Set async exception hook so we error handle better *)
  set_async_exception_hook ();
  rpc_loop server

(* See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification *)

let create caps capabilities =
  { session = Session.create caps capabilities; state = State.Uninitialized }
