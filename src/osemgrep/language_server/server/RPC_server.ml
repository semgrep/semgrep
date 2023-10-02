(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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
(* This module also provides some helper functions for the messsage *)
(* handler (Semgrep LS) to send notifications and requests to the *)
(* client. Any helper function should be LWT agnostic if possible. *)
(* This means wrapping it in Lwt.async, so the task will run but errors *)
(* will still surface *)
(*  *)
(* The hope here is that anything that isn't Semgrep *)
(* specific, but Language Server related, goes in here. The other goal *)
(* is that this contains as much of the Lwt code possible *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Jsonrpc
open Lsp
open Types
module SR = Server_request
module SN = Server_notification
module CR = Client_request
module CN = Client_notification

(*****************************************************************************)
(* Server IO *)
(*****************************************************************************)
module Io =
  Lsp.Io.Make
    (struct
      include Lwt

      module O = struct
        let ( let* ) x f = Lwt.bind x f
        let ( let+ ) x f = Lwt.map f x
      end

      let raise exn = Lwt.fail exn
    end)
    (struct
      type input = Lwt_io.input_channel
      type output = Lwt_io.output_channel

      let read_line = Lwt_io.read_line_opt
      let write = Lwt_io.write

      let read_exactly inc n =
        let rec read_exactly acc n =
          if n = 0 then
            let result = String.concat "" (List.rev acc) in
            Lwt.return (Some result)
          else
            let%lwt line = Lwt_io.read ~count:n inc in
            read_exactly (line :: acc) (n - String.length line)
        in
        read_exactly [] n
    end)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

module State = struct
  type t = Uninitialized | Running | Stopped
end

type t = { session : Session.t; state : State.t }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Why the atomic writes below? The LSP library we use does something weird, *)
(* it writes the jsonrpc header then body with seperate calls to write, which *)
(* means there's a race condition there. The below atomic calls ensures that *)
(* the ENTIRE packet is written at the same time *)
let respond server id json =
  match json with
  | Some json ->
      let response = Response.ok id json in
      let packet = Packet.Response response in
      Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing
  | None -> Lwt.return ()

(** Send a request to the client *)
let request server request =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let request = SR.to_jsonrpc_request request (`String id) in
  let packet = Packet.Request request in
  let () =
    Lwt.async (fun () ->
        Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing)
  in
  id

(** Send a notification to the client *)
let notify server notification =
  let notification = SN.to_jsonrpc notification in
  Logs.app (fun m ->
      m "Sending notification %s"
        (Notification.yojson_of_t notification |> Yojson.Safe.pretty_to_string));
  let packet = Packet.Notification notification in
  let%lwt () =
    Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing
  in
  Lwt_io.flush_all ()

(** Send a bunch of notifications to the client *)
let batch_notify server notifications =
  Logs.app (fun m -> m "Sending notifications");
  Lwt.async (fun () -> Lwt_list.iter_s (notify server) notifications)

(** Show a little progress circle while doing thing. Returns a token needed to end progress*)
let create_progress server title message =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let token = ProgressToken.t_of_yojson (`String id) in
  let progress =
    SR.WorkDoneProgressCreate (WorkDoneProgressCreateParams.create token)
  in
  let _ = request server progress in
  let start =
    SN.Progress.Begin (WorkDoneProgressBegin.create ~message ~title ())
  in
  let progress = SN.WorkDoneProgress (ProgressParams.create token start) in
  let () = Lwt.async (fun () -> notify server progress) in
  token

(** end progress circle *)
let end_progress server token =
  let end_ = SN.Progress.End (WorkDoneProgressEnd.create ()) in
  let progress = SN.WorkDoneProgress (ProgressParams.create token end_) in
  Lwt.async (fun () -> notify server progress)

(*****************************************************************************)
(* Server *)
(*****************************************************************************)

(* Functor !!! Scary D:. Why are we using a functor here? Well it's much *)
(* cleanr to split up semgrep specific stuff (handling of messages), and *)
(* the rest of the JSONRPC + LSP stuff. But still, why functors? Well the official *)
(* OCaml LS does it this way, and this feels a lot more readable than having *)
(* to create a server and pass it functions as params *)
module Make (MessageHandler : sig
  val on_request : _ CR.t -> t -> Json.t option * t
  val on_notification : CN.t -> t -> t
  val capabilities : ServerCapabilities.t
end) =
struct
  open MessageHandler

  let handle_client_message (msg : Packet.t) server =
    let server =
      match msg with
      | Notification n when CN.of_jsonrpc n |> Result.is_ok ->
          on_notification (CN.of_jsonrpc n |> Result.get_ok) server
      | Request req when CR.of_jsonrpc req |> Result.is_ok ->
          let (CR.E req_unpacked) = CR.of_jsonrpc req |> Result.get_ok in
          let response, server = on_request req_unpacked server in
          Lwt.async (fun () -> respond server req.id response);
          server
      | _ ->
          Logs.warn (fun m -> m "Unhandled message");
          server
    in
    Lwt.return server

  let rec rpc_loop server () =
    match server.state with
    | State.Stopped ->
        Logs.app (fun m -> m "Server stopped");
        Lwt.pause ()
    | _ -> (
        let%lwt () = Lwt.pause () in
        let%lwt client_msg = Io.read server.session.incoming in
        match client_msg with
        | Some msg ->
            let%lwt server = handle_client_message msg server in
            let%lwt () = Lwt.pause () in
            rpc_loop server ()
        | None ->
            Logs.app (fun m -> m "Client disconnected");
            Lwt.pause ())

  let start server = Lwt_main.run (rpc_loop server ())

  (* See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification *)

  let create () =
    { session = Session.create capabilities; state = State.Uninitialized }
end
