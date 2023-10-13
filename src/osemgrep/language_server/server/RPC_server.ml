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
module Lwt_js = Js_of_ocaml_lwt.Lwt_js

(*****************************************************************************)
(* Server IO *)
(*****************************************************************************)

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
end) : LSIO = struct
  open
    Lsp.Io.Make
      (struct
        include Lwt

        module O = struct
          let ( let* ) x f = Lwt.bind x f
          let ( let+ ) x f = Lwt.map f x
        end

        let raise exn = Lwt.fail exn
      end)
      (I)

  let read () = read I.stdin
  let write = write I.stdout
end

let unset_io : (module LSIO) =
  (module struct
    let read () = Lwt.return None
    let write _ = Lwt.return ()
  end)

let io_ref : (module LSIO) ref = ref unset_io

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
let respond id json =
  let module Io = (val !io_ref : LSIO) in
  match json with
  | Some json ->
      let response = Response.ok id json in
      let packet = Packet.Response response in
      Io.write packet
  | None -> Lwt.return ()

(** Send a request to the client *)
let request request =
  let module Io = (val !io_ref : LSIO) in
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let request = SR.to_jsonrpc_request request (`String id) in
  let packet = Packet.Request request in
  let () = Lwt.async (fun () -> Io.write packet) in
  id

(** Send a notification to the client *)
let notify notification =
  let module Io = (val !io_ref : LSIO) in
  let notification = SN.to_jsonrpc notification in
  Logs.debug (fun m ->
      m "Sending notification %s"
        (Notification.yojson_of_t notification |> Yojson.Safe.pretty_to_string));
  let packet = Packet.Notification notification in
  Io.write packet

(** Send a bunch of notifications to the client *)
let batch_notify notifications =
  Logs.debug (fun m -> m "Sending notifications");
  Lwt.async (fun () -> Lwt_list.iter_s notify notifications)

let notify_show_message ~kind s =
  let notif =
    Server_notification.ShowMessage
      { ShowMessageParams.message = s; type_ = kind }
  in
  batch_notify [ notif ]

(** Show a little progress circle while doing thing. Returns a token needed to end progress*)
let create_progress title message =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let token = ProgressToken.t_of_yojson (`String id) in
  let progress =
    SR.WorkDoneProgressCreate (WorkDoneProgressCreateParams.create token)
  in
  let _ = request progress in
  let start =
    SN.Progress.Begin (WorkDoneProgressBegin.create ~message ~title ())
  in
  let progress = SN.WorkDoneProgress (ProgressParams.create token start) in
  let () = Lwt.async (fun () -> notify progress) in
  token

(** end progress circle *)
let end_progress token =
  let end_ = SN.Progress.End (WorkDoneProgressEnd.create ()) in
  let progress = SN.WorkDoneProgress (ProgressParams.create token end_) in
  Lwt.async (fun () -> notify progress)

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
          Lwt.async (fun () -> respond req.id response);
          server
      | _ ->
          Logs.warn (fun m -> m "Unhandled message");
          server
    in
    Lwt.return server

  let rec rpc_loop server () =
    let module Io = (val !io_ref : LSIO) in
    match server.state with
    | State.Stopped ->
        Logs.debug (fun m -> m "Server stopped");
        Lwt.return ()
    | _ -> (
        let%lwt client_msg = Io.read () in
        match client_msg with
        | Some msg ->
            let%lwt server = handle_client_message msg server in
            rpc_loop server ()
        | None ->
            Logs.debug (fun m -> m "Client disconnected");
            Lwt.return ())

  let start server = rpc_loop server ()

  (* See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification *)

  let create () =
    { session = Session.create capabilities; state = State.Uninitialized }
end
