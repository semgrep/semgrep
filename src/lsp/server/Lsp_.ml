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
(* Coupling: See RPC_server.ml *)
(* This module provides some helper functions for the messsage *)
(* handler in RPC_server. This module is essentially some missing functions *)
(* from the LSP library, and is heavily inspired by lsp-fiber and jsonrpc-fiber *)
(* See: https://github.com/ocaml/ocaml-lsp/ lsp-fiber and jsonrpc-fiber *)
(* Any helper function must be LWT agnostic, except reply helpers. *)

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
(* Types *)
(*****************************************************************************)

module State = struct
  type t = Uninitialized | Running | Stopped
end

(* See mli for a description *)
module Reply = struct
  type send = Packet.t -> unit Lwt.t
  type t = Now of Packet.t | Later of (send -> unit Lwt.t)

  let now r = Now r
  let later (f : send -> unit Lwt.t) = Later f
  let empty = Later (fun _ -> Lwt.return_unit)

  let apply (f : send) (x : t) =
    match x with
    | Now r -> f r
    | Later f' -> f' f

  let both (r1 : t) (r2 : t) =
    later (fun f ->
        let%lwt () = apply f r1 in
        apply f r2)
end

type error_response = { message : string; name : string; stack : string }
[@@deriving yojson]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error_response_of_exception message e =
  let name = Printexc.to_string e in
  let stack = Printexc.get_backtrace () in
  { message; name; stack }

let packet_of_notification (n : Notification.t) = Packet.Notification n
let packet_of_response (r : Response.t) = Packet.Response r
let packet_of_request (req : Request.t) = Packet.Request req
let respond_json id result = packet_of_response (Response.ok id result)

let respond (type r) (id : Id.t) (request : r CR.t) (response : r) =
  response |> CR.yojson_of_result request |> respond_json id

(** Send a request to the client *)
let request request =
  let id =
    Uuidm.v4_gen (Stdlib.Random.State.make_self_init ()) () |> Uuidm.to_string
  in
  let request = SR.to_jsonrpc_request request (`String id) in
  Logs.debug (fun m ->
      m "Sending request %s"
        (request |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
  (packet_of_request request, id)

(** Send a notification to the client *)
let notify notification =
  let notification = SN.to_jsonrpc notification in
  Logs.debug (fun m ->
      m "Sending notification %s"
        (Notification.yojson_of_t notification |> Yojson.Safe.pretty_to_string));
  packet_of_notification notification

let notify_custom ?params method_ =
  Logs.debug (fun m -> m "Sending custom notification %s" method_);
  let jsonrpc_notif = Jsonrpc.Notification.create ~method_ ?params () in
  let server_notif = SN.of_jsonrpc jsonrpc_notif in
  Result.map notify server_notif

(** Send a bunch of notifications to the client *)
let batch_notify notifications =
  Logs.debug (fun m -> m "Sending notifications");
  List_.map notify notifications

let notify_show_message ~kind s =
  let notif =
    Server_notification.ShowMessage
      { ShowMessageParams.message = s; type_ = kind }
  in
  notify notif

(** Show a little progress circle while doing thing. Returns a token needed to end progress*)
let create_progress title message =
  let id =
    Uuidm.v4_gen (Stdlib.Random.State.make_self_init ()) () |> Uuidm.to_string
  in
  Logs.debug (fun m ->
      m "Creating progress token %s, (%s: %s)" id title message);
  let token = ProgressToken.t_of_yojson (`String id) in
  let progress =
    SR.WorkDoneProgressCreate (WorkDoneProgressCreateParams.create token)
  in
  let request, _id = request progress in
  let start =
    SN.Progress.Begin (WorkDoneProgressBegin.create ~message ~title ())
  in
  let progress = SN.WorkDoneProgress (ProgressParams.create token start) in
  ([ request; notify progress ], token)

(** end progress circle *)
let end_progress token =
  Logs.debug (fun m ->
      m "Ending progress token %s"
        (token |> ProgressToken.yojson_of_t |> Yojson.Safe.pretty_to_string));
  let end_ = SN.Progress.End (WorkDoneProgressEnd.create ()) in
  let progress = SN.WorkDoneProgress (ProgressParams.create token end_) in
  notify progress

let log_error_to_client msg exn =
  (* Let's use LogMessage since it has a nice setup anyways *)
  let message =
    error_response_of_exception msg exn |> error_response_to_yojson
  in
  (* We use telemetry notification here since that's basically what this is,
     and on the client side, nothing listens to this by default, and we can pass
     arbitrary data *)
  let notif = SN.TelemetryNotification message in
  notify notif

let notify_and_log_error msg exn =
  let trace = Printexc.get_backtrace () in
  let exn_str = Printexc.to_string exn in
  Logs.err (fun m -> m "%s: %s" msg exn_str);
  Logs.info (fun m -> m "Backtrace:\n%s" trace);
  let log_pkt = log_error_to_client msg exn in
  let show_pkt = notify_show_message ~kind:MessageType.Error exn_str in
  [ log_pkt; show_pkt ]

let error_response_of_exception id e =
  let error = Response.Error.of_exn e in
  packet_of_response (Response.error id error)
