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
(* This module handles all incoming requests from the client *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Lsp
open Legacy_lsp_
open Types
open Jsonrpc
module CR = Client_request

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let process_json_result (req_id : Id.t)
    (session, (response_json : Yojson.Safe.t option)) =
  let reply =
    match response_json with
    | None -> Reply.empty
    | Some response_json -> Reply.now (respond_json req_id response_json)
  in
  (session, reply)

let process_result (req_id : Id.t) (request : 'r CR.t) (session, (response : 'r))
    =
  (session, Reply.now (respond req_id request response))

(* Dispatch to the various custom request handlers. *)
let handle_custom_request session (req_id : Id.t) (meth : string)
    (params : Structured.t option) : Legacy_session.t * Reply.t =
  match
    [
      (Legacy_search.start_meth, Legacy_search.start_search);
      (Legacy_search.ongoing_meth, Legacy_search.search_next_file);
      (Legacy_showast.meth, Legacy_showast.on_request);
      (Legacy_login_start.meth, Legacy_login_start.on_request);
      (Legacy_login_status.meth, Legacy_login_status.on_request);
      (Legacy_loginfinish.meth, Legacy_loginfinish.on_request);
    ]
    |> List.assoc_opt meth
  with
  | Some (handler : _ -> _ -> _ -> Legacy_session.t * Reply.t) ->
      handler session req_id params
  | None ->
      (* TODO: Notify client *)
      Logs.warn (fun m -> m "Unhandled custom request %s" meth);
      (session, Reply.empty)

let on_request (type r) server (req_id : Id.t) (request : r CR.t) :
    Legacy_rpc_server.t * Reply.t =
  Logs.debug (fun m ->
      m "Handling request:\n%s"
        (CR.to_jsonrpc_request request (`Int 0)
        |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
  match request with
  | CR.Initialize params -> (
      try
        Legacy_initialize_request.on_request server params
        |> process_result req_id request
      with
      | e ->
          let backtrace = Printexc.get_backtrace () in
          Logs.err (fun m ->
              m "Error initializing server: %s" (Printexc.to_string e));
          Logs.info (fun m -> m "Backtrace: %s" backtrace);
          let reply =
            Reply.later (fun send ->
                let%lwt () =
                  send
                    (Legacy_lsp_.log_error_to_client "Error initializing server"
                       e)
                in
                let result =
                  InitializeError.create ~retry:false
                  |> InitializeError.yojson_of_t |> respond_json req_id
                in
                send result)
          in
          (server, reply))
  | _ when server.state = State.Uninitialized ->
      Logs.err (fun m -> m "Server not initialized, ignoring request");
      (* Explicitly don't respond *)
      (server, Reply.empty)
  | CR.CodeAction params ->
      Legacy_code_actions.on_request server params
      |> process_result req_id request
  | TextDocumentHover params ->
      Legacy_hover_request.on_request server params
      |> process_json_result req_id
  | CR.ExecuteCommand { arguments; command; _ } ->
      let args = Option.value arguments ~default:[] in
      let session, reply_opt =
        Legacy_execute_command.handle_execute_request server.session command
          args
      in
      ({ server with session }, Option.value reply_opt ~default:Reply.empty)
  | CR.UnknownRequest { meth; params } ->
      (* Could be handled better but :shrug: *)
      if meth = Legacy_login_start.meth && Semgrep_login.is_logged_in_weak ()
      then
        let reply =
          Reply.now
            (Legacy_lsp_.notify_show_message ~kind:MessageType.Info
               "Already logged in to Semgrep Code")
        in
        (server, reply)
      else
        let session, reply =
          handle_custom_request server.session req_id meth params
        in
        ({ server with session }, reply)
  | CR.Shutdown ->
      Logs.app (fun m -> m "Shutting down server");
      Legacy_session.save_local_skipped_fingerprints server.session;
      (server, Reply.empty)
  | CR.DebugEcho params -> process_result req_id request (server, params)
  | _ ->
      Logs.debug (fun m ->
          m "Unhandled request %s"
            (CR.to_jsonrpc_request request (`Int 0)
            |> Request.yojson_of_t |> Yojson.Safe.pretty_to_string));
      (* TODO: error response, log to client *)
      (server, Reply.empty)
