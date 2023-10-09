(* Brandon Wu
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

open Testutil
open Lsp
open Types
module Out = Semgrep_output_v1_t
module In = Input_to_core_t
module SR = Server_request
module CR = Client_request
module LanguageServer = LS.LanguageServer
open Jsonrpc

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type info = {
  server : RPC_server.t;
  in_begin : Lwt_io.output_channel;
  out_end : Lwt_io.input_channel;
}

(* The picture looks like this:

                  in_begin               in_end
       client         --------(in)-------->        server
  (response loop)     <-------(out)--------      (rpc loop)
                  out_end                out_begin

 *)
let create_info () =
  let in_end, in_begin = Unix.pipe () in
  let out_end, out_begin = Unix.pipe () in
  let in_end, in_begin =
    ( Lwt_io.of_unix_fd ~mode:Lwt_io.Input in_end,
      Lwt_io.of_unix_fd ~mode:Lwt_io.Output in_begin )
  in
  let out_end, out_begin =
    ( Lwt_io.of_unix_fd ~mode:Lwt_io.Input out_end,
      Lwt_io.of_unix_fd ~mode:Lwt_io.Output out_begin )
  in
  let server = LanguageServer.create () in
  let server =
    {
      server with
      session =
        { server.session with incoming = in_end; outgoing = out_begin };
    }
  in
  { server; in_begin; out_end }

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let default_content = {|
x = 0
if x == 5: # auto rule
    print("hello")
if x == 4: # CI rule
    print("hello")
|}

(*****************************************************************************)
(* Core primitives *)
(*****************************************************************************)

let send (info : info) packet : unit Lwt.t =
  match info.server.state with
  | RPC_server.State.Stopped ->
      Alcotest.failf "Cannot send, server stopped"
  | _ ->
      let%lwt () = Lwt.pause () in
      let%lwt () = RPC_server.Io.write info.in_begin packet in
      Lwt.return ()

let receive (info : info) : Json.t Lwt.t =
  match info.server.state with
  | RPC_server.State.Stopped ->
      Alcotest.failf "Cannot receive, server stopped"
  | _ ->
      let%lwt () = Lwt.pause () in
      let%lwt server_msg = RPC_server.Io.read info.out_end in
      match server_msg with
      (* Per the `respond` function in RPC_server, we always
         inject into Response with an Ok variant for the result.
       *)
      | Some (Response {id = _; result = Ok json}) ->
        Lwt.return json
      | _ ->
        Alcotest.failf "Received no response, client disconnected"

(** Everything this server supports from the LSP *)
let default_capabilities = ClientCapabilities.create ()

let of_request request =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let packet =
    Packet.Request (CR.to_jsonrpc_request request (`String id))
  in
  packet

let of_notification notif =
  let packet =
    Packet.Notification (Client_notification.to_jsonrpc notif)
  in
  packet

let assert_contains (json : Json.t) str =
  let json_str = (Yojson.Safe.to_string json) in
  if Common.contains json_str str then
    ()
  else
    Alcotest.failf "Expected string `%s` in response %s" str json_str

let mock_files () =
  let git_tmp_path = failwith "TODO" in
  let open Fpath in
  let checked_command s =
    if Sys.command s <> 0 then
      failwith (Common.spf "command %s exited with non-zero code" s)
  in
  let open_and_write file =
    let oc = open_out (to_string file) in
    output_string oc default_content
  in

  let root = git_tmp_path in
  (* should have preexisting matches that are committed *)
  let modified_file = git_tmp_path / "modified.py" in
  (* should have preexisting matches that are not committed *)
  let existing_file = git_tmp_path / "existing.py" in

  open_and_write modified_file;
  open_and_write existing_file;

  checked_command (String.concat " " ["git"; "remote"; "add"; "origin"; "/tmp/origin"]);
  checked_command (String.concat " " ["git"; "add"; "modified.py"]);
  checked_command (String.concat " " ["git"; "add"; "existing.py"]);
  checked_command (String.concat " " ["git"; "commit"; "-m"; "initial commit"]);

  open_and_write modified_file;

  let new_file = root / "new.py" in
  open_and_write new_file;

  (* THINK: sort?*)
  git_tmp_path, [existing_file; modified_file; new_file]

let mock_workspaces () =
  let _mock_files = mock_files () in
  Common2.with_tmp_dir (fun tmp_path ->
    (* Copy mock files to a second workspace
       This is gross IK but oh well
    *)
    let _workspace2_root = tmp_path ^ "1" in
    ()
  )


(*****************************************************************************)
(* Basic helpers *)
(*****************************************************************************)

let send_exit info =
  let packet = of_request CR.Shutdown in
  RPC_server.Io.write info.in_begin packet

let send_initialize info =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let request =
    CR.Initialize (InitializeParams.create ~capabilities:default_capabilities ())
  in
  let packet =
    Packet.Request (CR.to_jsonrpc_request request (`String id))
  in
  RPC_server.Io.write info.in_begin packet

let send_custom_request ~meth ?(params = None) info =
  let packet =
    let id = Uuidm.v `V4 |> Uuidm.to_string in
    let request =
      CR.UnknownRequest { meth; params }
    in
    Packet.Request (CR.to_jsonrpc_request request (`String id))
  in
  send info packet

let with_session (f : info -> unit Lwt.t) : unit =
  let info = create_info () in
  (* shut down the server when f exits *)
  let finalized_f =
    Lwt.finalize (fun () -> f info)
    (fun () ->
        send_exit info
    )
  in
  Lwt_main.run (
    (* Here, we wait for both the test job and the server to terminate.
       This is OK to do, because we have finalized the `f` function to
       terminate the server upon its own termination.
     *)
    Lwt.both
     (LanguageServer.start_async info.server)
     finalized_f
  ) |> ignore;
  ()

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let login_tests () =
  let response_test () =
    with_session (fun info ->
      let%lwt () = send_initialize info in
      let%lwt msg = receive info in
      assert_contains msg "capabilities";

      let%lwt () = send_custom_request ~meth:"semgrep/login" info in
      let%lwt _msg = receive info in
      (* Common.(pr2 (spf "got msg %s" ([%show: Yojson.Safe.t] msg)));
      *)
      Lwt.return ()
    )
  in
  pack_tests "Response test" [ ("Response test again", response_test) ]

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests = pack_suites "Language Server (e2e)" [ login_tests () ]
