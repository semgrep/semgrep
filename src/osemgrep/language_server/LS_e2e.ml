open Testutil
open Lsp
open Types
module Out = Semgrep_output_v1_t
module In = Input_to_core_t
module SR = Server_request
module CR = Client_request
module LanguageServer = LS.LanguageServer
open Jsonrpc

(* The internal representation of an `Lwt_io.input_channel` and
   `Lwt_io.output_channel` are the same. They are *)
let w_incoming = (Obj.magic Lwt_io.stdin : Lwt_io.output_channel)
let r_outgoing = (Obj.magic Lwt_io.stdout : Lwt_io.input_channel)

let response_loop (server : RPC_server.t) out_end () =
  match server.state with
  | RPC_server.State.Stopped ->
      Logs.app (fun m -> m "Server stopped");
      Lwt.return false
  | _ -> (
      let%lwt () = Lwt.pause () in
      let%lwt server_msg = RPC_server.Io.read out_end in
      match server_msg with
      | Some _msg ->
          Common.(pr2 (spf "got a message"));
          let%lwt () = Lwt.pause () in
          Lwt.return false
      | None ->
          Common.(pr2 (spf "Client disconnected"));
          Lwt.return false)

(** Everything this server supports from the LSP *)
let capabilities = ClientCapabilities.create ()

let login_tests () =
  let response_test () =
    let ans =
      Lwt_main.run
        (let in_end, in_begin = Unix.pipe () in
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

         (* start the server *)
         Lwt.async (fun () -> LanguageServer.start_async server);

         let id = Uuidm.v `V4 |> Uuidm.to_string in
         let request =
           CR.Initialize (InitializeParams.create ~capabilities ())
         in
         let packet =
           Packet.Request (CR.to_jsonrpc_request request (`String id))
         in
         let%lwt () = RPC_server.Io.write in_begin packet in

         let id = Uuidm.v `V4 |> Uuidm.to_string in
         let request =
           CR.UnknownRequest { meth = "semgrep/login"; params = None }
         in
         let packet =
           Packet.Request (CR.to_jsonrpc_request request (`String id))
         in
         let%lwt () = RPC_server.Io.write in_begin packet in

         response_loop server out_end ())
    in
    Alcotest.(check bool) "" true ans
  in
  pack_tests "Response test" [ ("Response test again", response_test) ]

let tests = pack_suites "Language Server (e2e)" [ login_tests () ]
