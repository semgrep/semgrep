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

open Lsp
open Lsp_
open Types
open Jsonrpc
open Fpath_.Operators
module Env = Semgrep_envvars
module OutJ = Semgrep_output_v1_t
module SR = Server_request
module CR = Client_request
module CN = Client_notification
module YS = Yojson.Safe

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This file is just a port of what was previously `test_ls.py`, to OCaml.
   It should cover all the same tests that existed there.
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* The picture looks like this:

                   in_begin               in_end
        client         --------(in)-------->        server
   (response loop)     <-------(out)--------      (rpc loop)
                   out_end                out_begin

   We have the channels associated with the "in" pipe, which
   has to do with data coming "in" to the server.
   We also have the channels associated with the "out" pipe,
   which deals with data flowing "out" of the server.
*)

type server_info = { server : RPC_server.t; handler : RPC_server.handler }
type test_info = { server : server_info; root : Fpath.t }

let create_info caps =
  let server = RPC_server.create caps LS.capabilities in
  let handler : RPC_server.handler =
    {
      on_request = Request_handler.on_request;
      on_notification = Notification_handler.on_notification;
    }
  in
  { server; handler }

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* These tests are run separately from both the JS side and the regular Semgrep
   side.
*)

(*
   This makes a system call that interferes with lwt's event loop.
   Be careful.
*)
let project_root () =
  match Git_wrapper.project_root_for_files_in_dir Fpath_.current_dir with
  | Some path ->
      let oss_path = path / "OSS" in
      if Sys.file_exists !!oss_path then oss_path else path
  | None ->
      (* Deal with the case where we're not a in git repo:
           if it looks like we're at the project root, we're happy. *)
      if Sys.file_exists "CONTRIBUTING.md" then Fpath.v (Sys.getcwd ())
      else
        failwith
          "You must run the test program from within the semgrep repo and not \
           one of its subfolders or submodules."

let get_rule_path () =
  match Git_wrapper.project_root_for_files_in_dir Fpath_.current_dir with
  | Some root -> root // Fpath.v "cli/tests/default/e2e/targets/ls/rules.yaml"
  | None ->
      failwith "The test program must run from within the semgrep git project"

(* We run this here because we can't run it during lwt's event loop. *)
let rule_path = get_rule_path ()

let default_content =
  {|
x = 0
if x == 5: # auto rule
    print("hello")
if x == 4: # CI rule
    print("hello")
|}

let login_url_regex =
  Pcre2_.pcre_compile {|https://semgrep.dev/login\?cli-token=.*"|}

let prog_regex = Pcre2_.pcre_compile {|Pr([\s\S]*)|}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let checked_command s =
  if Sys.command s <> 0 then
    Alcotest.failf "command %s exited with non-zero code" s

let open_and_write_default_content ?(mode = []) file =
  let oc = open_out_gen (Open_creat :: mode) 0o777 (Fpath.to_string file) in
  output_string oc default_content;
  close_out oc

let expect_one msg = function
  | [ x ] -> x
  | _ -> Alcotest.fail msg

let expect_one_msg xs = expect_one "Expected exactly one message from server" xs
let expect_empty msg list = Alcotest.(check int) msg 0 (List.length list)
let expect_empty_msg xs = expect_empty "Expected no messages from server" xs

let expect_object_field field_name (json : YS.t) =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt field_name fields with
      | Some value -> value
      | None -> Alcotest.fail (Printf.sprintf "Expected field %s" field_name))
  | _ -> Alcotest.fail "Expected object"

let expect_params_field field_name (params : Structured.t option) =
  match params with
  | Some (`Assoc _ as obj) -> expect_object_field field_name obj
  | _ -> Alcotest.fail (Printf.sprintf "Expected params field %s" field_name)

let scanned_files_of_files files =
  List.filter
    (fun f -> not (String_.contains (Fpath.to_string f) ~term:"existing"))
    files

(*****************************************************************************)
(* Core primitives *)
(*****************************************************************************)

let send (info : server_info) packet : server_info * Reply.t =
  match info.server.state with
  | Lsp_.State.Stopped -> Alcotest.failf "Cannot send, server stopped"
  | _ -> (
      let server, reply_result =
        RPC_server.handle_client_message ~handler:info.handler info.server
          packet
      in
      match reply_result with
      | Ok reply -> ({ info with server }, reply)
      | Error e -> Alcotest.failf "Error sending packet: %s" e)

let send_map (type a) (info : server_info) packet (f : Packet.t -> a) :
    (server_info * a list) Lwt.t =
  (* server may or may not be mutated at this point *)
  Logs.app (fun m ->
      m "Sending packet: %a" Yojson.Safe.pp (Packet.yojson_of_t packet));
  let info, reply = send info packet in
  let result : a list ref = ref [] in
  let%lwt () =
    Reply.apply
      (fun packet ->
        Logs.app (fun m ->
            m "Got result: %a" Yojson.Safe.pp (Packet.yojson_of_t packet));
        let res = f packet in
        result := res :: !result;
        Lwt.return_unit)
      reply
  in
  (* When this promise is resolved, it's guaranteed that the server has
     finished processing the packet we sent and has performed all side effects. *)
  Lwt.return (info, List.rev !result)

(*****************************************************************************)
(* Specific send/receive functions *)
(*****************************************************************************)

let send_request info request =
  let id =
    Uuidm.v4_gen (Stdlib.Random.State.make_self_init ()) () |> Uuidm.to_string
  in
  let packet = Packet.Request (CR.to_jsonrpc_request request (`String id)) in
  send_map info packet

let send_custom_request ~meth ?params info =
  send_request info (CR.UnknownRequest { meth; params })

let send_notification info (notification : Client_notification.t) =
  let packet =
    Packet.Notification (Client_notification.to_jsonrpc notification)
  in
  send_map info packet

let send_notification_map info notif =
  let packet = Packet.Notification (Client_notification.to_jsonrpc notif) in
  send_map info packet

let send_custom_notification ~meth ?params info =
  send_notification info (CN.UnknownNotification { method_ = meth; params })

let receive_response (packet : Packet.t) : Response.t =
  match packet with
  | Packet.Response resp -> resp
  | _ ->
      Alcotest.failf "expected valid response, got %s"
        (Packet.yojson_of_t packet |> YS.to_string)

(* This function simply preprocesses the result inside of the response with
   a specific function before returning, instead of leaving it to be
   unpacked later.
*)
let receive_response_result (f : Json.t -> 'a) (packet : Packet.t) : 'a =
  let resp = receive_response packet in
  resp.result |> Result.get_ok |> f

let receive_notification (packet : Packet.t) : Notification.t =
  match packet with
  | Packet.Notification notif -> notif
  | _ ->
      Alcotest.failf "expected notification, got %s"
        (Packet.yojson_of_t packet |> YS.to_string)

(* This function simply preprocesses the parameters of the notification
   with a specific function before returning, instead of leaving it to be
   unpacked later.
*)
let receive_notification_params (f : YS.t -> 'a) (packet : Packet.t) : 'a =
  let notif = receive_notification packet in
  f (notif.params |> Option.get |> Structured.yojson_of_t)

let receive_request (packet : Packet.t) : Request.t =
  match packet with
  | Packet.Request req -> req
  | _ ->
      Alcotest.failf "expected request, got %s"
        (Packet.yojson_of_t packet |> YS.to_string)

let receive_diagnostics =
  receive_notification_params PublishDiagnosticsParams.t_of_yojson

let receive_and_sort_diagnostics packets =
  packets
  |> List_.map receive_diagnostics
  |> List.sort
       (fun (x : PublishDiagnosticsParams.t) (y : PublishDiagnosticsParams.t) ->
         String.compare
           (DocumentUri.to_string x.uri)
           (DocumentUri.to_string y.uri))

let ids_of_params (params : PublishDiagnosticsParams.t) =
  List_.map (fun d -> d.Diagnostic.code) params.diagnostics

let receive_diagnostic_ids packet =
  let params = receive_diagnostics packet in
  ids_of_params params

(*****************************************************************************)
(* Mocking and testing functions *)
(*****************************************************************************)

let with_git_tmp_path f =
  let _orig_dir = Sys.getcwd () in
  let root =
    Testutil_files.with_tempdir ~persist:true (fun dir ->
        Git_wrapper.init ~cwd:dir ();
        Git_wrapper.config_set ~cwd:dir "user.email" "baselinetest@semgrep.com";
        Git_wrapper.config_set ~cwd:dir "user.name" "Baseline Test";
        dir)
  in
  Sys.chdir (Fpath.to_string root);
  let%lwt () = f root in
  Sys.chdir _orig_dir;
  (* Clean up the git repo *)
  FileUtil.rm ~recurse:true [ Fpath.to_string root ];
  Lwt.return_unit

let assert_contains (json : Json.t) str =
  let json_str = YS.to_string json in
  if not (String_.contains ~term:str json_str) then
    Alcotest.failf "Expected string `%s` in response %s" str json_str

let mock_files root : Fpath.t list =
  let open Fpath in
  (* should have preexisting matches that are committed *)
  let modified_file = root / "modified.py" in
  (* should have preexisting matches that are not committed *)
  let existing_file = root / "existing.py" in

  open_and_write_default_content ~mode:[ Open_wronly ] modified_file;
  open_and_write_default_content ~mode:[ Open_wronly ] existing_file;

  checked_command
    (* nosem *)
    (String.concat " "
       [
         "git";
         "-C";
         Fpath.to_string root;
         "remote";
         "add";
         "origin";
         (* nosem *)
         "/tmp/origin";
       ]);
  Git_wrapper.add ~cwd:root [ existing_file; modified_file ];
  Git_wrapper.commit ~cwd:root "initial commit";
  open_and_write_default_content ~mode:[ Open_append ] modified_file;

  let new_file = root / "new.py" in
  (* created after commit *)
  open_and_write_default_content ~mode:[ Open_wronly ] new_file;

  let files =
    [ existing_file; modified_file; new_file ]
    |> List.sort (fun x y ->
           String.compare (Fpath.to_string x) (Fpath.to_string y))
  in
  files

let mock_workspaces root =
  let workspace1_files = mock_files root in
  let workspace1_root = Fpath.to_string root in

  (* Copy mock files to a second workspace
      This is gross IK but oh well
  *)
  (* we needed to make this longer, or the ordering assumptions later become tricky *)
  (* In Python, the tmp_path generated is _always_ `test_ls_multi0`.
     We want `workspace2_root` to be lexicographically after `workspace1_root`, however,
     The easy way would be to simply append to the end of the string. For instance,
     workspace1_root = "abc"
     workspace2_root = "abc0"

     However, there is a later part of this test which checks whether something is
     from `workspace1_root`... by checking if `workspace1_root` is contained within
     it.

     Clearly, this will pass that check and thus cause undesired behavior. So we need
     a `workspace2_root` that is lexicographically later, without containing
     `workspace1_root`.

     We can achieve that by simply taking workspace1_root, slicing a bit off the end,
     and then putting "z" at the end. This works because the folder should only generate
     folder names which use the letters a-e.
  *)
  let workspace2_root =
    Str.first_chars workspace1_root (String.length workspace1_root - 1) ^ "z"
  in
  FileUtil.cp ~recurse:true [ workspace1_root ] workspace2_root;

  let workspace2_files =
    List_.map
      (fun file -> Fpath.(v workspace2_root / filename file))
      workspace1_files
    |> List.sort (fun x y ->
           String.compare (Fpath.to_string x) (Fpath.to_string y))
  in

  ( (Fpath.v workspace1_root, workspace1_files),
    (Fpath.v workspace2_root, workspace2_files) )

let mock_search_files root : Fpath.t list =
  let path1 = root / "a" / "b" / "c.py" in
  let path2 = root / "test.py" in
  (* should have preexisting matches that are committed *)
  Unix.mkdir (root / "a" |> Fpath.to_string) 0o777;
  Unix.mkdir (root / "a" / "b" |> Fpath.to_string) 0o777;
  open_and_write_default_content ~mode:[ Open_wronly ] path1;
  open_and_write_default_content ~mode:[ Open_wronly ] path2;

  [ path1; path2 ]

(*****************************************************************************)
(* Sending functions *)
(*****************************************************************************)

let send_exit info =
  let%lwt info, empty =
    send_notification info Client_notification.Exit Fun.id
  in
  expect_empty_msg empty;
  Lwt.return info

let send_initialize info ?(only_git_dirty = true) workspaceFolders =
  let request =
    let rootUri =
      match workspaceFolders with
      | [] -> None
      | f :: _ -> Some (Uri.of_path (Fpath.to_string f))
    in
    let workspaceFolders =
      Some
        (workspaceFolders
        |> List_.map (fun f ->
               let f = Fpath.to_string f in
               { Types.WorkspaceFolder.uri = Uri.of_path f; name = f }))
    in
    let initializationOptions =
      `Assoc
        [
          ( "scan",
            `Assoc
              [
                ( "configuration",
                  `List [ `String (rule_path |> Fpath.to_string) ] );
                ("exclude", `List []);
                ("include", `List []);
                ("jobs", `Int 1);
                ("maxMemory", `Int 0);
                ("maxTargetBytes", `Int 0);
                ("onlyGitDirty", `Bool only_git_dirty);
                ("ci", `Bool false);
              ] );
          ("trace", `Assoc [ ("server", `String "verbose") ]);
          ( "metrics",
            `Assoc [ ("enabled", `Bool false); ("isNewAppInstall", `Bool true) ]
          );
          ("doHover", `Bool true);
        ]
    in
    CR.Initialize
      (InitializeParams.create ~processId:1234
         ~clientInfo:
           (InitializeParams.create_clientInfo ~name:"Visual Studio Code"
              ~version:Version.version ())
         ~locale:"en-us" ~rootPath:(Some "") (* THINK? *)
         ?rootUri (* THINK? *)
         ~workspaceFolders ~initializationOptions
         ~capabilities:(ClientCapabilities.create ())
         ())
  in
  let%lwt info, responses = send_request info request receive_response in
  let response = expect_one_msg responses in
  assert_contains (Response.yojson_of_t response) "capabilities";
  Lwt.return info

let send_initialized info =
  let notif = CN.Initialized in
  send_notification info notif

let send_did_open info (path : Fpath.t) =
  let path = Fpath.to_string path in
  let text = UFile.read_file (Fpath.v path) in
  let textDocument =
    TextDocumentItem.create ~languageId:"python" ~version:1
      ~uri:(Uri.of_path path) ~text
  in
  let notif =
    CN.TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument)
  in
  send_notification info notif

let send_did_save info (path : Fpath.t) =
  let textDocument =
    TextDocumentIdentifier.create ~uri:(Uri.of_path (Fpath.to_string path))
  in
  let notif =
    CN.DidSaveTextDocument (DidSaveTextDocumentParams.create ~textDocument ())
  in
  send_notification_map info notif

let send_did_add info (path : Fpath.t) =
  let path = Fpath.to_string path in
  let files = [ FileCreate.create ~uri:("file://" ^ path) ] in
  let notif = CN.DidCreateFiles (CreateFilesParams.create ~files) in
  send_notification_map info notif

let send_did_delete info (path : Fpath.t) =
  let path = Fpath.to_string path in
  let files = [ FileDelete.create ~uri:("file://" ^ path) ] in
  let notif = CN.DidDeleteFiles (DeleteFilesParams.create ~files) in
  send_notification_map info notif

(* Info comes last because otherwise the optional arguments are unerasable... shame. *)
let send_did_change_folder ?(added = []) ?(removed = []) info =
  let added =
    added |> List_.map Fpath.to_string
    |> List_.map (fun file ->
           WorkspaceFolder.create ~name:file ~uri:(Uri.of_path file))
  in
  let removed =
    removed |> List_.map Fpath.to_string
    |> List_.map (fun file ->
           WorkspaceFolder.create ~name:file ~uri:(Uri.of_path file))
  in
  let notif =
    CN.ChangeWorkspaceFolders
      (DidChangeWorkspaceFoldersParams.create
         ~event:(WorkspaceFoldersChangeEvent.create ~added ~removed))
  in
  send_notification info notif

let send_code_action ~(path : Fpath.t) ~diagnostics ~line_start ~char_start
    ~line_end ~char_end info =
  let textDocument =
    TextDocumentIdentifier.create ~uri:(Uri.of_path (Fpath.to_string path))
  in
  let context = CodeActionContext.create ~diagnostics () in
  let range =
    Range.create
      ~start:(Position.create ~character:char_start ~line:line_start)
      ~end_:(Position.create ~character:char_end ~line:line_end)
  in
  let req =
    CR.CodeAction (CodeActionParams.create ~context ~range ~textDocument ())
  in
  send_request info req

let send_execute_command ~command ~arguments info =
  let req =
    CR.ExecuteCommand (ExecuteCommandParams.create ~command ~arguments ())
  in
  send_request info req

let send_hover info (path : Fpath.t) ~character ~line =
  let textDocument =
    TextDocumentIdentifier.create ~uri:(Uri.of_path (Fpath.to_string path))
  in
  let position = Position.create ~character ~line in
  let req =
    CR.TextDocumentHover (HoverParams.create ~position ~textDocument ())
  in
  send_request info req

let _send_semgrep_login info = send_custom_request info ~meth:"semgrep/login"

let _send_semgrep_login_finish info =
  let params = `Assoc [ ("url", `String ""); ("sessionId", `String "") ] in
  send_custom_notification info ~meth:"semgrep/loginFinish" ~params

let _send_semgrep_logout info =
  send_custom_notification info ~meth:"semgrep/logout"

let send_semgrep_scan_workspace ?(full = false) info =
  send_custom_notification info ~meth:"semgrep/scanWorkspace"
    ~params:(`Assoc [ ("full", `Bool full) ])

let _send_semgrep_refresh_rules info =
  send_custom_notification info ~meth:"semgrep/refreshRules"

let send_semgrep_search info ?language ~includes ~excludes pattern =
  let params =
    Search.mk_params ~lang:language ~fix:None ~includes ~excludes pattern
  in
  send_custom_request info ~meth:"semgrep/search" ~params

let send_semgrep_search_ongoing info =
  send_custom_request info ~meth:"semgrep/searchOngoing"
    ~params:(Jsonrpc.Structured.t_of_yojson (`Assoc []))

let send_semgrep_show_ast info ?(named = false) (path : Fpath.t) =
  let uri = Uri.of_path (Fpath.to_string path) |> Uri.yojson_of_t in
  let params = `Assoc [ ("uri", uri); ("named", `Bool named) ] in
  send_custom_request info ~meth:"semgrep/showAst" ~params

(*****************************************************************************)
(* Checking functions *)
(*****************************************************************************)

let check_diagnostics (file : Fpath.t) expected_ids
    (diagnostics : PublishDiagnosticsParams.t) =
  Alcotest.(check string)
    "uri is same as file"
    (Common.spf "file://%s" (Fpath.to_string file))
    (DocumentUri.to_string diagnostics.uri);
  let ids =
    diagnostics.diagnostics
    |> List_.map (fun (d : Diagnostic.t) ->
           d.code |> Option.get |> Id.yojson_of_t)
  in
  Alcotest.(check string)
    "diagnostics are cohesive"
    (YS.to_string (`List expected_ids))
    (YS.to_string (`List ids))

let assert_notif ?message ?kind meth (pkt : Packet.t) =
  let notif = receive_notification pkt in
  Alcotest.(check string) "methods should be same" meth notif.method_;
  (match message with
  | None -> ()
  | Some message ->
      let recv_message =
        notif.params
        |> expect_params_field "value"
        |> expect_object_field "message"
        |> YS.Util.to_string
      in
      Alcotest.(check string) "message should be same" message recv_message);
  match kind with
  | None -> ()
  | Some kind ->
      let recv_kind =
        notif.params
        |> expect_params_field "value"
        |> expect_object_field "kind" |> YS.Util.to_string
      in
      Alcotest.(check string) "kind should be same" kind recv_kind

let assert_request ?message ?kind meth (pkt : Packet.t) =
  let open YS.Util in
  let req = receive_request pkt in
  assert (req.method_ = meth);
  (match message with
  | None -> ()
  | Some message ->
      let recv_message =
        req.params
        |> expect_params_field "value"
        |> expect_object_field "message"
      in
      Alcotest.(check string)
        "message should be same" message (to_string recv_message));
  match kind with
  | None -> ()
  | Some kind ->
      let recv_kind =
        req.params
        |> expect_params_field "value"
        |> expect_object_field "kind" |> to_string
      in
      Alcotest.(check string) "kind should be same" kind recv_kind

let assert_message message (pkt : Packet.t) =
  let notif = receive_notification pkt in
  Alcotest.(check string)
    "methods should be same" notif.method_ "window/showMessage";
  let recv_message =
    notif.params |> expect_params_field "message" |> YS.Util.to_string
  in
  Alcotest.(check string) "message should be same" message recv_message

let rec map_asserts (recv_asserts : (string * ('a -> unit)) list) (xs : 'a list)
    =
  match (recv_asserts, xs) with
  | [], [] -> []
  | (name, assert_) :: recv_asserts, pkt :: pkts ->
      Logs.app (fun m -> m "Asserting: %s" name);
      assert_ pkt;
      map_asserts recv_asserts pkts
  | [], pkts -> pkts
  | asserts, [] ->
      let names = List_.map fst asserts in
      Alcotest.failf "Expected more packets, got none. Expected: %s"
        (String.concat ", " names)

let assert_progress message =
  [
    ( "received progress request",
      assert_request "window/workDoneProgress/create" );
    ("received progress message", assert_notif "$/progress" ~message);
    ("receive progress end", assert_notif "$/progress" ~kind:"end");
  ]

let check_startup info folders (files : Fpath.t list) =
  (* initialize *)
  let%lwt info = send_initialize info folders in
  let scanned_files = scanned_files_of_files files in

  let%lwt info =
    Lwt_list.fold_left_s
      (fun info file ->
        let%lwt info, notifs = send_did_open info file receive_notification in
        (* TODO properly check this? *)
        ignore notifs;
        Lwt.return info)
      info scanned_files
  in
  let%lwt info, packets = send_initialized info Fun.id in
  let remaining_packets =
    map_asserts
      (assert_progress "Refreshing Rules"
      @ [ ("received rules refreshed", assert_notif "semgrep/rulesRefreshed") ]
      @ assert_progress "Scanning Open Documents")
      packets
  in
  let scan_notifications = receive_and_sort_diagnostics remaining_packets in
  let expected_ids = [ `String "eqeq-five" ] in
  let check_diagnostics_asserts =
    List_.map
      (fun file ->
        ( Printf.sprintf "Checking file %s has diagnostics"
            (Fpath.to_string file),
          check_diagnostics file expected_ids ))
      scanned_files
  in
  let leftover_packets =
    map_asserts check_diagnostics_asserts scan_notifications
  in
  Alcotest.(check int)
    "no leftover packets on startup" 0
    (List.length leftover_packets);
  Lwt.return info

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let do_search ?(pattern = "print(...)") ?(includes = []) ?(excludes = []) info =
  let get_matches (packet : Packet.t) =
    let resp = receive_response packet in
    YS.Util.(resp.result |> Result.get_ok |> member "locations" |> to_list)
  in
  let%lwt info, resps =
    send_semgrep_search info pattern ~includes ~excludes get_matches
  in
  let resp = expect_one_msg resps in
  let send_ongoing info =
    let%lwt info, resps = send_semgrep_search_ongoing info get_matches in
    Lwt.return (info, expect_one_msg resps)
  in
  let rec gather_matches info acc =
    let%lwt info, resp = send_ongoing info in
    match resp with
    | [] -> Lwt.return (info, acc)
    | _ -> gather_matches info (acc @ resp)
  in
  let%lwt info, matches = gather_matches info resp in
  Lwt.return (info, matches)

let with_session caps (f : test_info -> unit Lwt.t) : unit Lwt.t =
  (* Not setting this means that really nasty errors happen when an exception
     is raised inside of an Lwt.async, when running the Alcotests.
     As in, the tests will just exit with no error message at all.
  *)
  (Lwt.async_exception_hook :=
     fun exn ->
       let err = Printexc.to_string exn in
       let traceback = Printexc.get_backtrace () in
       Logs.err (fun m -> m "Got exception: %s" err);
       Logs.err (fun m -> m "Traceback:\n%s" traceback);
       Alcotest.fail "Got exception in Lwt.async during tests");
  (* we want to start every test logged out, or the LS won't behave
     predictably, because the testing structure assumes a fresh start
  *)
  Testutil_login.with_login_test_env ~chdir:false
    (fun () ->
      with_git_tmp_path (fun root ->
          let server_info = create_info caps in
          let test_info = { server = server_info; root } in
          f test_info))
    ()

let test_ls_specs caps () =
  with_session caps (fun { server = info; root } ->
      let files = mock_files root in
      let%lwt info = check_startup info [ root ] files in
      let%lwt info =
        Lwt_list.fold_right_s
          (fun file info ->
            (* didOpen *)
            let%lwt info, old_ids =
              send_did_save info file receive_diagnostic_ids
            in
            let old_ids = expect_one_msg old_ids in
            (* This sleep is to ensure that the subsequent write to
               `modified.py` results in changing the modification time of
               the file. Otherwise, we might actually load from the cached
               AST for the file, and not incorporate the new change.
            *)
            Unix.sleep 2;

            open_and_write_default_content ~mode:[ Open_append ] file;

            (* didSave *)
            let%lwt info, params =
              send_did_save info file receive_diagnostics
            in
            let params = expect_one_msg params in
            let new_ids = ids_of_params params in

            Alcotest.(check int)
              "new finding from modified file" (List.length new_ids)
              (List.length old_ids + 1);

            let diagnostic = Common2.list_last params.diagnostics in
            (* get range of last diagnostic *)
            let line_start = diagnostic.range.start.line in
            let char_start = diagnostic.range.start.character in
            let line_end = diagnostic.range.end_.line in
            let char_end = diagnostic.range.end_.character in

            (* get code actions *)
            let%lwt info, res =
              send_code_action info ~path:file ~diagnostics:[ diagnostic ]
                ~line_start ~char_start ~line_end ~char_end
                (receive_response_result CodeActionResult.t_of_yojson)
            in
            let res = expect_one_msg res in
            assert (List.length (Option.get res) = 2);
            let command =
              match res with
              | Some
                  [
                    `CodeAction { kind; _ };
                    `CodeAction { command = Some command; _ };
                  ] ->
                  assert (
                    CodeActionKind.yojson_of_t (Option.get kind)
                    = `String "quickfix");
                  assert (command.command = "semgrep/ignore");
                  assert (Option.is_some command.arguments);
                  command
              | _ -> Alcotest.fail "expected code action kind"
            in

            (* execute command *)
            let%lwt info, params =
              send_execute_command ~command:command.command
                ~arguments:(Option.get command.arguments)
                info
                (receive_notification_params
                   PublishDiagnosticsParams.t_of_yojson)
            in
            let params = expect_one_msg params in
            Logs.app (fun m ->
                m "Received diagnostics after ignore %a" Yojson.Safe.pp
                  (PublishDiagnosticsParams.yojson_of_t params));
            let not_ignored_ids =
              List_.map
                (fun d ->
                  Logs.app (fun m ->
                      m "Diagnostic: %a" Yojson.Safe.pp
                        (Diagnostic.yojson_of_t d));
                  d.Diagnostic.code)
                params.diagnostics
            in
            Alcotest.(check int)
              "new finding from modified file with ignored finding"
              (List.length new_ids - 1)
              (List.length not_ignored_ids);

            Lwt.return info)
          files info
      in

      (* test did add *)
      let added = Fpath.(root / "added.py") in
      (* nosem *)
      FileUtil.cp [ List.hd files |> Fpath.to_string ] (added |> Fpath.to_string);

      (* Tests target caching *)
      let%lwt info, empty = send_did_add info added Fun.id in

      expect_empty_msg empty;
      let%lwt info, diagnostics =
        send_did_open info added receive_diagnostics
      in
      let diagnostics = expect_one_msg diagnostics in

      check_diagnostics added
        [ `String "eqeq-five"; `String "eqeq-five" ]
        diagnostics;

      let%lwt info, diagnostics =
        send_did_delete info added receive_diagnostics
      in
      let diagnostics = expect_one_msg diagnostics in

      check_diagnostics added [] diagnostics;

      let%lwt info = send_exit info in
      ignore info;
      expect_empty_msg empty;
      Lwt.return_unit)

let test_ls_ext caps () =
  with_session caps (fun { server = info; root } ->
      let files = mock_files root in
      Testutil_files.with_chdir root (fun () ->
          let%lwt info = check_startup info [ root ] files in

          (* scan workspace *)
          let%lwt info, scan_messages =
            send_semgrep_scan_workspace info Fun.id
          in
          let leftover_packets =
            map_asserts (assert_progress "Scanning Workspace") scan_messages
          in

          let scanned_files = scanned_files_of_files files in
          let diagnostics = receive_and_sort_diagnostics leftover_packets in
          let num_diagnostics =
            List_.map
              (fun (params : PublishDiagnosticsParams.t) ->
                Logs.app (fun m ->
                    m "Received diagnostics: %a" Yojson.Safe.pp
                      (PublishDiagnosticsParams.yojson_of_t params));
                List.length params.diagnostics)
              diagnostics
          in
          (* In unit testing we check to make sure that targeting is ok, so
             this is just double checking. We also have a vscode test suite
             that tests this logic *)
          Alcotest.(check int)
            "number of scanned files is expected"
            (List.length scanned_files)
            (List.length num_diagnostics);

          (* scan workspace full *)
          let%lwt info, full_scan_messages =
            send_semgrep_scan_workspace ~full:true info Fun.id
          in
          let scan_warn_msg =
            "Scanning all files regardless of git status. These diagnostics \
             will persist until a file is edited. To default to always \
             scanning regardless of git status, please disable 'Only Git \
             Dirty' in settings"
          in
          let leftover_packets =
            map_asserts
              ([ ("Full workspace warning", assert_message scan_warn_msg) ]
              @ assert_progress "Scanning Workspace")
              full_scan_messages
          in
          let diagnostics = receive_and_sort_diagnostics leftover_packets in
          let assert_modified_diagnostics num_diagnostics
              (params : PublishDiagnosticsParams.t) =
            Logs.app (fun m ->
                m "Received diagnostics: %a" Yojson.Safe.pp
                  (PublishDiagnosticsParams.yojson_of_t params));
            let uri = DocumentUri.to_string params.uri in
            if String_.contains uri ~term:"existing" then
              Alcotest.(check int)
                "number of diagnostics is unchanged" num_diagnostics
                (List.length params.diagnostics)
            else
              Alcotest.(check int)
                "number of diagnostics is expected after modified"
                (num_diagnostics + 1)
                (List.length params.diagnostics)
          in
          let asserts =
            List_.map
              (fun n ->
                ("assert modified diagnostics", assert_modified_diagnostics n))
              (* 0 is new file, since it doesn't have any normally *)
              (num_diagnostics @ [ 0 ])
          in
          let leftover_packets = map_asserts asserts diagnostics in
          expect_empty "no leftover packets after workspace scan"
            leftover_packets;

          (* search *)
          let%lwt info, matches = do_search info in
          assert (matches |> List.length = 3);

          (* hover *)
          (* hover is on by default *)
          let check_hover file info =
            let%lwt info, resp =
              send_hover info file ~character:1 ~line:0
                (receive_response_result Hover.t_of_yojson)
            in
            let contents =
              let resp = expect_one_msg resp in
              match resp.contents with
              | `MarkupContent s -> MarkupContent.yojson_of_t s
              | `MarkedString s -> MarkedString.yojson_of_t s
              | `List s -> `List (List_.map MarkedString.yojson_of_t s)
            in
            (* Assert contents is non-empty *)
            Alcotest.(check bool)
              "hover contents is non-empty" true (contents <> `Null);
            Lwt.return info
          in
          let%lwt info = Lwt_list.fold_right_s check_hover files info in

          (* showAst *)
          let check_show_ast file info =
            let%lwt info, resp =
              send_semgrep_show_ast info file receive_response
            in
            let resp = expect_one_msg resp in
            let ast = resp.result |> Result.get_ok |> YS.Util.to_string in
            assert (Pcre2_.unanchored_match prog_regex ast);
            Lwt.return info
          in
          let%lwt info = Lwt_list.fold_right_s check_show_ast files info in
          let%lwt info = send_exit info in
          ignore info;
          Lwt.return_unit))

let test_ls_multi caps () =
  with_session caps (fun { server = info; root } ->
      let ( (workspace1_root, workspace1_files),
            (workspace2_root, workspace2_files) ) =
        mock_workspaces root
      in
      let workspace_folders = [ workspace1_root; workspace2_root ] in
      let files = workspace1_files @ workspace2_files in
      let scanned_files = scanned_files_of_files files in
      let%lwt info = check_startup info workspace_folders files in

      let%lwt info, packets =
        send_did_change_folder info ~removed:[ workspace1_root ] Fun.id
      in

      let leftover_packets =
        map_asserts (assert_progress "Scanning Workspace") packets
      in
      let scan_notifications = receive_and_sort_diagnostics leftover_packets in
      let check_diagnostics_count expected_ids_ws1 expected_ids_ws2 file =
        let expected_ids =
          if Fpath.is_prefix workspace1_root file then expected_ids_ws1
          else expected_ids_ws2
        in
        let check = check_diagnostics file expected_ids in
        ( Printf.sprintf "Checking file %s has diagnostics"
            (Fpath.to_string file),
          check )
      in
      let leftover_packets =
        let expected_ids_ws1 = [] in
        let expected_ids_ws2 = [ `String "eqeq-five" ] in
        map_asserts
          (List_.map
             (check_diagnostics_count expected_ids_ws1 expected_ids_ws2)
             scanned_files)
          scan_notifications
      in
      expect_empty "no leftover packets after workspace change" leftover_packets;

      let%lwt info, packets =
        send_did_change_folder info ~added:[ workspace1_root ] Fun.id
      in
      let leftover_packets =
        map_asserts (assert_progress "Scanning Workspace") packets
      in

      let scan_notifications = receive_and_sort_diagnostics leftover_packets in
      let leftover_packets =
        (* Files should be exactly the same as before *)
        let expected_ids_ws1 = [ `String "eqeq-five" ] in
        let expected_ids_ws2 = expected_ids_ws1 in
        map_asserts
          (List_.map
             (check_diagnostics_count expected_ids_ws1 expected_ids_ws2)
             scanned_files)
          scan_notifications
      in
      expect_empty "no leftover packets after workspace change" leftover_packets;

      let%lwt info = send_exit info in
      ignore info;
      Lwt.return_unit)

let _test_login caps () =
  with_session caps (fun { server = info; root } ->
      (* If we don't log out prior to starting this test, the LS will complain
         we're already logged in, and not display the correct behavior.
      *)
      let files = mock_files root in
      Testutil_files.with_chdir root (fun () ->
          let%lwt info = check_startup info [ root ] files in
          let%lwt info, resp =
            send_custom_request ~meth:"semgrep/login" info receive_response
          in
          let msg = expect_one_msg resp in

          let url =
            YS.Util.(msg.result |> Result.get_ok |> member "url" |> to_string)
          in

          assert (Pcre2_.unanchored_match login_url_regex url);
          let%lwt info = send_exit info in
          ignore info;
          Lwt.return_unit))

let test_ls_no_folders caps () =
  with_session caps (fun { server = info; _ } ->
      let%lwt info = check_startup info [] [] in

      let%lwt info = send_exit info in
      ignore info;
      Lwt.return_unit)

let test_search_includes_excludes caps () =
  with_session caps (fun { server = info; root } ->
      let files = mock_search_files root in

      let%lwt info = check_startup info [ root ] files in
      let%lwt info, matches = do_search ~pattern:"x = 0" info in
      Alcotest.(check int) "number of matches" 2 (List.length matches);

      let assert_with_includes_excludes ?(includes = []) ?(excludes = [])
          ~matches_test ~matches_c () =
        let%lwt info, matches =
          do_search ~includes ~excludes ~pattern:"x = 0" info
        in
        let num_matches =
          (if matches_test then 1 else 0) + if matches_c then 1 else 0
        in
        Alcotest.(check int)
          "number of matches" num_matches (List.length matches);
        if
          matches_test
          && not
               (List.exists
                  (fun m ->
                    String.ends_with ~suffix:"test.py"
                      YS.Util.(m |> member "uri" |> to_string))
                  matches)
        then Alcotest.failf "failed to find test.py for matches_test case";
        if
          matches_c
          && not
               (List.exists
                  (fun m ->
                    String.ends_with ~suffix:"a/b/c.py"
                      YS.Util.(m |> member "uri" |> to_string))
                  matches)
        then Alcotest.failf "failed to find test.py for matches_c case";
        (* We don't care about state in these tests *)
        ignore info;
        Lwt.return_unit
      in

      let%lwt () =
        assert_with_includes_excludes ~includes:[ "c.py" ] ~matches_c:true
          ~matches_test:false ()
      in
      let%lwt () =
        assert_with_includes_excludes ~excludes:[ "c.py" ] ~matches_c:false
          ~matches_test:true ()
      in
      let%lwt () =
        assert_with_includes_excludes ~includes:[ "test.py" ] ~matches_c:false
          ~matches_test:true ()
      in
      let%lwt () =
        assert_with_includes_excludes ~excludes:[ "test.py" ] ~matches_c:true
          ~matches_test:false ()
      in

      let%lwt () =
        assert_with_includes_excludes ~includes:[ "py" ] ~matches_c:false
          ~matches_test:false ()
      in

      let%lwt () =
        assert_with_includes_excludes ~includes:[ "b" ] ~matches_c:true
          ~matches_test:false ()
      in
      let%lwt () =
        assert_with_includes_excludes ~includes:[ "a" ] ~matches_c:true
          ~matches_test:false ()
      in
      let%lwt () =
        assert_with_includes_excludes ~excludes:[ "a" ] ~matches_c:false
          ~matches_test:true ()
      in
      let%lwt () =
        assert_with_includes_excludes ~includes:[ "a/b/c.py" ] ~matches_c:true
          ~matches_test:false ()
      in
      let%lwt info = send_exit info in
      ignore info;

      Lwt.return_unit)

let test_ls_delete_cache caps () =
  with_session caps (fun { server = info; root } ->
      (* Helpers *)
      (* a string -> Fpath.t helper *)
      let to_fpath_exn (s : string) : Fpath.t =
        Result.get_ok @@ Fpath.of_string s
      in
      let assert_cache_exists () =
        let cache_dir = !Env.v.user_dot_semgrep_dir / "cache" in
        if not (Sys.file_exists !!cache_dir) then
          Alcotest.fail "cache wasn't created!"
      in
      (* init. fileserver *)
      let files = mock_search_files root in
      let%lwt info = check_startup info [ root ] files in

      (* Since the goal of this test is testing how the LS handles filesystem
       * manipulation, modify the environment's $HOME & $SEMGREP to tmp/
       * & tmp/.semgrep
       *)
      let tmp = to_fpath_exn (Filename.get_temp_dir_name ()) in
      Env.v :=
        {
          !Env.v with
          user_home_dir = tmp;
          user_dot_semgrep_dir = tmp / ".semgrep";
        };

      (* do a simple search *)
      let%lwt info, matches = do_search ~pattern:"x = 0" info in

      (* Check if we created search files + shutdown to create a cache *)
      Alcotest.(check int) "number of matches" 2 (List.length matches);
      let%lwt _ = send_request info CR.Shutdown receive_response in

      (* ASSERT: cache exists after shutdown *)
      assert_cache_exists ();

      (* mimic deleting cache dir by changing .semgrep/ to .semgrepp/ *)
      Env.v :=
        {
          !Env.v with
          user_dot_semgrep_dir =
            to_fpath_exn (!!(!Env.v.user_dot_semgrep_dir) ^ "p");
        };
      (* TEST: run the server again after shut down *)
      let%lwt info, _ = do_search ~pattern:"x = 0" info in
      Alcotest.(check int) "number of matches" 2 (List.length matches);
      let%lwt _ = send_request info CR.Shutdown receive_response in

      (* ASSERT: cache exists after deletion *)
      assert_cache_exists ();

      Lwt.return ())

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let sync f () = Lwt_platform.run (f ())

(* Create an lwt test and a synchronous test right away because we run
   both and it's hard to convert from one to the other. *)
let pair ?tolerate_chdir name func =
  ( Testo.create ?tolerate_chdir name (sync func),
    Testo_lwt.create ?tolerate_chdir name func )

let promise_tests caps =
  [
    pair "LS specs" (test_ls_specs caps) ~tolerate_chdir:true;
    (* Keep this test commented out while it is xfail.
        Because logging in is side-effecting, if the test never completes, we
        will stay log in, which can mangle some of the later tests.
       Test_lwt.create "Test LS login" (test_login caps)
       ~expected_outcome:
         (Should_fail "TODO: currently failing in js tests in CI"); *)
    pair "LS /semgrep/search includes/excludes"
      (test_search_includes_excludes caps)
      ~tolerate_chdir:true;
    pair "LS exts" (test_ls_ext caps) ~tolerate_chdir:true;
    pair "LS with no folders" (test_ls_no_folders caps);
    pair "LS multi-workspaces" (test_ls_multi caps) ~tolerate_chdir:true;
    pair "Test LS cache deletion" (test_ls_delete_cache caps);
  ]
  |> List_.split

let tests caps =
  let sync_promise_tests, _ = promise_tests caps in
  Testo.categorize "Language Server (e2e)" sync_promise_tests

(*
   Asynchronous tests for JS tests. We can't turn them into synchronous
   tests because the JavaScript environment doesn't support 'Lwt_main.run'
   or an equivalent call that starts/ends the event loop. This is in fact
   the only reason why Alcotest_lwt and Testo_lwt must exist.
*)
let lwt_tests caps =
  let _, async_promise_tests = promise_tests caps in
  Testo_lwt.categorize "Language Server (e2e)" async_promise_tests
