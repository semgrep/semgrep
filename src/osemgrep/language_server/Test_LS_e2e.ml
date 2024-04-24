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
open Types
open Jsonrpc
open Fpath_.Operators
module OutJ = Semgrep_output_v1_t
module In = Input_to_core_t
module SR = Server_request
module CR = Client_request
module CN = Client_notification
module YS = Yojson.Safe
module LanguageServer = LS.LanguageServer

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

type push_function = Jsonrpc.Packet.t option -> unit

let write_func : push_function ref = ref (fun _ -> ())

let read_stream : Jsonrpc.Packet.t Lwt_stream.t ref =
  ref (fst (Lwt_stream.create ()))

module Io : RPC_server.LSIO = struct
  let write packet =
    !write_func (Some packet);
    Lwt.return_unit

  let read () = Lwt_stream.get !read_stream
  let flush () = Lwt.return_unit
end

type info = {
  server : RPC_server.t;
  in_stream : Jsonrpc.Packet.t Lwt_stream.t * push_function;
  out_stream : Jsonrpc.Packet.t Lwt_stream.t * push_function;
}

let create_info caps =
  RPC_server.io_ref := (module Io);
  let in_stream, in_push_func = Lwt_stream.create () in
  let out_stream, out_push_func = Lwt_stream.create () in
  let server = LanguageServer.create caps in
  write_func := out_push_func;
  read_stream := in_stream;
  {
    server;
    in_stream = (in_stream, in_push_func);
    out_stream = (out_stream, out_push_func);
  }

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
let get_rule_path () =
  match Git_wrapper.get_project_root () with
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
let timeout = 30.0

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

let with_timeout (f : unit -> 'a Lwt.t) : unit -> 'a Lwt.t =
  let timeout_promise =
    let%lwt () = Lwt_platform.sleep timeout in
    Alcotest.failf "Test timed out after %f seconds!" timeout
  in
  let f () = Lwt.pick [ f (); timeout_promise ] in
  f

(*****************************************************************************)
(* Core primitives *)
(*****************************************************************************)

let send (info : info) packet : unit =
  match info.server.state with
  | RPC_server.State.Stopped -> Alcotest.failf "Cannot send, server stopped"
  | _ ->
      let push_func = snd info.in_stream in
      push_func (Some packet)

let receive (info : info) : Packet.t Lwt.t =
  match info.server.state with
  | RPC_server.State.Stopped -> Alcotest.failf "Cannot receive, server stopped"
  | _ -> (
      let%lwt server_msg = Lwt_stream.get (fst info.out_stream) in
      match server_msg with
      | Some packet -> Lwt.return packet
      | _ -> Alcotest.failf "Received no response, client disconnected")

(*****************************************************************************)
(* Specific send/receive functions *)
(*****************************************************************************)

let send_request info request : unit =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let packet = Packet.Request (CR.to_jsonrpc_request request (`String id)) in
  send info packet

let send_custom_request ~meth ?params info =
  send_request info (CR.UnknownRequest { meth; params })

let send_notification info (notification : Client_notification.t) : unit =
  let packet =
    Packet.Notification (Client_notification.to_jsonrpc notification)
  in
  send info packet

let send_custom_notification ~meth ?params info =
  send_notification info (CN.UnknownNotification { method_ = meth; params })

let receive_response (info : info) : Response.t Lwt.t =
  let%lwt packet = receive info in
  match packet with
  | Packet.Response resp -> Lwt.return resp
  | _ ->
      Alcotest.failf "expected valid response, got %s"
        (Packet.yojson_of_t packet |> YS.to_string)

(* This function simply preprocesses the result inside of the response with
   a specific function before returning, instead of leaving it to be
   unpacked later.
*)
let receive_response_result (f : Json.t -> 'a) (info : info) : 'a Lwt.t =
  let%lwt resp = receive_response info in
  Lwt.return (resp.result |> Result.get_ok |> f)

let receive_notification (info : info) : Notification.t Lwt.t =
  let%lwt packet = receive info in
  match packet with
  | Packet.Notification notif -> Lwt.return notif
  | _ ->
      Alcotest.failf "expected notification, got %s"
        (Packet.yojson_of_t packet |> YS.to_string)

(* This function simply preprocesses the parameters of the notification
   with a specific function before returning, instead of leaving it to be
   unpacked later.
*)
let receive_notification_params (f : YS.t -> 'a) (info : info) : 'a Lwt.t =
  let%lwt notif = receive_notification info in
  Lwt.return (f (notif.params |> Option.get |> Structured.yojson_of_t))

let receive_request (info : info) : Request.t Lwt.t =
  let%lwt packet = receive info in
  match packet with
  | Packet.Request req -> Lwt.return req
  | _ ->
      Alcotest.failf "expected request, got %s"
        (Packet.yojson_of_t packet |> YS.to_string)

(*****************************************************************************)
(* Mocking and testing functions *)
(*****************************************************************************)

let git_tmp_path () =
  let _orig_dir = Sys.getcwd () in
  Testutil_files.with_tempdir ~persist:true (*~chdir:true*) (fun dir ->
      (* TODO: investigate/report
         Mysterious bug: the test will hang as we chdir back into the original
         directory. *)
      if not !Common.jsoo then Sys.chdir !!dir;
      checked_command (String.concat " " [ "git"; "-C"; !!dir; "init" ]);
      checked_command
        (String.concat " "
           [
             "git";
             "-C";
             !!dir;
             "config";
             "user.email";
             "baselinetest@semgrep.com";
           ]);
      checked_command
        (String.concat " "
           [ "git"; "-C"; !!dir; "config"; "user.name"; "Baseline Test" ]);
      checked_command
        (String.concat " " [ "git"; "-C"; !!dir; "checkout"; "-B"; "main" ]);
      (* !!!!!!!!!!!!!!! This call causes hanging !!!!!!!!!!!!!!! *)
      (* Sys.chdir _orig_dir; *)
      dir)

let assert_contains (json : Json.t) str =
  let json_str = YS.to_string json in
  if not (String_.contains ~term:str json_str) then
    Alcotest.failf "Expected string `%s` in response %s" str json_str

let mock_files () : _ * Fpath.t list =
  let git_tmp_path = git_tmp_path () in

  let open Fpath in
  let root = git_tmp_path in
  (* should have preexisting matches that are committed *)
  let modified_file = git_tmp_path / "modified.py" in
  (* should have preexisting matches that are not committed *)
  let existing_file = git_tmp_path / "existing.py" in

  open_and_write_default_content ~mode:[ Open_wronly ] modified_file;
  open_and_write_default_content ~mode:[ Open_wronly ] existing_file;

  checked_command
    (* nosem *)
    (String.concat " "
       [
         "git";
         "-C";
         Fpath.to_string git_tmp_path;
         "remote";
         "add";
         "origin";
         (* nosem *)
         "/tmp/origin";
       ]);
  checked_command
    (String.concat " "
       [
         "git";
         "-C";
         Fpath.to_string git_tmp_path;
         "add";
         Fpath.to_string modified_file;
       ]);
  checked_command
    (String.concat " "
       [
         "git";
         "-C";
         Fpath.to_string git_tmp_path;
         "add";
         Fpath.to_string existing_file;
       ]);
  checked_command
    (String.concat " "
       [
         "git";
         "-C";
         Fpath.to_string git_tmp_path;
         "commit";
         "-m";
         "\"initial commit\"";
       ]);

  open_and_write_default_content ~mode:[ Open_append ] modified_file;

  let new_file = root / "new.py" in
  (* created after commit *)
  open_and_write_default_content ~mode:[ Open_wronly ] new_file;

  let files =
    [ existing_file; modified_file; new_file ]
    |> List.sort (fun x y ->
           String.compare (Fpath.to_string x) (Fpath.to_string y))
  in

  (git_tmp_path, files)

let mock_workspaces () =
  let ((workspace1_root, workspace1_files) as workspace1) = mock_files () in
  let workspace1_root = Fpath.to_string workspace1_root in

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

  (workspace1, (Fpath.v workspace2_root, workspace2_files))

let mock_search_files () : _ * Fpath.t list =
  let git_tmp_path = git_tmp_path () in

  let root = git_tmp_path in
  let path1 = root / "a" / "b" / "c.py" in
  let path2 = root / "test.py" in
  (* should have preexisting matches that are committed *)
  Unix.mkdir (root / "a" |> Fpath.to_string) 0o777;
  Unix.mkdir (root / "a" / "b" |> Fpath.to_string) 0o777;
  open_and_write_default_content ~mode:[ Open_wronly ] path1;
  open_and_write_default_content ~mode:[ Open_wronly ] path2;

  (root, [ path1; path2 ])

(*****************************************************************************)
(* Sending functions *)
(*****************************************************************************)

let send_exit info = send_notification info Client_notification.Exit

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
              ~version:"1.68.1" ())
         ~locale:"en-us" ~rootPath:(Some "") (* THINK? *)
         ?rootUri (* THINK? *)
         ~workspaceFolders ~initializationOptions
         ~capabilities:(ClientCapabilities.create ())
         ())
  in
  send_request info request

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
  send_notification info notif

let send_did_add info (path : Fpath.t) =
  let path = Fpath.to_string path in
  let files = [ FileCreate.create ~uri:("file://" ^ path) ] in
  let notif = CN.DidCreateFiles (CreateFilesParams.create ~files) in
  send_notification info notif

let send_did_delete info (path : Fpath.t) =
  let path = Fpath.to_string path in
  let files = [ FileDelete.create ~uri:("file://" ^ path) ] in
  let notif = CN.DidDeleteFiles (DeleteFilesParams.create ~files) in
  send_notification info notif

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

let check_diagnostics (notif : Notification.t) (file : Fpath.t) expected_ids =
  let open YS.Util in
  let resp = Notification.yojson_of_t notif in
  Alcotest.(check string)
    "method is publishDiagnostics"
    (resp |> member "method" |> to_string)
    "textDocument/publishDiagnostics";
  Alcotest.(check string)
    "uri is same as file"
    (resp |> member "params" |> member "uri" |> to_string)
    (Common.spf "file://%s" (Fpath.to_string file));
  let ids =
    resp |> member "params" |> member "diagnostics" |> to_list
    |> List_.map (fun d -> member "code" d)
  in
  Alcotest.(check string)
    "diagnostics are cohesive"
    (YS.to_string (`List ids))
    (YS.to_string (`List expected_ids));
  Lwt.return_unit

let assert_notif (notif : Notification.t) ?message ?kind meth =
  Alcotest.(check string) "methods should be same" meth notif.method_;
  (match message with
  | None -> ()
  | Some message ->
      assert (
        YS.Util.(
          notif.params |> Option.get |> Structured.yojson_of_t |> member "value"
          |> member "message" |> to_string = message)));
  (match kind with
  | None -> ()
  | Some kind ->
      assert (
        YS.Util.(
          notif.params |> Option.get |> Structured.yojson_of_t |> member "value"
          |> member "kind" |> to_string = kind)));
  Lwt.return_unit

let assert_request (req : Request.t) ?message ?kind meth =
  let open YS.Util in
  assert (req.method_ = meth);
  (match message with
  | None -> ()
  | Some message ->
      assert (
        req.params |> Option.get |> Structured.yojson_of_t |> member "value"
        |> member "message" |> to_string = message));
  (match kind with
  | None -> ()
  | Some kind ->
      assert (
        req.params |> Option.get |> Structured.yojson_of_t |> member "value"
        |> member "kind" |> to_string = kind));
  Lwt.return_unit

let assert_message (notif : Notification.t) message =
  Alcotest.(check string)
    "methods should be same" notif.method_ "window/showMessage";
  assert (
    YS.Util.(
      notif.params |> Option.get |> Structured.yojson_of_t |> member "message"
      |> to_string = message));
  Lwt.return_unit

let assert_progress info message =
  let%lwt req = receive_request info in
  let%lwt () = assert_request req "window/workDoneProgress/create" in

  let%lwt notif = receive_notification info in
  let%lwt () = assert_notif notif "$/progress" ~message in

  let%lwt notif = receive_notification info in
  let%lwt () = assert_notif notif "$/progress" ~kind:"end" in
  Lwt.return_unit

let check_startup info folders (files : Fpath.t list) =
  (* initialize *)
  send_initialize info folders;

  let%lwt resp = receive_response info in
  assert_contains (Response.yojson_of_t resp) "capabilities";

  let scanned_files =
    List.filter
      (fun f -> not (String_.contains (Fpath.to_string f) ~term:"existing"))
      files
  in

  List.iter (send_did_open info) scanned_files;
  let%lwt () =
    Lwt_list.iter_s
      (fun _ ->
        let%lwt notif = receive_notification info in
        ignore notif;
        Lwt.return_unit)
      scanned_files
  in
  send_initialized info;
  let%lwt () = assert_progress info "Refreshing Rules" in
  let%lwt rulesRefreshedNotif = receive_notification info in
  let%lwt () = assert_notif rulesRefreshedNotif "semgrep/rulesRefreshed" in
  let%lwt () = assert_progress info "Scanning Open Documents" in

  let%lwt scan_notifications =
    Lwt_list.map_s (fun _ -> receive_notification info) scanned_files
  in

  let scan_notifications =
    List.sort
      (fun (x : Notification.t) (y : Notification.t) ->
        let open YS.Util in
        String.compare
          (x.params |> Option.get |> Structured.yojson_of_t |> member "uri"
         |> to_string)
          (y.params |> Option.get |> Structured.yojson_of_t |> member "uri"
         |> to_string))
      scan_notifications
  in

  let expected_ids = [ `String "eqeq-five" ] in

  let%lwt _ =
    scanned_files
    |> List_.mapi (fun i file ->
           let notification = List.nth scan_notifications i in
           check_diagnostics notification file expected_ids)
    |> Lwt_list.map_s Fun.id
  in

  Lwt.return_unit

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let do_search ?(pattern = "print(...)") ?(includes = []) ?(excludes = []) info =
  send_semgrep_search info pattern ~includes ~excludes;
  Lwt_seq.unfold_lwt
    (fun () ->
      let%lwt resp = receive_response info in
      match
        YS.Util.(resp.result |> Result.get_ok |> member "locations" |> to_list)
      with
      | [] -> Lwt.return None
      | matches ->
          (* Send the searchOngoing so we get the next response *)
          send_semgrep_search_ongoing info;
          Lwt.return (Some (matches, ())))
    ()
  |> Lwt_seq.to_list |> Lwt.map List.concat

let with_session caps (f : info -> unit Lwt.t) : unit Lwt.t =
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
  let info = create_info caps in
  let server_promise () = LanguageServer.start info.server in
  (* Separate promise so we actually bubble up errors from this one *)
  Lwt.async server_promise;
  f info

let test_ls_specs caps () =
  with_session caps (fun info ->
      let root, files = mock_files () in
      let%lwt () = Lwt.return_unit in
      let%lwt () = check_startup info [ root ] files in
      let%lwt () =
        files
        |> Lwt_list.iter_s (fun file ->
               (* didOpen *)
               send_did_save info file;

               (* add content *)
               let%lwt params =
                 receive_notification_params
                   PublishDiagnosticsParams.t_of_yojson info
               in
               let old_ids =
                 List_.map (fun d -> d.Diagnostic.code) params.diagnostics
               in

               (* This sleep is to ensure that the subsequent write to
                  `modified.py` results in changing the modification time of
                  the file. Otherwise, we might actually load from the cached
                  AST for the file, and not incorporate the new change.
               *)
               Unix.sleep 1;

               open_and_write_default_content ~mode:[ Open_append ] file;

               (* didSave *)
               send_did_save info file;

               let%lwt params =
                 receive_notification_params
                   PublishDiagnosticsParams.t_of_yojson info
               in
               let new_ids =
                 List_.map (fun d -> d.Diagnostic.code) params.diagnostics
               in

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
               send_code_action info ~path:file ~diagnostics:[ diagnostic ]
                 ~line_start ~char_start ~line_end ~char_end;
               let%lwt res =
                 receive_response_result CodeActionResult.t_of_yojson info
               in
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
               send_execute_command ~command:command.command
                 ~arguments:(Option.get command.arguments)
                 info;
               let%lwt params =
                 receive_notification_params
                   PublishDiagnosticsParams.t_of_yojson info
               in
               let not_ignored_ids =
                 List_.map (fun d -> d.Diagnostic.code) params.diagnostics
               in
               Alcotest.(check int)
                 "new finding from modified file with ignored finding"
                 (List.length not_ignored_ids)
                 (List.length new_ids - 1);

               Lwt.return_unit)
      in

      (* test did add *)
      let added = Fpath.(root / "added.py") in
      (* nosem *)
      FileUtil.cp [ List.hd files |> Fpath.to_string ] (added |> Fpath.to_string);

      (* Tests target caching *)
      send_did_add info added;
      send_did_open info added;

      let%lwt notif = receive_notification info in
      let%lwt () =
        check_diagnostics notif added
          [ `String "eqeq-five"; `String "eqeq-five" ]
      in

      send_did_delete info added;

      let%lwt notif = receive_notification info in
      let%lwt () = check_diagnostics notif added [] in

      send_exit info;
      Lwt.return_unit)

let test_ls_ext caps () =
  with_session caps (fun info ->
      let root, files = mock_files () in
      Testutil_files.with_chdir root (fun () ->
          let%lwt () = check_startup info [ root ] files in

          (* scan workspace *)
          send_semgrep_scan_workspace info;
          let%lwt () = assert_progress info "Scanning Workspace" in

          let scanned_files =
            List.filter
              (fun f ->
                not (String_.contains (Fpath.to_string f) ~term:"existing"))
              files
          in
          let%lwt num_ids =
            Lwt_list.map_s
              (fun _ ->
                let%lwt notif = receive_notification info in
                Lwt.return
                  YS.Util.(
                    List.length
                      (notif.params |> Option.get |> Structured.yojson_of_t
                     |> member "diagnostics" |> to_list)))
              scanned_files
          in

          (* scan workspace full *)
          send_semgrep_scan_workspace ~full:true info;

          Logs.app (fun m -> m "Waiting for scan to finish 2");
          let%lwt notif = receive_notification info in
          Logs.app (fun m -> m "Received notification");
          let%lwt () =
            assert_message notif
              "Scanning all files regardless of git status. These diagnostics \
               will persist until a file is edited. To default to always \
               scanning regardless of git status, please disable 'Only Git \
               Dirty' in settings"
          in

          let%lwt () = assert_progress info "Scanning Workspace" in

          let%lwt () =
            files
            |> Lwt_list.iteri_s
                 YS.Util.(
                   fun i _ ->
                     let%lwt notif = receive_notification info in
                     let uri =
                       notif.params |> Option.get |> Structured.yojson_of_t
                       |> member "uri"
                     in
                     if String_.contains (YS.to_string uri) ~term:"modified"
                     then
                       assert (
                         List.length
                           (notif.params |> Option.get |> Structured.yojson_of_t
                          |> member "diagnostics" |> to_list)
                         > List.nth num_ids i);
                     Lwt.return_unit)
          in

          (* search *)
          let%lwt matches = do_search info in
          assert (matches |> List.length = 3);

          (* hover is on by default *)
          let%lwt () =
            files
            |> Lwt_list.iter_s (fun file ->
                   send_hover info file ~character:1 ~line:0;
                   let%lwt resp = receive_response info in
                   assert (resp.result |> Result.get_ok <> `Null);
                   (* just checking that "contents" exists *)
                   YS.Util.(
                     resp.result |> Result.get_ok |> member "contents" |> ignore);
                   Lwt.return_unit)
          in

          (* showAst *)
          let%lwt () =
            files
            |> Lwt_list.iter_s (fun file ->
                   send_semgrep_show_ast info file;
                   let%lwt resp = receive_response info in
                   let resp =
                     resp.result |> Result.get_ok |> YS.Util.to_string
                   in
                   assert (Pcre2_.unanchored_match prog_regex resp);
                   Lwt.return_unit)
          in

          send_exit info;
          Lwt.return_unit))

let test_ls_multi caps () =
  with_session caps (fun info ->
      let ( (workspace1_root, workspace1_files),
            (workspace2_root, workspace2_files) ) =
        mock_workspaces ()
      in
      let workspace_folders = [ workspace1_root; workspace2_root ] in
      let files = workspace1_files @ workspace2_files in
      let scanned_files =
        List.filter
          (fun f -> not (String_.contains (Fpath.to_string f) ~term:"existing"))
          files
      in

      let%lwt () = check_startup info workspace_folders files in

      send_did_change_folder info ~removed:[ workspace1_root ];

      let%lwt () = assert_progress info "Scanning Workspace" in

      let%lwt () =
        scanned_files
        |> Lwt_list.iter_s (fun file ->
               let%lwt notif = receive_notification info in
               (* If it's a workspace we removed, then we expect no diagnostics *)
               if
                 String_.contains (Fpath.to_string file)
                   ~term:(Fpath.to_string workspace1_root)
               then (
                 Alcotest.(check int)
                   "check number of diagnostics is good"
                   YS.Util.(
                     List.length
                       (notif.params |> Option.get |> Structured.yojson_of_t
                      |> member "diagnostics" |> to_list))
                   0;
                 Lwt.return_unit)
               else check_diagnostics notif file [ `String "eqeq-five" ])
      in

      send_did_change_folder info ~added:[ workspace1_root ];

      let%lwt () = assert_progress info "Scanning Workspace" in

      let%lwt () =
        scanned_files
        |> Lwt_list.iter_s (fun file ->
               let%lwt notif = receive_notification info in
               check_diagnostics notif file [ `String "eqeq-five" ])
      in
      send_exit info;
      Lwt.return_unit)

let _test_login caps () =
  with_session caps (fun info ->
      (* If we don't log out prior to starting this test, the LS will complain
         we're already logged in, and not display the correct behavior.
      *)
      let settings = Semgrep_settings.load () in
      if not (Semgrep_settings.save { settings with api_token = None }) then
        Alcotest.fail "failed to save settings to log out in ls e2e test";
      let root, files = mock_files () in
      Testutil_files.with_chdir root (fun () ->
          let%lwt () = check_startup info [ root ] files in
          send_initialize info [];
          let%lwt resp = receive_response info in
          assert_contains (Response.yojson_of_t resp) "capabilities";

          send_custom_request ~meth:"semgrep/login" info;
          let%lwt msg = receive_response info in

          let url =
            YS.Util.(msg.result |> Result.get_ok |> member "url" |> to_string)
          in

          assert (Pcre2_.unanchored_match login_url_regex url);
          Semgrep_settings.save settings |> ignore;
          send_exit info;
          Lwt.return_unit))

let test_ls_no_folders caps () =
  with_session caps (fun info ->
      let%lwt () = check_startup info [] [] in

      send_exit info;
      Lwt.return_unit)

let test_ls_libev () =
  Lwt_platform.set_engine ();
  Lwt.return_unit

let test_search_includes_excludes caps () =
  with_session caps (fun info ->
      let root, files = mock_search_files () in

      let%lwt () = check_startup info [ root ] files in
      let%lwt matches = do_search ~pattern:"x = 0" info in
      assert (List.length matches = 2);

      let assert_with_includes_excludes ?(includes = []) ?(excludes = [])
          ~matches_test ~matches_c () =
        let%lwt matches = do_search ~includes ~excludes ~pattern:"x = 0" info in
        let num_matches =
          (if matches_test then 1 else 0) + if matches_c then 1 else 0
        in
        assert (List.length matches = num_matches);
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

      Lwt.return_unit)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let sync f () = Lwt_platform.run (f ())

(* Create an lwt test and a synchronous test right away because we run
   both and it's hard to convert from one to the other. *)
let pair ?tolerate_chdir name func =
  let func = with_timeout func in
  ( Testo.create ?tolerate_chdir name (sync func),
    Testo_lwt.create ?tolerate_chdir name func )

let promise_tests caps =
  [
    pair "Test LS" (test_ls_specs caps) ~tolerate_chdir:true;
    pair "Test LS exts" (test_ls_ext caps) ~tolerate_chdir:true;
    pair "Test LS multi-workspaces" (test_ls_multi caps) ~tolerate_chdir:true;
    (* Keep this test commented out while it is xfail.
        Because logging in is side-effecting, if the test never completes, we
        will stay log in, which can mangle some of the later tests.
       Test_lwt.create "Test LS login" (test_login caps)
       ~expected_outcome:
         (Should_fail "TODO: currently failing in js tests in CI"); *)
    pair "Test LS with no folders" (test_ls_no_folders caps);
    pair "Test LS /semgrep/search includes/excludes"
      (test_search_includes_excludes caps)
      ~tolerate_chdir:true;
  ]
  |> List.split

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
  Testo.categorize "Language Server (e2e)"
    (Testo_lwt.create "Test LS with libev" test_ls_libev :: async_promise_tests)
