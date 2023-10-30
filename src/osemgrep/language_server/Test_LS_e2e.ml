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
module Out = Semgrep_output_v1_t
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

let create_info () =
  RPC_server.io_ref := (module Io);
  let in_stream, in_push_func = Lwt_stream.create () in
  let out_stream, out_push_func = Lwt_stream.create () in
  let server = LanguageServer.create () in
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
   To ensure they can find the path properly, we just go backwards until we
   find the directoroy named "semgrep".
*)
let rec backtrack path =
  match Fpath.basename path with
  | "semgrep" ->
      Fpath.(path / "cli" / "tests" / "e2e" / "targets" / "ls" / "rules.yaml")
  | _ -> backtrack (Fpath.parent path)

(* Lots of `parent`s here because we're located in _build/default/src/tests *)
let rule_path = backtrack (Fpath.v (Sys.getcwd ()))

let default_content =
  {|
x = 0
if x == 5: # auto rule
    print("hello")
if x == 4: # CI rule
    print("hello")
|}

let login_url_regex =
  Regexp_engine.pcre_compile "https://semgrep.dev/login\\?cli-token=.*"

let prog_regex = Regexp_engine.pcre_compile "Pr([\\s\\S]*)"

(* Not setting this means that really nasty errors happen when an exception
   is raised inside of an Lwt.async, when running the Alcotests.
   As in, the tests will just exit with no error message at all.
*)
let () =
  Lwt.async_exception_hook :=
    fun exn ->
      let err = Printexc.to_string exn in
      Alcotest.fail err

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

(* When running the JSCaml code, if we do an actual pause, the tests will
   straight up exit, without giving any error messages or anything.
   Further investigation seemed to show that pausing caused an issue with
   how the promises resolve, such that the sleeping thread would stay
   sleeping and the entire process would eventually exit.
   So, when running in JSCaml, let's just not use pauses. *)
let lwt_pause () = if !Common.jsoo then Lwt.return_unit else Lwt.pause ()

(*****************************************************************************)
(* Core primitives *)
(*****************************************************************************)

let send (info : info) packet : unit Lwt.t =
  match info.server.state with
  | RPC_server.State.Stopped -> Alcotest.failf "Cannot send, server stopped"
  | _ ->
      let%lwt () = lwt_pause () in
      let () = (snd info.in_stream) (Some packet) in
      Lwt.return_unit

let receive (info : info) : Packet.t Lwt.t =
  match info.server.state with
  | RPC_server.State.Stopped -> Alcotest.failf "Cannot receive, server stopped"
  | _ -> (
      let%lwt () = lwt_pause () in
      let%lwt server_msg = Lwt_stream.get (fst info.out_stream) in
      match server_msg with
      | Some packet -> Lwt.return packet
      | _ -> Alcotest.failf "Received no response, client disconnected")

(*****************************************************************************)
(* Specific send/receive functions *)
(*****************************************************************************)

let send_request info request : unit Lwt.t =
  let id = Uuidm.v `V4 |> Uuidm.to_string in
  let packet = Packet.Request (CR.to_jsonrpc_request request (`String id)) in
  send info packet

let send_custom_request ~meth ?params info =
  send_request info (CR.UnknownRequest { meth; params })

let send_notification info (notification : Client_notification.t) : unit Lwt.t =
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
  Testutil_files.with_tempdir ~persist:true (fun dir ->
      let dir = Fpath.to_string dir in
      (* This chdir does nothing in the JSCaml code, because the OCaml path is
         only simulated, in the Javascript environment.
         Hence, the liberal uses of `-C` everywhere.
         This might still be wrong, though.
      *)
      let () = Sys.chdir dir in
      checked_command (String.concat " " [ "git"; "-C"; dir; "init" ]);
      checked_command
        (String.concat " "
           [
             "git";
             "-C";
             dir;
             "config";
             "user.email";
             "baselinetest@semgrep.com";
           ]);
      checked_command
        (String.concat " "
           [ "git"; "-C"; dir; "config"; "user.name"; "Baseline Test" ]);
      checked_command
        (String.concat " " [ "git"; "-C"; dir; "checkout"; "-B"; "main" ]);
      dir)

let assert_contains (json : Json.t) str =
  let json_str = YS.to_string json in
  if not (Common.contains json_str str) then
    Alcotest.failf "Expected string `%s` in response %s" str json_str

let mock_files () : _ * Fpath.t list =
  let git_tmp_path = Fpath.v (git_tmp_path ()) in

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
    Common.map
      (fun file -> Fpath.(v workspace2_root / filename file))
      workspace1_files
    |> List.sort (fun x y ->
           String.compare (Fpath.to_string x) (Fpath.to_string y))
  in

  (workspace1, (Fpath.v workspace2_root, workspace2_files))

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
        |> Common.map (fun f ->
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
            `Assoc [ ("enabled", `Bool true); ("isNewAppInstall", `Bool true) ]
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
  let text = File.read_file (Fpath.v path) in
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
    added |> Common.map Fpath.to_string
    |> Common.map (fun file ->
           WorkspaceFolder.create ~name:file ~uri:(Uri.of_path file))
  in
  let removed =
    removed |> Common.map Fpath.to_string
    |> Common.map (fun file ->
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

let send_semgrep_search info ?language pattern =
  let params =
    match language with
    | None -> `Assoc [ ("pattern", `String pattern) ]
    | Some lang ->
        `Assoc [ ("pattern", `String pattern); ("language", `String lang) ]
  in
  send_custom_request info ~meth:"semgrep/search" ~params

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
    |> Common.map (fun d -> member "code" d)
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
  let%lwt () = send_initialize info folders in

  let%lwt resp = receive_response info in
  assert_contains (Response.yojson_of_t resp) "capabilities";

  let scanned_files =
    List.filter
      (fun f -> not (Common.contains (Fpath.to_string f) "existing"))
      files
  in

  let%lwt () =
    Lwt_list.iter_s (fun file -> send_did_open info file) scanned_files
  in
  let%lwt () =
    Lwt_list.iter_s
      (fun _ ->
        let%lwt notif = receive_notification info in
        ignore notif;
        Lwt.return_unit)
      scanned_files
  in
  let%lwt () = send_initialized info in
  let%lwt () = assert_progress info "Refreshing Rules" in

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
    |> Common.mapi (fun i file ->
           let notification = List.nth scan_notifications i in
           check_diagnostics notification file expected_ids)
    |> Lwt_list.map_s Fun.id
  in

  Lwt.return_unit

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let with_session (f : info -> unit Lwt.t) : unit Lwt.t =
  (Lwt.async_exception_hook :=
     fun exn ->
       let err = Printexc.to_string exn in
       Logs.err (fun m -> m "Got exception: %s" err);
       Alcotest.fail err);
  let info = create_info () in
  let server_promise = LanguageServer.start info.server in
  let f_promise = f info in
  Lwt.join [ f_promise; server_promise ]

let test_ls_specs () =
  with_session (fun info ->
      let root, files = mock_files () in
      let%lwt () = Lwt.return_unit in
      let%lwt () = check_startup info [ root ] files in
      let%lwt () =
        files
        |> Lwt_list.iter_s (fun file ->
               (* didOpen *)
               let%lwt () = send_did_save info file in

               (* add content *)
               let%lwt params =
                 receive_notification_params
                   PublishDiagnosticsParams.t_of_yojson info
               in
               let old_ids =
                 Common.map (fun d -> d.Diagnostic.code) params.diagnostics
               in

               (* This sleep is to ensure that the subsequent write to
                  `modified.py` results in changing the modification time of
                  the file. Otherwise, we might actually load from the cached
                  AST for the file, and not incorporate the new change.
               *)
               Unix.sleep 1;

               open_and_write_default_content ~mode:[ Open_append ] file;

               (* didSave *)
               let%lwt () = send_did_save info file in

               let%lwt params =
                 receive_notification_params
                   PublishDiagnosticsParams.t_of_yojson info
               in
               let new_ids =
                 Common.map (fun d -> d.Diagnostic.code) params.diagnostics
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
               let%lwt () =
                 send_code_action info ~path:file ~diagnostics:[ diagnostic ]
                   ~line_start ~char_start ~line_end ~char_end
               in
               let%lwt res =
                 receive_response_result CodeActionResult.t_of_yojson info
               in
               assert (List.length (Option.get res) = 1);
               (match res with
               | Some (`CodeAction { kind; _ } :: _) ->
                   assert (
                     CodeActionKind.yojson_of_t (Option.get kind)
                     = `String "quickfix")
               | _ -> Alcotest.fail "expected code action kind");

               Lwt.return_unit)
      in

      (* test did add *)
      let added = Fpath.(root / "added.py") in
      (* nosem *)
      FileUtil.cp [ List.hd files |> Fpath.to_string ] (added |> Fpath.to_string);

      let%lwt () = send_did_add info added in
      let%lwt () = send_did_open info added in

      let%lwt notif = receive_notification info in
      let%lwt () =
        check_diagnostics notif added
          [ `String "eqeq-five"; `String "eqeq-five" ]
      in

      let%lwt () = send_did_delete info added in

      let%lwt notif = receive_notification info in
      let%lwt () = check_diagnostics notif added [] in

      send_exit info)

let test_ls_ext () =
  with_session (fun info ->
      let root, files = mock_files () in
      Testutil_files.with_chdir root (fun () ->
          let%lwt () = check_startup info [ root ] files in

          (* scan workspace *)
          let%lwt () = send_semgrep_scan_workspace info in
          let%lwt () = assert_progress info "Scanning Workspace" in

          let scanned_files =
            List.filter
              (fun f -> not (Common.contains (Fpath.to_string f) "existing"))
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
          let%lwt () = send_semgrep_scan_workspace ~full:true info in

          let%lwt notif = receive_notification info in
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
                     if Common.contains (YS.to_string uri) "modified" then
                       assert (
                         List.length
                           (notif.params |> Option.get |> Structured.yojson_of_t
                          |> member "diagnostics" |> to_list)
                         > List.nth num_ids i);
                     Lwt.return_unit)
          in

          (* Check did open does not rescan if diagnostics exist *)
          let%lwt () =
            files
            |> Lwt_list.iteri_s (fun i _ ->
                   let file = List.nth files i in
                   let%lwt () =
                     (* ??? is this an accurate translation *)
                     if String.length (Fpath.to_string file) > 0 then
                       send_did_open info file
                     else Lwt.return_unit
                   in
                   Lwt.return_unit)
          in

          let%lwt () = send_semgrep_search info "print(...)" in
          let%lwt resp = receive_response info in
          assert (
            YS.Util.(
              resp.result |> Result.get_ok |> member "locations" |> to_list
              |> List.length = 3));

          (* hover is on by default *)
          let%lwt () =
            files
            |> Lwt_list.iter_s (fun file ->
                   let%lwt () = send_hover info file ~character:1 ~line:0 in
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
                   let%lwt () = send_semgrep_show_ast info file in
                   let%lwt resp = receive_response info in
                   let resp =
                     resp.result |> Result.get_ok |> YS.Util.to_string
                   in
                   assert (Regexp_engine.unanchored_match prog_regex resp);
                   Lwt.return_unit)
          in

          send_exit info))

let test_ls_multi () =
  with_session (fun info ->
      let ( (workspace1_root, workspace1_files),
            (workspace2_root, workspace2_files) ) =
        mock_workspaces ()
      in
      let workspace_folders = [ workspace1_root; workspace2_root ] in
      let files = workspace1_files @ workspace2_files in
      let scanned_files =
        List.filter
          (fun f -> not (Common.contains (Fpath.to_string f) "existing"))
          files
      in

      let%lwt () = check_startup info workspace_folders files in

      let%lwt () = send_did_change_folder info ~removed:[ workspace1_root ] in

      let%lwt () = assert_progress info "Scanning Workspace" in

      let%lwt () =
        scanned_files
        |> Lwt_list.iter_s (fun file ->
               let%lwt notif = receive_notification info in
               (* If it's a workspace we removed, then we expect no diagnostics *)
               if
                 Common.contains (Fpath.to_string file)
                   (Fpath.to_string workspace1_root)
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

      let%lwt () = send_did_change_folder info ~added:[ workspace1_root ] in

      let%lwt () = assert_progress info "Scanning Workspace" in

      let%lwt () =
        scanned_files
        |> Lwt_list.iter_s (fun file ->
               let%lwt notif = receive_notification info in
               check_diagnostics notif file [ `String "eqeq-five" ])
      in
      send_exit info)

let test_login () =
  with_session (fun info ->
      let root, files = mock_files () in
      Testutil_files.with_chdir root (fun () ->
          let%lwt () = check_startup info [ root ] files in
          let%lwt () = send_initialize info [] in
          let%lwt resp = receive_response info in
          assert_contains (Response.yojson_of_t resp) "capabilities";

          let%lwt () = send_custom_request ~meth:"semgrep/login" info in
          let%lwt msg = receive_response info in

          let url =
            YS.Util.(msg.result |> Result.get_ok |> member "url" |> to_string)
          in

          assert (Regexp_engine.unanchored_match login_url_regex url);
          send_exit info))

let test_ls_no_folders () =
  with_session (fun info ->
      let%lwt () = check_startup info [] [] in

      send_exit info)

let test_ls_libev () =
  Lwt_platform.set_engine ();
  Lwt.return_unit

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let promise_tests =
  [
    ("Test LS", test_ls_specs);
    ("Test LS exts", test_ls_ext);
    ("Test LS multi-workspaces", test_ls_multi);
    ("Test Login", test_login);
    ("Test LS with no folders", test_ls_no_folders);
  ]

let tests =
  let prepare f () = Lwt_platform.run (f ()) in
  Testutil.pack_tests "Language Server (e2e)"
    (promise_tests |> Common.map (fun (s, f) -> (s, prepare f)))

let lwt_tests =
  Testutil.pack_tests_lwt "Language Server (e2e)"
    [
      ("Test LS", test_ls_specs);
      ("Test LS exts", test_ls_ext);
      ("Test LS multi-workspaces", test_ls_multi);
      ("Test Login", test_login);
      ("Test LS with no folders", test_ls_no_folders);
      ("Test LS with libev", test_ls_libev);
    ]
