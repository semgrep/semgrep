open Yojson.Safe.Util
open Jsonrpc
open Lsp
open Types
module In = Input_to_core_t
module SR = Server_request
module SN = Server_notification
module CR = Client_request
module CN = Client_notification
module Out = Semgrep_output_v1_t
module Conv = Convert_utils

let logger = Logging.get_logger [ __MODULE__ ]

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

module State = struct
  type t = Uninitialized | Running | Stopped
end

(*****************************************************************************)
(* Server *)
(*****************************************************************************)

(* Probably could split this into two modules the server + handlers but oh well maybe later *)
module Server = struct
  type t = { session : Session.t; state : State.t }

  (* Why the atomic writes below? The LSP library we use does something weird, *)
  (* it writes the jsonrpc header then body with seperate calls to write, which *)
  (* means there's a race condition there. The below atomic calls ensures that *)
  (* the ENTIRE packet is written at the same time *)
  let respond server id json =
    match json with
    | Some json ->
        logger#info "Server response:\n%s" (Yojson.Safe.pretty_to_string json);
        let response = Response.ok id json in
        let packet = Packet.Response response in
        Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing
    | None -> Lwt.return ()

  let request server request =
    let id = Uuidm.v `V4 |> Uuidm.to_string in
    (* Yea mutable variables are lame but whatever *)
    let request = SR.to_jsonrpc_request request (`String id) in
    let packet = Packet.Request request in
    let _ =
      Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing
    in
    id

  let notify server notification =
    let notification = SN.to_jsonrpc notification in
    logger#info "Server notification:\n%s"
      (notification |> Notification.yojson_of_t |> Yojson.Safe.pretty_to_string);
    let packet = Packet.Notification notification in
    Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing

  let batch_notify server notifications =
    Lwt_list.iter_s (notify server) notifications

  (** Show a little progress circle while doing thing. Returns a token needed to end progress*)
  let create_progresss server title message =
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
    let _ = notify server progress in
    let _ = Lwt_io.flush server.session.outgoing in
    token

  let end_progress server token =
    let end_ = SN.Progress.End (WorkDoneProgressEnd.create ()) in
    let progress = SN.WorkDoneProgress (ProgressParams.create token end_) in
    notify server progress

  (* Relevant here means any matches we actually care about showing the user.
     This means like some matches, such as those that appear in committed
     files/lines, will be filtered out*)

  (** This is the entry point for scanning, returns /relevant/ matches, and all files scanned*)
  let run_semgrep ?(single_file = None) server =
    let config = server.session.config in
    let single_file =
      match single_file with
      | Some file -> Some (Uri.to_path file)
      | None -> None
    in
    let targets = Session.targets server.session in
    let targets =
      match single_file with
      | Some file ->
          let new_targets =
            Common2.filter
              (fun (t : In.target) -> t.path = file)
              targets.target_mappings
          in
          { targets with target_mappings = new_targets }
      | None -> targets
    in
    let target_source = Some (Runner_config.Targets targets) in
    let config =
      { config with target_source; rule_source = server.session.cached_rules }
    in
    let _, res, files =
      Run_semgrep.semgrep_with_raw_results_and_exn_handler config
    in
    let only_git_dirty = server.session.only_git_dirty in
    let final_results, files =
      Processed_run.of_matches ~only_git_dirty res.matches
        (Session.hrules server.session)
        files
    in
    let files = Common2.uniq files in
    (final_results, files)

  let initialize_session server workspace_folders only_git_dirty =
    {
      session = { server.session with workspace_folders; only_git_dirty };
      state = State.Running;
    }

  let on_notification notification server =
    let scan_workspace server =
      let token =
        create_progresss server "Semgrep Scan in Progress" "Scanning Workspace"
      in
      let results, files = run_semgrep server in
      Session.record_results server.session results files;
      (* LSP expects empty diagnostics to clear problems *)
      let files = Session.scanned_files server.session in
      let diagnostics = Diagnostics.diagnostics_of_results results files in
      let _ = end_progress server token in
      batch_notify server diagnostics
    in
    let scan_file server uri =
      let results, files = run_semgrep server ~single_file:(Some uri) in
      Session.record_results server.session results files;
      batch_notify server
        (Diagnostics.diagnostics_of_results results [ Uri.to_path uri ])
    in
    let refresh_rules server =
      let token =
        create_progresss server "Semgrep Rules Refresh" "Loading Rules"
      in
      let session = Session.load_rules server.session in
      let _ = end_progress server token in
      let server = { server with session } in
      let _ = scan_workspace server in
      server
    in
    let server =
      match notification with
      | _ when server.state = State.Uninitialized -> server
      | CN.Initialized ->
          let token =
            create_progresss server "Semgrep Initializing" "Loading Rules"
          in
          logger#info "Client initialized";
          let session = Session.load_rules server.session in
          logger#info "Rules loaded";
          let _ = end_progress server token in
          let server = { server with session } in
          logger#info "Scanning workspace";
          let _ = scan_workspace server in
          server
      | CN.DidSaveTextDocument { textDocument = { uri }; _ }
      | CN.TextDocumentDidOpen { textDocument = { uri; _ } } ->
          logger#info "Scanning %s" (Uri.to_string uri);
          let _ = scan_file server uri in
          server
      | CN.ChangeWorkspaceFolders { event = { added; removed }; _ } ->
          let added = Conv.workspace_folders_to_paths added in
          let removed = Conv.workspace_folders_to_paths removed in
          let session =
            Session.update_workspace_folders server.session ~added ~removed
          in
          let server = { server with session } in
          let _ = scan_workspace server in
          server
      | CN.DidDeleteFiles { files; _ } ->
          (* This is lame, for whatever reason they chose to type uri as string here, not Uri.t *)
          let files =
            Common.map
              (fun { FileDelete.uri } ->
                Str.string_after uri (String.length "file://"))
              files
          in
          let diagnostics = Diagnostics.diagnostics_of_results [] files in
          let _ = batch_notify server diagnostics in
          server
      | CN.UnknownNotification { method_ = "semgrep/loginFinish"; _ }
      | CN.UnknownNotification { method_ = "semgrep/logout"; _ }
      | CN.UnknownNotification { method_ = "semgrep/refreshRules"; _ } ->
          logger#info "Refreshing rules";
          refresh_rules server
      | CN.UnknownNotification
          { method_ = "semgrep/scanWorkspace"; params = Some json } ->
          let json = Structured.yojson_of_t json in
          let full =
            json |> member "full" |> to_bool_option
            |> Option.value ~default:false
          in
          logger#info "Scanning workspace full=%b" full;
          let _server =
            {
              server with
              session = { server.session with only_git_dirty = not full };
            }
          in
          let _ = scan_workspace _server in
          logger#info "Scanning workspace complete";
          server
      | CN.Exit ->
          logger#info "Client exited";
          { server with state = State.Stopped }
      | _ ->
          logger#warning "Unhandled notification";
          server
    in
    server

  let handle_custom_request server (meth : string)
      (params : Jsonrpc.Structured.t option) : Yojson.Safe.t option * t =
    let search_handler =
      Search.on_request server.session.config
        (Session.targets { server.session with only_git_dirty = false })
    in
    match [ (Search.meth, search_handler) ] |> List.assoc_opt meth with
    | None ->
        logger#warning "Unhandled custom request %s" meth;
        (None, server)
    | Some handler -> (handler params, server)

  let on_request (type r) (request : r CR.t) server =
    let to_yojson r = Some (CR.yojson_of_result request r) in
    let resp, server =
      match request with
      | CR.Initialize { rootUri; workspaceFolders; initializationOptions; _ } ->
          (* There's rootPath, rootUri, and workspaceFolders. First two are
             deprecated, so let's split the diffrence and support last two *)
          let initializationOptions =
            match initializationOptions with
            | Some json -> json
            | None -> `Assoc []
          in
          let scan_options = initializationOptions |> member "scan" in
          let only_git_dirty =
            scan_options |> member "onlyGitDirty" |> to_bool_option
            |> Option.value ~default:true
          in
          let workspace_folders =
            match (workspaceFolders, rootUri) with
            | Some (Some folders), _ -> Conv.workspace_folders_to_paths folders
            | _, Some uri -> [ Uri.to_path uri |> Fpath.v ]
            | _ ->
                logger#warning "No rootUri or workspaceFolders provided";
                []
          in
          let init =
            InitializeResult.
              {
                capabilities = server.session.capabilities;
                serverInfo =
                  Some { name = "Semgrep Language Server"; version = None };
              }
          in
          let server =
            initialize_session server workspace_folders only_git_dirty
          in
          (* TODO we should create a progress symbol before calling initialize server! *)
          (to_yojson init, server)
      | _ when server.state = State.Uninitialized ->
          logger#info "Received request before initialization";
          (None, server)
      | CR.CodeAction { textDocument = { uri }; context; _ } ->
          let file = Uri.to_path uri in
          let results = Hashtbl.find_opt server.session.documents file in
          let matches = Common.map fst (Option.value results ~default:[]) in
          let diagnostics = context.diagnostics in
          let ranges =
            Common.map (fun (d : Diagnostic.t) -> d.range) diagnostics
          in
          let matches =
            Common2.filter
              (fun (m : Semgrep_output_v1_t.core_match) ->
                List.mem (Conv.range_of_location m.location) ranges)
              matches
          in
          let actions =
            CodeActions.code_actions_of_core_matches matches [ file ]
          in
          (to_yojson (Some actions), server)
      | TextDocumentHover { position; textDocument; _ } -> (
          let file = Uri.to_path textDocument.uri in
          let contents = Common.cat file in
          let lines =
            contents |> Common.index_list
            |> List.filter (fun (_, idx) -> idx < position.line)
          in
          (* Add 1 to each list for the newline! *)
          let base_charpos =
            lines
            |> Common.map (fun (l, _) -> String.length l + 1)
            |> List.fold_left ( + ) 0
          in
          let charpos = base_charpos + position.character in
          let lang = Lang.lang_of_filename_exn (Fpath.v file) in
          (* copied from -dump_ast *)
          let { Parsing_result2.ast; _ } =
            Parse_target.parse_and_resolve_name lang file
            (* else Parse_target.just_parse_with_lang lang file
            *)
          in
          let res = AST_generic_helpers.nearest_any_of_pos ast charpos in
          match res with
          | None -> (Some `Null, server)
          | Some (any, (t1, t2)) ->
              let v = Meta_AST.vof_any any in
              (* 80 columns is too little *)
              Format.set_margin 120;
              let s = OCaml.string_of_v v in
              let end_line, end_col, _ = Tok.end_pos_of_loc t2 in
              let hover =
                Hover.
                  {
                    contents =
                      `MarkedString { language = Some "OCaml"; value = s };
                    range =
                      Some
                        {
                          (* Subtract one for each line, because we want to switch to
                             0-indexing
                          *)
                          start =
                            {
                              character = t1.pos.column;
                              line = t1.pos.line - 1;
                            };
                          end_ = { character = end_col; line = end_line - 1 };
                        };
                  }
              in
              (Some (Hover.yojson_of_t hover), server))
      | CR.UnknownRequest { meth; params } ->
          handle_custom_request server meth params
      | CR.Shutdown ->
          logger#info "Shutting down";
          (None, { server with state = State.Stopped })
      | CR.DebugEcho params -> (to_yojson params, server)
      | _ ->
          logger#warning "Unhandled request: %s" (Common2.dump request);
          (None, server)
    in
    (resp, server)

  let handle_client_message (msg : Packet.t) server =
    logger#info "Server received:\n%s"
      (msg |> Packet.yojson_of_t |> Yojson.Safe.pretty_to_string);
    let server =
      match msg with
      | Notification n when CN.of_jsonrpc n |> Result.is_ok ->
          on_notification (CN.of_jsonrpc n |> Result.get_ok) server
      | Request req when CR.of_jsonrpc req |> Result.is_ok ->
          let (CR.E r) = CR.of_jsonrpc req |> Result.get_ok in
          let response, server = on_request r server in
          let _ = respond server req.id response in
          server
      | _ ->
          logger#warning "Invalid message";
          server
    in
    Lwt.return server

  let rec rpc_loop server () =
    match server.state with
    | State.Stopped ->
        logger#info "Server stopped";
        Lwt.return ()
    | _ -> (
        let%lwt client_msg = Io.read server.session.incoming in
        match client_msg with
        | Some msg ->
            let%lwt server = handle_client_message msg server in
            rpc_loop server ()
        | None ->
            logger#warning "Client disconnected";
            Lwt.return ())

  let start server = Lwt_main.run (rpc_loop server ())

  (* See: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification *)

  (** Everything this server supports from the LSP *)
  let capabilities =
    let fil =
      FileOperationFilter.create
        ~pattern:(FileOperationPattern.create ~glob:"**/*" ())
        ()
    in
    let reg_opts = FileOperationRegistrationOptions.create ~filters:[ fil ] in
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true ~save:(`Bool true) ()))
      ~workspace:
        (ServerCapabilities.create_workspace
           ~workspaceFolders:
             (WorkspaceFoldersServerCapabilities.create ~supported:true
                ~changeNotifications:(`Bool true) ())
           ~fileOperations:
             (FileOperationOptions.create ~didCreate:reg_opts
                ~didRename:reg_opts ~didDelete:reg_opts ())
           ())
      ~codeActionProvider:(`Bool true) ()

  let create config =
    let config =
      {
        config with
        (* Make sure output format is Json false, dots/etc break LSP *)
        Runner_config.output_format = Json false;
      }
    in
    {
      session = Session.create capabilities config;
      state = State.Uninitialized;
    }
end

let start config =
  logger#info "Starting server";
  let server = Server.create config in
  Server.start server;
  exit 0
