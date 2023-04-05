open Lsp
open Types

let logger = Logging.get_logger [ __MODULE__ ]

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

module Server = struct
  type t = { session : Session.t; state : State.t }

  (* Why the atomic writes below? The LSP library we use does something weird, *)
  (* it writes the jsonrpc header then body with seperate calls to write, which *)
  (* means there's a race condition there. The below atomic calls ensures that *)
  (* the ENTIRE packet is written at the same time *)
  let respond server id json =
    match json with
    | Some json ->
        logger#info "Server response: %s" (Yojson.Safe.to_string json);
        let response = Jsonrpc.Response.ok id json in
        let packet = Jsonrpc.Packet.Response response in
        Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing
    | None -> Lwt.return ()

  let request server request =
    let id = server.session.next_id in
    server.session.next_id <- id + 1;
    let request = Server_request.to_jsonrpc_request request (`Int id) in
    let packet = Jsonrpc.Packet.Request request in
    let _ =
      Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing
    in
    id

  let notify server notification =
    let notification = Server_notification.to_jsonrpc notification in
    let packet = Jsonrpc.Packet.Notification notification in
    Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing

  let batch_notify server notifications =
    Lwt_list.iter_s (notify server) notifications

  let create_progresss server title message =
    let token = ProgressToken.t_of_yojson (`Int server.session.next_id) in
    let progress =
      Server_request.WorkDoneProgressCreate
        (WorkDoneProgressCreateParams.create token)
    in
    let _ = request server progress in
    let start =
      Server_notification.Progress.Begin
        (WorkDoneProgressBegin.create ~message ~title ())
    in
    let progress =
      Server_notification.WorkDoneProgress (ProgressParams.create token start)
    in
    let _ = notify server progress in
    let _ = Lwt_io.flush server.session.outgoing in
    token

  let end_progress server token =
    let end_ =
      Server_notification.Progress.End (WorkDoneProgressEnd.create ())
    in
    let progress =
      Server_notification.WorkDoneProgress (ProgressParams.create token end_)
    in
    notify server progress

  let run_semgrep ?(single_file = None) server =
    let config = server.session.config in
    let single_file =
      match single_file with
      | Some file -> Some (Uri.to_path file)
      | None -> None
    in
    let%lwt targets = Session.targets server.session in
    let targets =
      match single_file with
      | Some file ->
          let new_targets =
            Common2.filter
              (fun (t : Input_to_core_t.target) -> t.path = file)
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
    let%lwt final_results, files =
      Reporting.postprocess_results res (Session.hrules server.session) files
    in
    let files = Common2.uniq files in
    Lwt.return (final_results, files)

  let initialize_session server root =
    { session = { server.session with root }; state = State.Running }

  let on_notification notification server =
    let scan_workspace server =
      let token =
        create_progresss server "Semgrep Scan in Progress" "Scanning Workspace"
      in
      let%lwt results, files = run_semgrep server in
      let diagnostics = Diagnostics.diagnostics_of_results results files in
      let _ = end_progress server token in
      batch_notify server diagnostics
    in
    let scan_file server uri =
      let%lwt results, _ = run_semgrep server ~single_file:(Some uri) in
      Hashtbl.add server.session.documents uri results;
      batch_notify server
        (Diagnostics.diagnostics_of_results results [ Uri.to_path uri ])
    in
    let server =
      match notification with
      | Client_notification.Initialized ->
          let token =
            create_progresss server "Semgrep Initializing" "Loading Rules"
          in
          logger#info "Client initialized";

          let session = Session.load_rules server.session in
          let _ = end_progress server token in
          let server = { server with session } in
          let _ = scan_workspace server in
          server
      | Client_notification.DidSaveTextDocument { textDocument = { uri }; _ }
      | Client_notification.TextDocumentDidOpen { textDocument = { uri; _ } } ->
          logger#info "Scanning %s" (Uri.to_string uri);
          let _ = scan_file server uri in
          server
      | Client_notification.UnknownNotification
          { method_ = "semgrep/loginFinish"; _ } ->
          logger#info "Refreshing rules";
          let token =
            create_progresss server "Semgrep Rules Refresh" "Loading Rules"
          in
          let session = Session.load_rules server.session in
          let _ = end_progress server token in
          let server = { server with session } in
          (* Let's scan the whole workspace to get a nice overview and so users know the extension is running *)
          let _ = scan_workspace server in
          server
      | Client_notification.UnknownNotification
          { method_ = "semgrep/scanWorkspace"; _ } ->
          logger#info "Scanning workspace";
          let _ = scan_workspace server in
          server
      | _ ->
          logger#warning "Unhandled notification";
          server
    in
    server

  let on_request (type r) (request : r Client_request.t) server =
    let to_yojson r = Some (Client_request.yojson_of_result request r) in
    let resp, server =
      match request with
      | Client_request.Shutdown -> (None, { server with state = State.Stopped })
      | Client_request.Initialize { rootUri; workspaceFolders; _ } ->
          (* There's rootPath, rootUri, and workspaceFolders. First two are
             deprecated, so let's split the diffrence and support last two *)
          let workspace_uri =
            match workspaceFolders with
            | Some (Some ({ uri; _ } :: _)) -> Uri.to_path uri
            | _ ->
                logger#warning "No rootUri or workspaceFolders provided";
                ""
          in
          let root =
            match rootUri with
            | Some uri -> Uri.to_path uri
            | None -> workspace_uri
          in
          let init =
            InitializeResult.
              {
                capabilities = server.session.capabilities;
                serverInfo =
                  Some { name = "Semgrep Language Server"; version = None };
              }
          in
          let server = initialize_session server root in
          (* TODO we should create a progress symbol before calling initialize server! *)
          (to_yojson init, server)
      | Client_request.CodeAction { textDocument = { uri }; context; _ } ->
          let results = Hashtbl.find_opt server.session.documents uri in
          let matches = Common.map fst (Option.value results ~default:[]) in
          let diagnostics = context.diagnostics in
          let ranges =
            Common.map (fun (d : Diagnostic.t) -> d.range) diagnostics
          in
          let matches =
            Common2.filter
              (fun (m : Semgrep_output_v1_t.core_match) ->
                List.mem (Lsp_util.range_of_location m.location) ranges)
              matches
          in
          let actions =
            CodeActions.code_actions_of_results matches [ Uri.to_path uri ]
          in
          let actions =
            match actions with
            | [] -> None
            | actions -> Some actions
          in
          (to_yojson actions, server)
      | Client_request.DebugEcho params -> (to_yojson params, server)
      | _ ->
          logger#warning "Unhandled request";
          (None, server)
    in
    (resp, server)

  let handle_client_message (msg : Jsonrpc.Packet.t) server =
    logger#info "Server received: %s"
      (Jsonrpc.Packet.yojson_of_t msg |> Yojson.Safe.to_string);
    let server =
      match msg with
      | Notification n -> (
          match Client_notification.of_jsonrpc n with
          | Ok n -> on_notification n server
          | Error _ -> server)
      | Request req -> (
          match Client_request.of_jsonrpc req with
          | Ok (Client_request.E r) ->
              let response, server = on_request r server in
              let _ = respond server req.id response in
              server
          | Error e ->
              logger#warning "Invalid request: %s" e;
              server)
      | _ ->
          logger#warning "Invalid message";
          server
    in
    Lwt.return server

  let rec rpc_loop server () =
    let%lwt client_msg = Io.read server.session.incoming in
    match client_msg with
    | None ->
        logger#info "Client disconnected";
        Lwt.return ()
    | Some msg ->
        let%lwt server = handle_client_message msg server in
        rpc_loop server ()

  let start server = Lwt_main.run (rpc_loop server ())

  let capabilities =
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true ~save:(`Bool true) ()))
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
      session =
        Session.
          {
            capabilities;
            incoming = Lwt_io.stdin;
            outgoing = Lwt_io.stdout;
            config;
            root = "";
            cached_rules = None;
            documents = Hashtbl.create 10;
            next_id = 0;
          };
      state = State.Uninitialized;
    }
end

let start config =
  logger#info "Starting server";
  let server = Server.create config in
  Server.start server
