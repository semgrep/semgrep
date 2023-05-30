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
        logger#info "Server response: %s" (Yojson.Safe.to_string json);
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

  let search_semgrep server pattern_string language =
    (* We should use Session.targets here, but that uses the LWT monad, so once the git commands use Bos the below can become simpler *)
    let config = server.session.config in
    let targets =
      match config.target_source with
      | Some (Targets targets) -> targets
      | Some (Target_file _) ->
          let targets, _ = Run_semgrep.targets_of_config config [] in
          targets
      | None -> failwith "No targets provided"
    in
    let target_mappings = targets.target_mappings in
    let target_mappings =
      Common.map (fun t -> { t with In.rule_nums = [ 0 ] }) target_mappings
    in
    let target_mappings =
      List.filter (fun t -> t.In.language = language) target_mappings
    in
    let lang = Lang.of_string language in
    let pattern =
      Parse_pattern.parse_pattern ~print_errors:false lang pattern_string
    in
    let rule, _ =
      Common.with_time (fun () ->
          let fk = Tok.unsafe_fake_tok "" in
          let xlang = Xlang.L (lang, []) in
          let xpat =
            Xpattern.mk_xpat
              (Xpattern.Sem (lazy pattern, lang))
              (pattern_string, fk)
          in
          Rule.rule_of_xpattern xlang xpat)
    in
    let config =
      {
        config with
        target_source =
          Some
            (Runner_config.Targets
               { target_mappings; rule_ids = [ fst rule.id ] });
      }
    in
    let config = { config with rule_source = Some (Rules [ rule ]) } in
    let _, res, _ =
      Run_semgrep.semgrep_with_raw_results_and_exn_handler config
    in
    let matches, _ =
      Common.partition_either
        (JSON_report.match_to_match (Some Autofix.render_fix))
        res.matches
    in
    let files_found =
      Common.map (fun (m : Out.core_match) -> m.location) matches
    in
    let found =
      Common.group_by (fun (m : Out.location) -> m.path) files_found
    in
    found

  let initialize_session server root only_git_dirty =
    {
      session = { server.session with root; only_git_dirty };
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
          let _ = end_progress server token in
          let server = { server with session } in
          let _ = scan_workspace server in
          server
      | CN.DidSaveTextDocument { textDocument = { uri }; _ }
      | CN.TextDocumentDidOpen { textDocument = { uri; _ } } ->
          logger#info "Scanning %s" (Uri.to_string uri);
          let _ = scan_file server uri in
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
          server
      | CN.Exit ->
          logger#info "Client exited";
          { server with state = State.Stopped }
      | _ ->
          logger#warning "Unhandled notification";
          server
    in
    server

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
          let server = initialize_session server root only_git_dirty in
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
      | CR.UnknownRequest { meth = "semgrep/search"; params = Some params } ->
          let params = Structured.yojson_of_t params in
          let pattern = params |> member "pattern" |> to_string in
          let lang = params |> member "language" |> to_string in
          logger#info "Searching for pattern %s w/ language %s" pattern lang;
          let locations = search_semgrep server pattern lang in
          let json =
            Common.map
              (fun (file, matches) ->
                let uri = file |> Uri.of_path |> Uri.to_string in
                let ranges =
                  matches
                  |> Common.map Conv.range_of_location
                  |> Common.map Range.yojson_of_t
                in
                `Assoc [ ("uri", `String uri); ("ranges", `List ranges) ])
              locations
          in
          let json = `Assoc [ ("locations", `List json) ] in
          (Some json, server)
      | CR.Shutdown -> (None, { server with state = State.Stopped })
      | CR.DebugEcho params -> (to_yojson params, server)
      | _ ->
          logger#warning "Unhandled request: %s" (Common2.dump request);
          (None, server)
    in
    (resp, server)

  let handle_client_message (msg : Packet.t) server =
    logger#info "Server received: %s"
      (Packet.yojson_of_t msg |> Yojson.Safe.to_string);
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
    let%lwt client_msg = Io.read server.session.incoming in
    match (client_msg, server.state) with
    | None, _
    | _, State.Stopped ->
        logger#info "Server stopped";
        Lwt.return ()
    | Some msg, _ ->
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
      session = Session.create capabilities config;
      state = State.Uninitialized;
    }
end

let start config =
  logger#info "Starting server";
  let server = Server.create config in
  Server.start server
