open Lsp
open Types

let logger = Logging.get_logger [ __MODULE__ ]

module State = struct
  type t = Uninitialized | Running | Stopped
end

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

module Session = struct
  type t = {
    capabilities : ServerCapabilities.t;
    incoming : Lwt_io.input_channel;
    outgoing : Lwt_io.output_channel;
    config : Runner_config.t; (* ... *)
    root : string;
    cached_rules : Runner_config.rule_source option;
    documents :
      (Uri.t, (Semgrep_output_v1_t.core_match * Rule.rule) list) Hashtbl.t;
  }

  (* This is dynamic so if the targets file is updated we don't have to restart (and reparse rules...) *)
  let targets session =
    let config = session.config in
    match config.target_source with
    | Some (Targets targets) -> targets
    | Some (Target_file _) ->
        let targets, _ = Run_semgrep.targets_of_config config [] in
        targets
    | None -> failwith "No targets provided"

  let load_rules session =
    let config = session.config in
    let rules =
      match config.rule_source with
      | Some (Rule_file file) ->
          let (rules, _), _ =
            Common.with_time (fun () ->
                Parse_rule.parse_and_filter_invalid_rules file)
          in
          rules
      | Some (Rules rules) -> rules
      | None -> failwith "No rules provided"
    in
    { session with cached_rules = Some (Rules rules) }

  let hrules session =
    let rules =
      match session.cached_rules with
      | Some (Rules rules) -> rules
      | _ -> []
    in
    Rule.hrules_of_rules rules
end

(* This probably should all go in a separate reporting file *)

let interpolate_metavars (metavars : Semgrep_output_v1_t.metavars) text =
  Common2.fold
    (fun text ((l, v) : string * Semgrep_output_v1_t.metavar_value) ->
      let re = Str.regexp_string l in
      Str.global_replace re v.abstract_content text)
    text metavars

let convert_fix (m : Semgrep_output_v1_t.core_match) (rule : Rule.t) =
  let rule_fix (r : Rule.t) =
    match r.fix with
    | Some fix -> Some (interpolate_metavars m.extra.metavars fix)
    (*TODO: regex autofix*)
    | None -> None
  in
  let fix =
    match m.extra.rendered_fix with
    | Some fix -> Some fix
    | None -> rule_fix rule
  in
  fix

let postprocess_results results hrules files =
  let results =
    JSON_report.match_results_of_matches_and_errors (Some Autofix.render_fix)
      (List.length files) results
  in
  let matches =
    Common2.map
      (fun (m : Semgrep_output_v1_t.core_match) ->
        let rule = Hashtbl.find hrules m.rule_id in
        let m =
          {
            m with
            extra =
              {
                m.extra with
                rendered_fix = convert_fix m rule;
                message =
                  Some (interpolate_metavars m.extra.metavars rule.Rule.message);
              };
          }
        in
        (m, rule))
      results.matches
  in
  let matches =
    Common2.filter
      (fun ((_, r) : Semgrep_output_v1_t.core_match * Rule.rule) ->
        r.severity != Rule.Experiment && r.severity != Rule.Inventory)
      matches
  in
  (* Canonicalize paths so we can convert them to URIs LSP supports later *)
  (matches, files)

module Server = struct
  type t = { session : Session.t; state : State.t (* ... *) }

  (* Why the atomic writes below? The LSP library we use does something weird, *)
  (* it writes the jsonrpc header then body with seperate calls to write, which *)
  (* means there's a race condition there. The below atomic calls ensures that *)
  (* the ENTIRE packet is written at the same time *)
  let respond id json server =
    match json with
    | Some json ->
        logger#info "Server response: %s" (Yojson.Safe.to_string json);
        let response = Jsonrpc.Response.ok id json in
        let packet = Jsonrpc.Packet.Response response in
        Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing
    | None -> Lwt.return ()

  let notify ?(channel = None) server notification =
    let notification = Server_notification.to_jsonrpc notification in
    let packet = Jsonrpc.Packet.Notification notification in
    match channel with
    | Some channel -> Io.write channel packet
    | None ->
        Lwt_io.atomic (fun oc -> Io.write oc packet) server.session.outgoing

  let batch_notify server notifications =
    logger#info "Batch notifications: %d" (List.length notifications);
    Lwt_io.atomic
      (fun channel ->
        Lwt_list.iter_s (notify server ~channel:(Some channel)) notifications)
      Lwt_io.stdout

  let run_semgrep ?(files = []) server =
    let config = server.session.config in
    let targets = Session.targets server.session in
    let files = Common.map Uri.to_path files in
    let target_source =
      match files with
      | [] -> config.target_source
      | files ->
          let filtered_targets =
            Common2.filter
              (fun (target : Input_to_core_t.target) ->
                List.mem target.path files)
              targets.target_mappings
          in
          Some (Targets { targets with target_mappings = filtered_targets })
    in
    let config =
      { config with target_source; rule_source = server.session.cached_rules }
    in
    let _, res, files =
      Run_semgrep.semgrep_with_raw_results_and_exn_handler config
    in
    (* We get duplicate files listed because lang jobs I believe *)
    let files = Common2.uniq files in
    logger#info "Server scanned %d files" (List.length files);
    let final_results, files =
      postprocess_results res (Session.hrules server.session) files
    in
    (final_results, files)

  let initialize_session server root =
    let session = Session.load_rules server.session in
    { session = { session with root }; state = State.Running }

  let on_notification notification server =
    let scan_workspace server =
      let results, files = run_semgrep server in
      (* Let's scan the whole workspace to get a nice overview and so users know the extension is running *)
      let diagnostics = Diagnostics.diagnostics_of_results results files in
      let _ = batch_notify server diagnostics in
      ()
    in
    let server =
      match notification with
      | Client_notification.Initialized ->
          logger#info "Client initialized";
          scan_workspace server;
          server
      | Client_notification.DidSaveTextDocument { textDocument = { uri }; _ }
      | Client_notification.TextDocumentDidOpen { textDocument = { uri; _ } } ->
          let results, _ = run_semgrep server ~files:[ uri ] in
          Hashtbl.add server.session.documents uri results;
          logger#info "Added results for %s" (Uri.to_string uri);
          let _ =
            batch_notify server
              (Diagnostics.diagnostics_of_results results [ Uri.to_path uri ])
          in
          server
      | Client_notification.UnknownNotification { method_ = "semgrep/loginFinish"; _ } ->
          logger#info "Refreshing rules";
          let session = Session.load_rules server.session in

          let server = { server with session } in
          scan_workspace server;
          server

      | Client_notification.UnknownNotification { method_ = "semgrep/scanWorkspace"; _ } ->
          logger#info "Scanning workspace";
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
          let rootUri =
            match rootUri with
            | Some uri -> uri
            | None -> (
                match workspaceFolders with
                | Some (Some ({ uri; _ } :: _)) -> uri
                | _ -> failwith "No rootUri or workspaceFolders provided")
          in
          let root = Uri.to_path rootUri in
          let init =
            InitializeResult.
              {
                capabilities = server.session.capabilities;
                serverInfo =
                  Some { name = "Semgrep LSP Server"; version = Some "0.0.1" };
              }
          in
          let server = initialize_session server root in
          (* TODO we should create a progress symbol before calling initialize server! *)
          (to_yojson init, server)
      | Client_request.CodeAction { textDocument = { uri }; _ } ->
          let results = Hashtbl.find server.session.documents uri in
          let matches = Common.map fst results in
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
              let _ = respond req.id response server in
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

  let start server () = Lwt_main.run (rpc_loop server ())

  let capabilities =
    ServerCapabilities.create
      ~textDocumentSync:
        (`TextDocumentSyncOptions
          (TextDocumentSyncOptions.create ~openClose:true ~save:(`Bool true) ()))
      ~codeActionProvider:(`Bool true) ()

  let create config () =
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
          };
      state = State.Uninitialized;
    }
end

let start config () =
  logger#info "Starting server";
  let server = Server.create config () in
  Server.start server ()
