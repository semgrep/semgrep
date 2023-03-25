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
  (* Active rules *)
  (* Documents *)
  (* Capabalites *)

  type t = {
    capabilities : ServerCapabilities.t;
    incoming : Lwt_io.input_channel;
    outgoing : Lwt_io.output_channel;
    config : Runner_config.t; (* ... *)
    hrules : Rule.hrules;
  }
end

module Server = struct
  type t = { session : Session.t; state : State.t (* ... *) }

  (* on_request *)
  (* on_notification *)

  let respond id json server =
    match json with
    | Some json ->
        logger#info "Server response: %s" (Yojson.Safe.to_string json);
        let response = Jsonrpc.Response.ok id json in
        let packet = Jsonrpc.Packet.Response response in
        Io.write server.session.outgoing packet
    | None -> Lwt.return ()

  let notify server notification =
    let notification = Server_notification.to_jsonrpc notification in
    let packet = Jsonrpc.Packet.Notification notification in
    Io.write server.session.outgoing packet

  let batch_notify server notifications =
    logger#info "Batch notifications: %d" (List.length notifications);
    Lwt_list.iter_s (notify server) notifications

  let postprocess_results results hrules files =
    let results =
      JSON_report.match_results_of_matches_and_errors (Some Autofix.render_fix)
        (List.length files) results
    in
    let matches =
      Common2.map
        (fun (m : Semgrep_output_v1_t.core_match) ->
          let rule = Hashtbl.find hrules m.rule_id in
          (m, rule))
        results.matches
    in
    Common2.filter
      (fun ((_, r) : Semgrep_output_v1_t.core_match * Rule.rule) ->
        r.severity != Rule.Experiment && r.severity != Rule.Inventory)
      matches

  let run_semgrep server =
    let _, res, files =
      Run_semgrep.semgrep_with_raw_results_and_exn_handler server.session.config
    in
    logger#info "Server scanned %d files" (List.length files);
    let final_results = postprocess_results res server.session.hrules files in
    let _ =
      batch_notify server
        (Diagnostics.diagnostics_of_results final_results files)
    in
    Lwt.return server

  let on_notification notification server =
    let () =
      match notification with
      | Client_notification.Initialized -> logger#info "Server initialized"
      | Client_notification.DidSaveTextDocument _
      | Client_notification.TextDocumentDidOpen _ ->
          let _ = run_semgrep server in
          ()
      | _ -> logger#info "Notification"
    in
    server

  let on_request (type r) (request : r Client_request.t) server =
    let to_yojson r = Some (Client_request.yojson_of_result request r) in
    let resp, server =
      match request with
      | Client_request.Shutdown -> (None, { server with state = State.Stopped })
      | Client_request.Initialize _ ->
          let init =
            InitializeResult.
              {
                capabilities = server.session.capabilities;
                serverInfo =
                  Some { name = "semgrep-lsp"; version = Some "0.0.1" };
              }
          in
          (to_yojson init, { server with state = State.Running })
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
    logger#info "Server listening for next message";
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
          (TextDocumentSyncOptions.create ~openClose:true ~save:(`Bool true)
             ~change:TextDocumentSyncKind.Incremental ()))
      ()

  let create config () =
    let open Runner_config in
    let rules =
      match config.rule_source with
      | Some (Rule_file file) ->
          logger#info "Loading rules from %s" file;
          let (rules, _), _ =
            Common.with_time (fun () ->
                Parse_rule.parse_and_filter_invalid_rules file)
          in
          rules
      | Some (Rules rules) -> rules
      | None -> failwith "No rules provided"
    in
    (* Make sure output format is Json false, dots/etc break LSP *)
    let config =
      {
        config with
        rule_source = Some (Rules rules);
        output_format = Json false;
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
            hrules = Rule.hrules_of_rules rules;
          };
      state = State.Uninitialized;
    }
end

let start config () =
  logger#info "Starting server";
  let server = Server.create config () in
  Server.start server ()
