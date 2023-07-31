open Grpc_lwt
open Lwt.Syntax [@@warning "-33"]
module RP = Runner.Protos
open Runner.Protos
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let load_rules () =
  let file = Fpath.v "/Users/yannes/.semgrep/secret_rules.json" in
  let res = Rule_fetching.load_rules_from_file ~registry_caching:true file
  in
  Logs.info (fun m -> m "Loaded %d rules from CI" (List.length res.rules));
  Logs.info (fun m -> m "Got %d errors from CI" (List.length res.errors));
  res.rules

let decode_target (target: Runner.Target.t) =
  let name = Fpath.basename (Fpath.v target.name) in
  let ext = Fpath.get_ext (Fpath.v target.name) in
  let tmp_file = Common.new_temp_file name ext in
  Common.write_file tmp_file target.content;
  Fpath.v tmp_file

let run_scan rules targets =

  let conf = Core_runner.
    {
      num_jobs = 1;
      optimizations = false;
      max_memory_mb = 1000000;
      timeout = float_of_int 30;
      timeout_threshold = 3;
      ast_caching = true;
    } in
  let run =
    Core_runner.invoke_semgrep_core ~respect_git_ignore:true
      ~file_match_results_hook:None conf rules [] targets
  in
  let (res : Core_runner.result) = Core_runner.create_core_result rules run in
  let log_level = Some Logs.Warning in
  let cli_output : Out.cli_output = Cli_json_output.cli_output_of_core_results ~logging_level:log_level res
  in
  cli_output

let make_runner_result (input: Runner.Target.t) (output: Out.cli_output): Runner.SearchStreamResponse.t =
  let results = Common.map (fun (r : Out.cli_match) -> Runner.RunnerResult.{
      check_id = r.check_id;
      path = r.path;
      start = Option.some Runner.ResultPosition.{
        line = r.start.line;
        col = r.start.col;
        offset = r.start.offset;
      };
      end' = Option.some Runner.ResultPosition.{
        line = r.end_.line;
        col = r.end_.col;
        offset = r.end_.offset;
      };
      fixed_lines = Option.value ~default:[] r.extra.fixed_lines;
    }
  ) output.results in
  Runner.SearchStreamResponse.make ~results:results ~output:input.content ()

let search_stream (stream : string Lwt_stream.t) (f : string -> unit) =
  (* TODO: Move to cache *)
  let rules = load_rules () in
  let open Ocaml_protoc_plugin in
  let decode, encode = Service.make_service_functions Runner.RunnerService.searchStream in

  let* () =
    Lwt_stream.iter_s
      (fun i ->
        let req =
          Reader.create i |> decode |> function
          | Ok v -> v
          | Error e ->
              failwith
                (Printf.sprintf "Could not decode request: %s"
                   (Result.show_error e))
        in

        let* () = Lwt_io.(flush stdout) in
        let target = match req.target with
          | None ->
            failwith
                (Printf.sprintf "Target is empty")
          | Some t -> t
        in
        let target_file = decode_target target in
        let (output : Out.cli_output) = run_scan rules [target_file] in
        let reply = make_runner_result target output in
        Logs.app (fun m -> m "Results: %s" (Runner.SearchStreamResponse.show reply));
        Lwt.return (encode reply |> Writer.contents |> f))
      stream
  in
  let* () = Lwt_io.printf "SearchStream exiting\n" in
  let* () = Lwt_io.(flush stdout) in
  Lwt.return Grpc.Status.(v OK)

let search_service =
   Server.Service.(
    v () |> add_rpc ~name:"SearchStream" ~rpc:(Bidirectional_streaming search_stream) |> handle_request)

let server =
  Server.(
    v () |> add_service ~name:"protos.runner.RunnerService" ~service:search_service)

let run_server () =
  let open Lwt.Syntax in
  let port = 8080 in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
      let server =
        H2_lwt_unix.Server.create_connection_handler ?config:None
          ~request_handler:(fun _ reqd -> Server.handle_request server reqd)
          ~error_handler:(fun _ ?request:_ _ _ ->
            print_endline "an error occurred")
      in
      let+ _server =
        Lwt_io.establish_server_with_client_socket listen_address server
      in
      Printf.printf "Listening on port %i for grpc requests\n" port;
      print_endline "";
      print_endline "Try running:";
      print_endline "";
      print_endline
        {| osemgrep runner --client |});

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
