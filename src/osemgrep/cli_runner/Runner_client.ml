open Grpc_lwt
open Lwt.Syntax
open Runner.Protos

let connect address port : H2_lwt_unix.Client.t Lwt.t =
  let* addresses =
    Lwt_unix.getaddrinfo address (string_of_int port)
      [ Unix.(AI_FAMILY PF_INET) ]
  in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket (List.hd addresses).Unix.ai_addr in
  let error_handler _ = print_endline "error" in
  H2_lwt_unix.Client.create_connection ~error_handler socket

let call_server connection input_stream =
  let open Ocaml_protoc_plugin in
  let encode, decode = Service.make_client_functions Runner.RunnerService.searchStream in
  let rec go f stream targets =
    match targets with
    | [] ->
        f None;
        Lwt.return ()
    | req :: xs ->
      let () = encode req |> Writer.contents |> fun x -> f (Some x) in
      let* () = Lwt_unix.sleep 1.0 in

      let* response = Lwt_stream.next stream in
      let scan_result =
        Reader.create response |> decode |> function
        | Ok response -> response
        | Error e ->
          failwith
            (Printf.sprintf "Could not decode request: %s"
                (Result.show_error e))
      in
      let* () = Lwt_io.printf "Result = {%s}\n" (Runner.SearchStreamResponse.show scan_result) in
      go f stream xs
  in
  let* result =
    Client.call ~service:"protos.runner.RunnerService" ~rpc:"SearchStream"
      ~do_request:(H2_lwt_unix.Client.request connection ~error_handler:ignore)
      ~handler:
        (Client.Rpc.bidirectional_streaming ~f:(fun f stream ->
             go f stream input_stream))
      ()
  in
  match result with
  | Ok ((), _ok) -> Lwt.return ()
  | Error e -> failwith (Printf.sprintf "GRPC error: %s" (Grpc.Status.show e))

let make_token i () =
  let token = Printf.sprintf "some-token-%d" i in
  let prefix = if i mod 2 = 0 then "access_token" else "not_a_secret" in
  Printf.sprintf "{\"%s\": \"%s\"}" prefix token

let make_input_stream () =
  let options = Runner.RunOptions.make ~autofix:false ~dryrun:false ~matching_explanations:false ~no_rewrite_rule_ids:false ~strict:false ~use_pro_engine:false () in

  let count = 5 in
  Printf.printf "Generating %i targets\n" count;
  let targets =
    count
    |> Seq.unfold (function
         | 0 -> None
         | x ->
            Some
              ( Runner.Target.make ~name:"test-target.json" ~content:(make_token x ())
            (),
          x - 1 ))
    |> List.of_seq
  in
  let stream = Common2.map (fun t -> Runner.SearchStreamRequest.make ~target:t ~options:options ()) targets in
  stream

let run_client () =
  let port = 8080 in
  let address = "localhost" in
  let () = Random.self_init () in
  let input_stream = make_input_stream ()
  in
  Lwt_main.run
    (
      let* connection = connect address port in
      let* () = call_server connection input_stream in
     Lwt.return ())
