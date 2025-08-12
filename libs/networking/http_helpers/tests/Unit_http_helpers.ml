let t = Testo.create
let uri = Uri.of_string "https://example.com/test"
let expected_body = "It works!"

let response_fn delay _resp _body =
  let%lwt () = Lwt_unix.sleep delay in
  let resp_body = Cohttp_lwt.Body.of_string expected_body in
  Lwt.return Http_mock_client.(basic_response ~status:200 resp_body)

let test_http_timeout _caps =
  let successful_req =
    Http_mock_client.with_mocked_http (response_fn 0.1) (fun () ->
        let%lwt result = Http_helpers.call_client ~timeout_secs:0.2 `GET uri in
        match result with
        | Ok (_, body) ->
            Alcotest.(check string) "body matches" body expected_body;
            Lwt.return_unit
        | Error e -> Alcotest.fail ("Expected success but got error: " ^ e))
  in

  let timedout_req =
    Http_mock_client.with_mocked_http (response_fn 0.2) (fun () ->
        let%lwt result = Http_helpers.call_client ~timeout_secs:0.1 `GET uri in
        match result with
        | Ok _ -> Alcotest.fail "Expected timeout but got success!"
        | Error _ -> Lwt.return_unit)
  in

  Lwt_platform.run (successful_req ());
  Lwt_platform.run (timedout_req ())

let test_ok_bias _caps =
  (* All things being equal, if both promises resolve we should choose the Ok
   * over the Error. *)
  let successful_req =
    Http_mock_client.with_mocked_http (response_fn 0.0) (fun () ->
        let%lwt result = Http_helpers.call_client ~timeout_secs:0.0 `GET uri in
        match result with
        | Ok (_, body) ->
            Alcotest.(check string) "body matches" body expected_body;
            Lwt.return_unit
        | Error e -> Alcotest.fail ("Expected success but got error: " ^ e))
  in
  Lwt_platform.run
    (let _ = Lwt_unix.sleep 0.1 in
     successful_req ())

let tests caps =
  Testo.categorize "Http_helpers"
    [
      t "test_http_timeout" (fun () -> test_http_timeout caps);
      t "test_ok_bias" (fun () -> test_ok_bias caps);
    ]
