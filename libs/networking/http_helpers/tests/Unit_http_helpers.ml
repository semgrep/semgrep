(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let t = Testo.create
let uri = Uri.of_string "https://example.com/test"
let expected_body = "It works!"

let response_fn delay _resp _body =
  let%lwt () = Lwt_unix.sleep delay in
  let resp_body = Cohttp_lwt.Body.of_string expected_body in
  Lwt.return Http_mock_client.(basic_response ~status:200 resp_body)

let test_http_timeout () =
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

let test_ok_bias () =
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

(* ENGINE-2712: guard against URL leakage from http_helpers's error paths.
   We plant a sentinel in the request URI's query string and assert that it
   never appears in any error string returned to callers or in any body
   argument the mock sees. The sentinel represents, e.g., an API key. *)
let sentinel = "SHOULDNOTAPPEAR"

let secret_uri =
  Uri.of_string
    (Printf.sprintf "https://example.com/search?api_key=%s" sentinel)

let contains_sentinel s =
  try
    ignore (Str.search_forward (Str.regexp_string sentinel) s 0);
    true
  with
  | Not_found -> false

let assert_no_leak_in_error = function
  | Ok _ -> Alcotest.fail "Expected an error from the HTTP mock, got Ok"
  | Error msg ->
      Alcotest.(check bool)
        ("returned error must not contain the URL; got: " ^ msg)
        false (contains_sentinel msg);
      Lwt.return_unit

let test_error_string_omits_url_on_exception () =
  let run =
    Http_mock_client.with_mocked_http
      (fun _req _body -> failwith "simulated network failure")
      (fun () ->
        let%lwt result = Http_helpers.call_client `GET secret_uri in
        assert_no_leak_in_error result)
  in
  Lwt_platform.run (run ())

let test_error_string_omits_url_on_retry () =
  let run =
    Http_mock_client.with_mocked_http
      (fun _req _body -> Lwt.fail Cohttp_lwt.Connection.Retry)
      (fun () ->
        let%lwt result = Http_helpers.call_client `GET secret_uri in
        assert_no_leak_in_error result)
  in
  Lwt_platform.run (run ())

let test_error_string_omits_url_on_timeout () =
  let run =
    Http_mock_client.with_mocked_http
      (fun _req _body ->
        let%lwt () = Lwt_unix.sleep 1.0 in
        Lwt.return Http_mock_client.(basic_response Cohttp_lwt.Body.empty))
      (fun () ->
        let%lwt result =
          Http_helpers.call_client ~timeout_secs:0.05 `GET secret_uri
        in
        assert_no_leak_in_error result)
  in
  Lwt_platform.run (run ())

let tests =
  Testo.categorize "Http_helpers"
    [
      t "test_http_timeout" test_http_timeout;
      t "test_ok_bias" test_ok_bias;
      t "test_error_string_omits_url_on_exception"
        test_error_string_omits_url_on_exception;
      t "test_error_string_omits_url_on_retry"
        test_error_string_omits_url_on_retry;
      t "test_error_string_omits_url_on_timeout"
        test_error_string_omits_url_on_timeout;
    ]
