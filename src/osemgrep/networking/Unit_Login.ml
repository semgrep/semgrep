(* Austin Theriault
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let t = Testo.create

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ok_token = Auth.unsafe_token_of_string "ok_token"
let bad_token = Auth.unsafe_token_of_string "bad_token"

let secret =
  Uuidm.of_string "00000000-0000-0000-0000-000000000000" |> Option.get

let fake_settings =
  "has_shown_metrics_notification: true\n\
   anonymous_user_id: 11111111-1111-1111-1111-111111111111"

let with_mock_normal_responses =
  let make_fn req body =
    let uri = Cohttp.Request.uri req in
    match Uri.path uri with
    | "/api/agent/deployments/current" ->
        let status, body_path =
          match Http_mock_client.get_header req "Authorization" with
          | Some "Bearer ok_token" ->
              (200, Fpath.v "./tests/login/ok_response.json")
          | Some "Bearer bad_token" ->
              (401, Fpath.v "./tests/login/bad_response.json")
          | _ -> failwith "Unexpected token"
        in
        let body = UFile.read_file body_path |> Cohttp_lwt.Body.of_string in
        Lwt.return Http_mock_client.(basic_response ~status body)
    | "/api/agent/tokens/requests" ->
        let%lwt () =
          Http_mock_client.check_body body
            (Http_mock_client.body_of_file ~trim:true
               (Fpath.v "./tests/login/fetch_body.json"))
        in
        Lwt.return
          (Http_mock_client.basic_response ~status:200
             (Http_mock_client.body_of_file
                (Fpath.v "./tests/login/token_response.json")))
    | _ -> failwith ("Unexpected path: " ^ Uri.path uri)
  in
  Http_mock_client.with_testing_client make_fn

let with_mock_four_o_four_responses =
  let make_fn req body =
    ignore body;
    ignore req;
    Lwt.return
      Http_mock_client.(
        basic_response ~status:404 (Cohttp_lwt.Body.of_string ""))
  in
  Http_mock_client.with_testing_client make_fn

let with_mock_envvars f () =
  UTmp.with_temp_file ~contents:fake_settings ~suffix:".yml"
    (fun tmp_settings_file ->
      let new_settings =
        { !Semgrep_envvars.v with user_settings_file = tmp_settings_file }
      in
      Common.save_excursion Semgrep_envvars.v new_settings f)

let with_mock_envvars_and_normal_responses f =
  with_mock_normal_responses (with_mock_envvars f)

let with_logged_in f =
  let token = ok_token in
  let caps = Cap.network_caps_UNSAFE () in
  let caps = Auth.cap_token_and_network token caps in
  match Semgrep_login.save_token caps with
  | Ok _deployment_config -> f ()
  | Error e -> failwith e

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let save_token_tests caps =
  ignore with_logged_in;
  let valid_token_test () =
    let caps = Auth.cap_token_and_network ok_token caps in
    match Semgrep_login.save_token caps with
    | Ok _deployment_config ->
        Alcotest.(check bool)
          "logged in" true
          (Semgrep_login.is_logged_in_weak ())
    | Error e -> failwith e
  in
  let invalid_token_test () =
    let caps = Auth.cap_token_and_network bad_token caps in
    match Semgrep_login.save_token caps with
    | Ok _deployment_config -> failwith "Expected error"
    | Error _ ->
        Alcotest.(check bool)
          "not logged in" false
          (Semgrep_login.is_logged_in_weak ())
  in
  let tests =
    [ ("invalid token", invalid_token_test); ("valid token", valid_token_test) ]
    |> List_.map (fun (n, f) -> t n (with_mock_envvars_and_normal_responses f))
  in
  Testo.categorize "save_token" tests

let fetch_token_tests caps =
  let fetch_basic () =
    let token = Semgrep_login.fetch_token caps secret in
    match token with
    | Ok (token, username) ->
        let str_token = Auth.string_of_token token in
        let ok_token_str = Auth.string_of_token ok_token in
        Alcotest.(check string) "token" ok_token_str str_token;
        Alcotest.(check string) "username" "testuser" username
    | Error e -> failwith e
  in
  let fetch_no_internet () =
    let retry_count = ref 0 in
    (* please ignore the nesting *)
    let wait_hook _delay =
      match !retry_count with
      | 12 -> failwith "Unexpected wait"
      | _ -> incr retry_count
    in
    let token =
      Semgrep_login.fetch_token ~min_wait_ms:0 ~next_wait_ms:0 ~wait_hook caps
        secret
    in
    match token with
    | Error e ->
        let re = Str.regexp ".*internet connection.*" in
        Logs.debug (fun m -> m "Error: %s" e);
        Alcotest.(check bool) "no internet" true (Str.search_forward re e 0 > 0);
        Alcotest.(check int) "retry count" 12 !retry_count
    | _ -> failwith "Expected timeout"
  in
  Testo.categorize "fetch_token"
    [
      t "basic" (with_mock_envvars_and_normal_responses fetch_basic);
      t "no internet"
        (with_mock_envvars (with_mock_four_o_four_responses fetch_no_internet));
    ]

let tests caps =
  Testo.categorize_suites "Osemgrep Login"
    [ save_token_tests caps; fetch_token_tests caps ]
