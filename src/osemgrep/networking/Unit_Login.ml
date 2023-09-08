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
open Testutil

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ok_token = "ok_token"
let bad_token = "bad_token"

let login_session =
  ( Uuidm.of_string "00000000-0000-0000-0000-000000000000" |> Option.get,
    snd (Semgrep_login.make_login_url ()) )

let fake_settings =
  "has_shown_metrics_notification: true\n\
   anonymous_user_id: 5a06eb03-071d-4c89-b297-993c4912fa8f"

let with_login_client =
  let make_fn req body =
    let uri = Cohttp.Request.uri req in
    match Uri.path uri with
    | "/api/agent/deployments/current" ->
        let status, body =
          match Testing_client.get_header req "Authorization" with
          | Some "Bearer ok_token" -> (200, "./tests/login/ok_response.json")
          | Some "Bearer bad_token" -> (401, "./tests/login/bad_response.json")
          | _ -> failwith "Unexpected token"
        in
        Lwt.return Testing_client.(basic_response ~status body)
    | "/api/agent/tokens/requests" ->
        let%lwt () =
          Testing_client.check_body body "./tests/login/fetch_body.json"
        in
        Lwt.return
          (Testing_client.basic_response ~status:200
             "./tests/login/token_response.json")
    | _ -> failwith ("Unexpected path: " ^ Uri.path uri)
  in
  Testing_client.with_testing_client make_fn

let with_four_o_four_client =
  let make_fn req body =
    ignore body;
    ignore req;
    Lwt.return Testing_client.(basic_response ~status:404 "/dev/null")
  in
  Testing_client.with_testing_client make_fn

let with_tmp_settings_file f () =
  Common2.with_tmp_file ~str:fake_settings ~ext:"yml" (fun tmp_settings_file ->
      let old_settings = !Semgrep_envvars.v in
      let new_settings =
        { old_settings with user_settings_file = Fpath.v tmp_settings_file }
      in
      Semgrep_envvars.v := new_settings;
      f ();
      Semgrep_envvars.v := old_settings)

let with_tmp_settings_and_login_client f =
  with_login_client (with_tmp_settings_file f)

let with_logged_in f =
  let token = ok_token in
  match Semgrep_login.save_token token with
  | Ok () -> f ()
  | Error e -> failwith e

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let save_token_tests () =
  ignore with_logged_in;
  let valid_token_test () =
    match Semgrep_login.save_token ok_token with
    | Ok () ->
        Alcotest.(check bool) "logged in" true (Semgrep_login.is_logged_in ())
    | Error e -> failwith e
  in
  let invalid_token_test () =
    match Semgrep_login.save_token bad_token with
    | Ok () -> failwith "Expected error"
    | Error _ ->
        Alcotest.(check bool)
          "not logged in" false
          (Semgrep_login.is_logged_in ())
  in
  let tests =
    pack_tests "save_token"
      [
        ("invalid token", invalid_token_test); ("valid token", valid_token_test);
      ]
  in
  Common.map (fun (n, f) -> (n, with_tmp_settings_and_login_client f)) tests

let fetch_token_tests () =
  let fetch_basic () =
    let token = Semgrep_login.fetch_token login_session in
    match token with
    | Ok (token, username) ->
        Alcotest.(check string) "token" ok_token token;
        Alcotest.(check string) "username" "testuser" username
    | Error e -> failwith e
  in
  let fetch_no_internet () =
    let retry_count = ref 0 in
    (* please ignore the nesting *)
    let wait_hook () =
      match !retry_count with
      | 12 -> failwith "Unexpected wait"
      | _ -> retry_count := !retry_count + 1
    in
    let token =
      Semgrep_login.fetch_token ~min_wait:0 ~next_wait:0 ~wait_hook
        login_session
    in
    match token with
    | Error e ->
        let re = Str.regexp ".*internet connection.*" in
        Logs.debug (fun m -> m "Error: %s" e);
        Alcotest.(check bool) "no internet" true (Str.search_forward re e 0 > 0);
        Alcotest.(check int) "retry count" 12 !retry_count
    | _ -> failwith "Expected timeout"
  in
  pack_tests "fetch_token"
    [
      ("basic", with_tmp_settings_and_login_client fetch_basic);
      ( "no internet",
        with_tmp_settings_file (with_four_o_four_client fetch_no_internet) );
    ]

let tests =
  pack_suites "Osemgrep Login" [ save_token_tests (); fetch_token_tests () ]
