(* Brandon Wu
 *
 * Copyright (C) 2023 Semgrep, Inc.
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
open Common
open Fpath_.Operators
open Testutil
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of test_publish.py to OCaml.
 *
 * Note that unlike most cli/tests/e2e/test_xxx.py tests, we can't reuse
 * test_publish.py to test osemgrep because of the use of mocking
 * and 'use_click+runner=True' in test_publish.py
 *)

let tests_path () = Fpath.(v (Sys.getcwd ()) / "cli" / "tests")

type result = { exit_code : Exit_code.t; logs : string }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* This token does not have to be valid because we mock the function
 * that checks for its validation (XXX)
 *)
let fake_token = "1234"

let fake_registry_response =
  `Assoc
    [
      ("pr_url", `String "fake_url");
      ("id", `String "fake_id");
      ("path", `String "fake_path");
    ]
  |> Yojson.to_string

(* deployment_response in semgrep_output_v1.atd
 * alt: could build one using Semgrep_output_v1_j
 *)
let fake_deployment =
  {|
  { "deployment":
    { "id": 1234,
      "name": "deployment1234"
    }
  }
|}

let with_logs ~f ~final =
  Testutil_mock.with_mocked_logs ~f ~final:(fun log_content res ->
      pr2 (spf "logs = %s" log_content);
      let exit_code, logs =
        match res with
        | Ok code -> (code, log_content)
        | Error (Error.Exit code) -> (code, log_content)
        | _exn -> (Exit_code.fatal, log_content ^ Printexc.get_backtrace ())
      in
      final { exit_code; logs })

(* we return a fun () to match Testutil.test second element *)
let with_test_env f =
  Testutil_files.with_tempdir ~chdir:true (fun tmp_path ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE"
        !!(tmp_path / "settings.yaml")
        f)

let with_mocks f =
  let make_response_fn (req : Cohttp.Request.t) _body =
    match Uri.path (Cohttp.Request.uri req) with
    | "/api/registry/rules" ->
        Http_mock_client.check_method `POST req.meth;
        let response_body =
          fake_registry_response |> Cohttp_lwt.Body.of_string
        in
        Lwt.return Http_mock_client.(basic_response response_body)
    | "/api/agent/deployments/current" ->
        Http_mock_client.check_method `GET req.meth;
        let response_body = fake_deployment |> Cohttp_lwt.Body.of_string in
        Lwt.return Http_mock_client.(basic_response response_body)
    | url -> Alcotest.fail (spf "unexpected request: %s" url)
  in
  Http_mock_client.with_testing_client make_response_fn f ()

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

let test_publish () =
  let tests_path = tests_path () in
  with_test_env (fun () ->
      with_mocks (fun () ->
          let valid_target = tests_path in
          let valid_single_file_target =
            tests_path / "e2e" / "targets" / "semgrep-publish" / "valid"
            / "valid1.yaml"
          in

          with_logs
            ~f:(fun () -> Logout_subcommand.main [| "semgrep-logout" |])
            ~final:(fun res -> assert (res.exit_code =*= Exit_code.ok));

          (* should require login *)
          with_logs
            ~f:(fun () ->
              Publish_subcommand.main [| "semgrep-publish"; !!valid_target |])
            ~final:(fun res ->
              assert (res.exit_code =*= Exit_code.fatal);
              assert (
                Common.contains res.logs
                  "run `semgrep login` before using upload"));

          (* log back in *)
          Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" fake_token (fun () ->
              with_logs
                ~f:(fun () -> Login_subcommand.main [| "semgrep-login" |])
                ~final:(fun res -> assert (res.exit_code =*= Exit_code.ok)));

          (* fails if no rule specified *)
          with_logs
            ~f:(fun () -> Publish_subcommand.main [| "semgrep-publish" |])
            ~final:(fun res -> assert (res.exit_code =*= Exit_code.fatal));

          (* fails if invalid rule specified *)
          with_logs
            ~f:(fun () ->
              let path =
                tests_path / "e2e" / "targets" / "semgrep-publish" / "invalid"
              in
              Publish_subcommand.main [| "semgrep-publish"; !!path |])
            ~final:(fun res ->
              assert (res.exit_code =*= Exit_code.fatal);
              assert (Common.contains res.logs "Invalid rule definition:"));

          (* fails if a yaml with more than one rule is specified *)
          with_logs
            ~f:(fun () ->
              let path =
                tests_path / "e2e" / "targets" / "semgrep-publish" / "multirule"
              in
              Publish_subcommand.main [| "semgrep-publish"; !!path |])
            ~final:(fun res ->
              assert (res.exit_code =*= Exit_code.fatal);
              assert (
                Common.contains res.logs
                  "Rule contains more than one rule: only yaml files with a \
                   single can be published"));

          with_logs
            ~f:(fun () ->
              Publish_subcommand.main
                [| "semgrep-publish"; "--visibility=public"; !!valid_target |])
            ~final:(fun res ->
              assert (res.exit_code =*= Exit_code.fatal);
              assert (
                Common.contains res.logs
                  "Only one public rule can be uploaded at a time: specify a \
                   single Semgrep rule"));

          with_logs
            ~f:(fun () ->
              Publish_subcommand.main
                [|
                  "semgrep-publish";
                  "--visibility=public";
                  !!valid_single_file_target;
                |])
            ~final:(fun res ->
              assert (res.exit_code =*= Exit_code.fatal);
              assert (
                Common.contains res.logs
                  "--visibility=public requires --registry-id"));

          ()))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  pack_tests "Osemgrep Publish (e2e)" [ ("test_publish", test_publish) ]
