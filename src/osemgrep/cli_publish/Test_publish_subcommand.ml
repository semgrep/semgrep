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
module Http_helpers = Http_helpers.Make (Lwt_platform)

let t = Testo.create

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

let test_publish (caps : < Cap.network ; Cap.stdout >) () =
  let tests_path = tests_path () in
  with_test_env (fun () ->
      with_mocks (fun () ->
          let valid_target = tests_path in
          let valid_single_file_target =
            tests_path / "e2e" / "targets" / "semgrep-publish" / "valid"
            / "valid1.yaml"
          in

          let exit_code =
            Logout_subcommand.main
              (caps :> < Cap.stdout >)
              [| "semgrep-logout" |]
          in
          assert (exit_code =*= Exit_code.ok);

          (* should require login *)
          let exit_code =
            Publish_subcommand.main caps [| "semgrep-publish"; !!valid_target |]
          in
          assert (exit_code =*= Exit_code.fatal);

          (*
          assert (
            String_.contains res.logs
              ~term:"run `semgrep login` before using upload");
          *)

          (* log back in *)
          Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" fake_token (fun () ->
              let exit_code =
                Login_subcommand.main caps [| "semgrep-login" |]
              in
              assert (exit_code =*= Exit_code.ok));

          (* fails if no rule specified *)
          let exit_code =
            Publish_subcommand.main caps [| "semgrep-publish" |]
          in
          assert (exit_code =*= Exit_code.fatal);

          (* fails if invalid rule specified *)
          let exit_code =
            let path =
              tests_path / "e2e" / "targets" / "semgrep-publish" / "invalid"
            in
            Publish_subcommand.main caps [| "semgrep-publish"; !!path |]
          in
          assert (exit_code =*= Exit_code.fatal);

          (*
          assert (String_.contains res.logs ~term:"Invalid rule definition:");
          *)

          (* fails if a yaml with more than one rule is specified *)
          let exit_code =
            let path =
              tests_path / "e2e" / "targets" / "semgrep-publish" / "multirule"
            in
            Publish_subcommand.main caps [| "semgrep-publish"; !!path |]
          in
          assert (exit_code =*= Exit_code.fatal);

          (*
          assert (
                String_.contains res.logs
                  ~term:
                    "Rule contains more than one rule: only yaml files with a \
                     single can be published");
          *)
          let exit_code =
            Publish_subcommand.main caps
              [| "semgrep-publish"; "--visibility=public"; !!valid_target |]
          in
          assert (exit_code =*= Exit_code.fatal);

          (*
              assert (
                String_.contains res.logs
                  ~term:
                    "Only one public rule can be uploaded at a time: specify a \
                     single Semgrep rule"));
          *)
          let exit_code =
            Publish_subcommand.main caps
              [|
                "semgrep-publish";
                "--visibility=public";
                !!valid_single_file_target;
              |]
          in
          assert (exit_code =*= Exit_code.fatal)
          (*
              assert (
                String_.contains res.logs
                  ~term:"--visibility=public requires --registry-id"));
          *)))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Cap.network ; Cap.stdout >) =
  Testo.categorize "Osemgrep Publish (e2e)"
    [
      t
        ~expected_outcome:
          (Should_fail
             "TODO: something calls 'Error.exit 2' which raises an exception \
              that makes the test fail where it shouldn't.")
        ~checked_output:Stderr
        ~normalize:
          [
            (* We expect all these substrings, in this order *)
            Testo.mask_not_substrings ~mask:"[...]\n"
              [
                "run `semgrep login` before using upload";
                "Invalid rule definition:";
                "Rule contains more than one rule: only yaml files with a \
                 single can be published";
                "Only one public rule can be uploaded at a time: specify a \
                 single Semgrep rule";
                "--visibility=public requires --registry-id";
              ];
          ]
        "test_publish" (test_publish caps);
    ]
