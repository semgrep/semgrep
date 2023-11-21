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
open File.Operators
open Testutil
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of test_publish.py to OCaml.
 *
 * Note that unlike most cli/tests/e2e/test_xxx.py tests, we can't reuse
 * test_login.py to test osemgrep because of the use of mocking
 * and 'use_click+runner=True' in test_login.py
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

(* TODO? move in commons? Test_helpers.ml? *)
let with_setenv envvar str f =
  let old = Sys.getenv_opt envvar in
  Unix.putenv envvar str;
  Common.finalize f (fun () ->
      match old with
      | Some str -> Unix.putenv envvar str
      (* ugly: Unix does not provide unsetenv,
       * see https://discuss.ocaml.org/t/unset-environment-variable/9025
       *)
      | None -> Unix.putenv envvar "")

let with_logs ~f ~final =
  Logs_helpers.with_mocked_logs ~f ~final:(fun log_content res ->
      pr2 (spf "logs = %s" log_content);
      let exit_code, logs =
        match res with
        | Ok code -> (code, log_content)
        | Error (Error.Exit code) -> (code, log_content)
        | _exn -> (Exit_code.fatal, log_content ^ Printexc.get_backtrace ())
      in
      final { exit_code; logs })

(* TODO: factorize with Unit_LS.with_mock_envvars *)
let with_semgrep_envvar envvar str f =
  with_setenv envvar str (fun () ->
      Semgrep_envvars.with_envvars (Semgrep_envvars.of_current_sys_env ()) f)

(* we return a fun () to match Testutil.test second element *)
let with_test_env f =
  Testutil_files.with_tempdir ~chdir:true (fun tmp_path ->
      with_semgrep_envvar "SEMGREP_SETTINGS_FILE"
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

(* alt: we're calling Logout_subcommand.main() below; we could
 * be even more "e2e" by calling CLI.main() instead, but that would require
 * to move this file out of cli_login/ because of mutual dependencies.
 *)
(* let _test_login_no_tty : Testutil.test =
   ( __FUNCTION__,
     with_test_env (fun () ->
         with_logs
           ~f:(fun () ->
             (* make stdin non-interactive so Unix.isatty Unix.stdin
              * called in Login_subcommand.run returns false
              *)
             let old_stdin = Unix.dup Unix.stdin in
             let in_, _out_ = Unix.pipe () in
             Unix.dup2 in_ Unix.stdin;
             let exit_code = Login_subcommand.main [| "semgrep-login" |] in
             Unix.dup2 old_stdin Unix.stdin;
             exit_code)
           ~final:(fun res ->
             assert (res.logs =~ ".*meant to be run in an interactive terminal");
             assert (res.exit_code =*= Exit_code.fatal))) ) *)

(* let _test_logout_with_env_token : Testutil.test =
   ( __FUNCTION__,
     with_test_env (fun () ->
         with_semgrep_envvar "SEMGREP_APP_TOKEN" fake_token (fun () ->
             with_fake_deployment_response fake_deployment (fun () ->
                 with_logs
                   ~f:(fun () -> Login_subcommand.main [| "semgrep-logout" |])
                   ~final:(fun res ->
                     assert (res.logs =~ "[.\n]*Logged out (log back in with `semgrep login`)");
                     assert (res.exit_code =*= Exit_code.ok))))) ) *)

let test_publish () =
  (* let runner = SemgrepRunner(
         env={"SEMGREP_SETTINGS_FILE": str(tmp_path / ".settings.yaml")},
         use_click_runner=True,
         mix_stderr=False
     ) *)
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

          (* mocker.patch(
                 "semgrep.app.auth.get_deployment_from_token", return_value="deployment_name"
             );
          *)

          (* log back in *)
          with_semgrep_envvar "SEMGREP_APP_TOKEN" fake_token (fun () ->
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
          (*
                str(
                  Path(
                      TESTS_PATH / "e2e" / "targets" / "semgrep-publish" / "multirule"
                  ).resolve()
              ),
      *)
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
