(* Yoann Padioleau
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

let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Partial port of test_login.py to OCaml (some is in Test_osemgrep.ml)
 *
 * Note that unlike most cli/tests/e2e/test_xxx.py tests, we can't reuse
 * test_login.py to test osemgrep because of the use of mocking
 * and 'use_click_runner=True' in test_login.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

type result = { exit_code : Exit_code.t; logs : string }

let with_logs ~f ~final =
  Testutil_mock.with_mocked_logs ~f ~final:(fun log_content res ->
      UCommon.pr2 (spf "logs = %s" log_content);
      let code =
        match res with
        | Ok code -> code
        | Error (Error.Exit code) -> code
        | _ -> Exit_code.fatal
      in
      UCommon.pr2
        (spf "exit_code = %d, meaning = %s" (Exit_code.to_int code)
           (Exit_code.to_message code));
      final { exit_code = code; logs = log_content })

(* we return a fun () to match Testo.test second element *)
let with_login_test_env f () =
  Testutil_files.with_tempdir ~chdir:true (fun tmp_path ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE"
        !!(tmp_path / "settings.yaml")
        f)

let with_fake_deployment_response return_value f =
  let make_response_fn (req : Cohttp.Request.t) _body =
    match Uri.path (Cohttp.Request.uri req) with
    | "/api/agent/deployments/current" ->
        Http_mock_client.check_method `GET req.meth;
        let response_body = return_value |> Cohttp_lwt.Body.of_string in
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
let test_logout_not_logged_in caps : Testo.test =
  t __FUNCTION__
    (with_login_test_env (fun () ->
         with_logs
           ~f:(fun () -> Logout_subcommand.main caps [| "semgrep-logout" |])
           ~final:(fun res ->
             assert (res.logs =~ ".*You are not logged in");
             assert (res.exit_code =*= Exit_code.ok))))

let test_login_no_tty caps : Testo.test =
  t __FUNCTION__
    (with_login_test_env (fun () ->
         with_logs
           ~f:(fun () ->
             (* make stdin non-interactive so Unix.isatty Unix.stdin
              * called in Login_subcommand.run returns false
              *)
             let old_stdin = Unix.dup Unix.stdin in
             let in_, _out_ = Unix.pipe () in
             Unix.dup2 in_ Unix.stdin;
             let exit_code = Login_subcommand.main caps [| "semgrep-login" |] in
             Unix.dup2 old_stdin Unix.stdin;
             exit_code)
           ~final:(fun res ->
             assert (res.logs =~ ".*meant to be run in an interactive terminal");
             assert (res.exit_code =*= Exit_code.fatal))))

(* This token does not have to be valid because we mock the deployment
 * request and response that is supposed to come from our endpoint and
 * check for its validity.
 *)
let fake_token = "token1234"

(* deployment_response in semgrep_output_v1.atd
 * alt: we could build it using Semgrep_output_v1_t deployment_response record.
 *)
let fake_deployment =
  {|
  { "deployment":
    { "id": 1234,
      "name": "deployment1234"
    }
  }
|}

let test_login_with_env_token caps : Testo.test =
  t __FUNCTION__
    (with_login_test_env (fun () ->
         Semgrep_envvars.with_envvar "SEMGREP_APP_TOKEN" fake_token (fun () ->
             with_fake_deployment_response fake_deployment (fun () ->
                 (* login with env token *)
                 with_logs
                   ~f:(fun () ->
                     Login_subcommand.main caps [| "semgrep-login" |])
                   ~final:(fun res ->
                     assert (res.logs =~ "[.\n]*Saved access token");
                     assert (res.exit_code =*= Exit_code.ok));

                 (* login should fail on second call *)
                 with_logs
                   ~f:(fun () ->
                     Login_subcommand.main caps [| "semgrep-login" |])
                   ~final:(fun res ->
                     assert (res.logs =~ ".*You're already logged in");
                     assert (res.exit_code =*= Exit_code.fatal));

                 (* clear login (by logging out) *)
                 with_logs
                   ~f:(fun () ->
                     Logout_subcommand.main
                       (caps :> < Cap.stdout >)
                       [| "semgrep-logout" |])
                   ~final:(fun res ->
                     assert (res.logs =~ ".*Logged out!");
                     assert (res.exit_code =*= Exit_code.ok));

                 (* logout twice should work *)
                 with_logs
                   ~f:(fun () ->
                     Logout_subcommand.main
                       (caps :> < Cap.stdout >)
                       [| "semgrep-logout" |])
                   ~final:(fun res ->
                     assert (res.logs =~ ".*You are not logged in");
                     assert (res.exit_code =*= Exit_code.ok))))))

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : < Cap.network ; Cap.stdout >) =
  Testo.categorize "Osemgrep Login (e2e)"
    [
      test_logout_not_logged_in (caps :> < Cap.stdout >);
      test_login_no_tty caps;
      test_login_with_env_token caps;
    ]
