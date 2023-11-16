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
open File.Operators
open Testutil
module Http_helpers = Http_helpers.Make (Lwt_platform)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Port of test_login.py to OCaml.
 *
 * Note that unlike most cli/tests/e2e/test_xxx.py tests, we can't reuse
 * test_login.py to test osemgrep because of the use of mocking
 * and 'use_click+runner=True' in test_login.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

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

type result = { exit_code : Exit_code.t; logs : string }

let with_logs ~f ~final =
  Logs_helpers.with_mocked_logs ~f ~final:(fun log_content res ->
      final { exit_code = res; logs = log_content })

let with_semgrep_envvar envvar str f =
  with_setenv envvar str (fun () ->
      Semgrep_envvars.with_envvars (Semgrep_envvars.of_current_sys_env ()) f)

(* we return a fun () to match Testutil.test second element *)
let with_login_test_env f () =
  Testutil_files.with_tempdir ~chdir:true (fun tmp_path ->
      with_semgrep_envvar "SEMGREP_SETTINGS_FILE"
        !!(tmp_path / "settings.yaml")
        f)

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)

(* alt: we're calling Logout_subcommand.main() below; we could
 * be even more "e2e" by calling CLI.main() instead, but that would require
 * to move this file out of cli_login/ because of mutual dependencies.
 *)
let test_logout_not_logged_in : Testutil.test =
  ( __FUNCTION__,
    with_login_test_env (fun () ->
        with_logs
          ~f:(fun () -> Logout_subcommand.main [| "semgrep-logout" |])
          ~final:(fun res ->
            pr2 (spf "logs = %s" res.logs);
            assert (res.logs =~ ".*You are not logged in");
            assert (res.exit_code =*= Exit_code.ok))) )

let test_login_no_tty : Testutil.test =
  ( __FUNCTION__,
    with_login_test_env (fun () ->
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
            pr2 (spf "logs = %s" res.logs);
            assert (res.logs =~ ".*meant to be run in an interactive terminal");
            assert (res.exit_code =*= Exit_code.fatal))) )

(* This token does not have to be valid because we mock the function
 * that checks for its validation (XXX)
 *)
let fake_token = "fake1234"

let test_login_with_env_token : Testutil.test =
  ( __FUNCTION__,
    with_login_test_env (fun () ->
        with_logs
          ~f:(fun () ->
            Semgrep_envvars.with_envvars
              { !Semgrep_envvars.v with app_token = Some fake_token } (fun () ->
                Login_subcommand.main [| "semgrep-login" |]))
          ~final:(fun res ->
            pr2 (spf "logs = %s" res.logs);
            assert (res.logs =~ ".*You are not logged in");
            assert (res.exit_code =*= Exit_code.ok))) )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  pack_tests "Osemgrep Login (e2e)"
    [ test_logout_not_logged_in; test_login_no_tty; test_login_with_env_token ]
