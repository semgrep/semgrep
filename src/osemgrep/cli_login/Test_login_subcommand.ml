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
(* Tests *)
(*****************************************************************************)

(* alt: we're calling Logout_subcommand.main() below; we could
 * be even more "e2e" by calling CLI.main() instead, but that would require
 * to move this file out of cli_login/ because of mutual dependencies.
 *)
let test_logout_already_logged_out () =
  Logs_helpers.with_mocked_logs
    ~f:(fun () -> Logout_subcommand.main [| "semgrep-logout" |])
    ~final:(fun log_content exit_code ->
      pr2 (spf "buffer = %s" log_content);
      assert (log_content =~ ".*You are not logged in");
      assert (exit_code =*= Exit_code.ok))

(* TODO
   Testutil_files.with_tempdir ~persist:true ~chdir:true (fun tmp_path ->
       pr2 (spf "tmp_path = %s" !!tmp_path);
*)

let tests =
  pack_tests "OSemgrep Login (e2e)"
    [ ("Logout already logged out", test_logout_already_logged_out) ]
