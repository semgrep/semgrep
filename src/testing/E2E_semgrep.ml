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
(*
   Utilities to run the semgrep command in tests
*)

open Printf
module Out = Semgrep_output_v1_j

(*
   When semgrep runs, it will try to update the user's settings file.
   We don't want that when running tests. This environment variable
   causes semgrep to use a local file instead.
   See settings.py in pysemgrep.
*)
let default_semgrep_settings_file = Fpath.v "fake-semgrep-settings.yml"

(*
   A fake token will be used by pysemgrep's is_logged_in_weak() to
   determine whether a user is logged in. When the user is not logged in,
   some JSON response fields are omitted.

   Since the user isn't really logged in, operations that require true
   login will fail unless they find the login token somewhere else...
   TODO: clarify the above
*)
let fake_settings =
  {|has_shown_metrics_notification: true
anonymous_user_id: 621a0253-8747-45ae-9964-9c4d53baf444
api_token: d12d11d7b8773eab9e04e5e0f9f399df804f4a1727782347f3d1a3b39cd07e8a
|}

(* This will run any command but the standard output must be compatible
   with the semgrep scan's JSON output *)
let run_semgrep ?expected_exit_code ?(print_json_output = false)
    ?(semgrep_settings_file = default_semgrep_settings_file)
    ?(pretend_logged_in = true) argv : Out.cli_output =
  let (), out =
    Testo.with_environment_variables
      [ ("SEMGREP_SETTINGS_FILE", Fpath.to_string semgrep_settings_file) ]
      (fun () ->
        if pretend_logged_in then
          UFile.write_file semgrep_settings_file fake_settings;
        Testo.with_capture stdout (fun () ->
            Testutil_e2e.run_command ?expected_exit_code argv))
  in
  if print_json_output then
    (* This is for testing. Printing to stdout is encouraged. *)
    (* nosemgrep: forbid-console *)
    printf "semgrep output, reformatted:\n%s\n%!" (Yojson.Safe.prettify out);
  Out.cli_output_of_string out
