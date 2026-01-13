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
(**
   Utilities to run the semgrep command in tests
*)

val run_semgrep :
  ?expected_exit_code:int ->
  ?print_json_output:bool ->
  ?semgrep_settings_file:Fpath.t ->
  ?pretend_logged_in:bool ->
  string list ->
  Semgrep_output_v1_t.cli_output
(** Run any command [argv] that produces JSON of the [cli_output] type.
    Exceptions are raised in case of any failure so as to trigger a test
    failure.

    @param expected_exit_code specifies an expected exit code other than
    the default of 0.
    @param print_json_output causes the JSON output to be printed
    to stdout in a human-readable style.
    @param use_fake_login is true by default.
*)
