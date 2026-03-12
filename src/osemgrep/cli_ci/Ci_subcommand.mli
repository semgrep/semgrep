(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Parse a semgrep-ci command, execute it and exit.

   Usage: main [| "semgrep-ci"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run_conf : Ci_CLI.conf -> Exit_code.t

(* used by Test_is_blocking_Helpers.ml used itself in Test.ml *)
val rule_is_blocking : JSON.t -> bool
val finding_is_blocking : Semgrep_output_v1_t.cli_match -> bool

(* used by Unit_ci.ml *)
val scan_metadata : unit -> Semgrep_output_v1_t.scan_metadata

(* Returns true if the error type indicates a scan failure (timeout, OOM, etc.)
   that should cause the file to be added to skipped_paths. *)
val is_scan_failure_error : Semgrep_output_v1_t.error_type -> bool
