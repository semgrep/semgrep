(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep, Inc.
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
module Out = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let t = Testo.create

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let sms_scan_id = "SMS_1234"

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests =
  Testo.categorize "semgrep ci unit tests"
    [
      t "sms_scan_id from env to field" (fun () ->
          Semgrep_envvars.with_envvar "SEMGREP_MANAGED_SCAN_ID" sms_scan_id
            (fun () ->
              let res = Ci_subcommand.scan_metadata () in
              Alcotest.(check (option string))
                "checking sms_scan_id" (Some sms_scan_id) res.sms_scan_id));
      (* Tests for is_scan_failure_error - should return true for scan failures *)
      t "is_scan_failure_error returns true for Timeout" (fun () ->
          Alcotest.(check bool)
            "Timeout is scan failure" true
            (Ci_subcommand.is_scan_failure_error Out.Timeout));
      t "is_scan_failure_error returns true for OutOfMemory" (fun () ->
          Alcotest.(check bool)
            "OutOfMemory is scan failure" true
            (Ci_subcommand.is_scan_failure_error Out.OutOfMemory));
      t "is_scan_failure_error returns true for StackOverflow" (fun () ->
          Alcotest.(check bool)
            "StackOverflow is scan failure" true
            (Ci_subcommand.is_scan_failure_error Out.StackOverflow));
      t "is_scan_failure_error returns true for FixpointTimeout" (fun () ->
          Alcotest.(check bool)
            "FixpointTimeout is scan failure" true
            (Ci_subcommand.is_scan_failure_error Out.FixpointTimeout));
      t "is_scan_failure_error returns true for TimeoutDuringInterfile"
        (fun () ->
          Alcotest.(check bool)
            "TimeoutDuringInterfile is scan failure" true
            (Ci_subcommand.is_scan_failure_error Out.TimeoutDuringInterfile));
      t "is_scan_failure_error returns true for OutOfMemoryDuringInterfile"
        (fun () ->
          Alcotest.(check bool)
            "OutOfMemoryDuringInterfile is scan failure" true
            (Ci_subcommand.is_scan_failure_error Out.OutOfMemoryDuringInterfile));
      (* Tests for is_scan_failure_error - should return false for non-failures *)
      t "is_scan_failure_error returns false for ParseError" (fun () ->
          Alcotest.(check bool)
            "ParseError is not scan failure" false
            (Ci_subcommand.is_scan_failure_error Out.ParseError));
      t "is_scan_failure_error returns false for LexicalError" (fun () ->
          Alcotest.(check bool)
            "LexicalError is not scan failure" false
            (Ci_subcommand.is_scan_failure_error Out.LexicalError));
      t "is_scan_failure_error returns false for RuleParseError" (fun () ->
          Alcotest.(check bool)
            "RuleParseError is not scan failure" false
            (Ci_subcommand.is_scan_failure_error Out.RuleParseError));
      t "is_scan_failure_error returns false for MissingPlugin" (fun () ->
          Alcotest.(check bool)
            "MissingPlugin is not scan failure" false
            (Ci_subcommand.is_scan_failure_error Out.MissingPlugin));
    ]
