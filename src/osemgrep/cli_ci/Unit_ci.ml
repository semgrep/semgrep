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
    ]
