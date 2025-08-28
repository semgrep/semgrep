(*
   Copyright (c) 2024-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* we run metachecking rules hence the need to fetch those rules *)
type caps = < Cap.stdout ; Rule_fetching.caps ; Core_scan.caps >

(*
   Parse a semgrep-validate command, execute it and exit.

   Usage: main caps [| "semgrep-validate"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> string array -> Exit_code.t

(* called from main() above but also from Scan_subcommand.ml to manage the
 * legacy way to test things (e.g., 'semgrep scan --validate <dir>')
 *)
val run_conf : < caps ; .. > -> Validate_CLI.conf -> Exit_code.t

(* pro hooks *)
val hook_pro_init : (unit -> unit) Hook.t
