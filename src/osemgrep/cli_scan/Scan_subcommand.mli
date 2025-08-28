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
   Parse a semgrep-scan command, execute it and exit.

   Usage: main caps [| "semgrep-scan"; ... |]

   This function returns an exit code to be passed to the 'exit' function.

   Note that this subcommand can also calls the 'test', 'validate', and 'show'
   subcommands when using legacy flags (e.g., with 'semgrep scan --test').
*)

type caps =
  < Cap.stdout
  ; Cap.network
  ; Cap.tmp
  ; Cap.chdir
  ; Cap.readdir
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit >

val main : < caps ; .. > -> string array -> Exit_code.t

(* internal *)
val run_conf : < caps ; .. > -> Scan_CLI.conf -> Exit_code.t
val run_scan_conf : < caps ; .. > -> Scan_CLI.conf -> Exit_code.t

(* internal: also used in CI *)
val check_targets_with_rules :
  (* caps - network *)
  < Cap.stdout
  ; Cap.chdir
  ; Cap.tmp
  ; Cap.fork
  ; Cap.time_limit
  ; Cap.memory_limit
  ; Cap.readdir
  ; .. > ->
  Scan_CLI.conf ->
  Profiler.t ->
  Rule_fetching.rules_and_origin list ->
  Fppath.t list * Core_error.t list * Semgrep_output_v1_t.skipped_target list ->
  ( Rule.rule list * Core_runner.result * Semgrep_output_v1_t.cli_output,
    Exit_code.t )
  result
