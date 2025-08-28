(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   List files that semgrep treats as targets before rule-specific
   or language-specific filtering.

   This is an internal/experimental option for troubleshooting
   currently implemented as a '--x-ls' flag of 'semgrep scan' for simplicity
   of implementation.
   If we wanted to make it official and permanent, we should probably
   turn it into a proper 'semgrep ls' or 'semgrep show targets' subcommand.
*)

(*
   Print just the paths (like 'ls') or print a bunch of details (like 'ls -l')
*)
type format = Paths_only | Long [@@deriving show]

val default_format : format

(*
   Print the list of selected targets in alphabetical order, one per line.
*)
val run :
  < Cap.readdir ; .. > ->
  target_roots:Scanning_root.t list ->
  targeting_conf:Find_targets.conf ->
  format:format ->
  Exit_code.t
