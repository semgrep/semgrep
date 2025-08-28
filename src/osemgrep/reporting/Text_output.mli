(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(* Semgrep text output. For the JSON output see Cli_json_output.ml *)

val text_output :
  max_chars_per_line:int ->
  max_lines_per_finding:int ->
  Semgrep_output_v1_t.cli_output ->
  string

(* internals, used also for incremental display of matches *)
val matches_output :
  max_chars_per_line:int ->
  max_lines_per_finding:int ->
  Semgrep_output_v1_t.cli_match list ->
  string
