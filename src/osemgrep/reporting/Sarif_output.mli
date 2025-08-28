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
(* Formats the CLI output to the SARIF format. *)
val sarif_output :
  Rule.hrules ->
  Semgrep_output_v1_t.format_context ->
  Semgrep_output_v1_t.cli_output ->
  is_pro:bool ->
  show_dataflow_traces:bool ->
  Sarif.Sarif_v_2_1_0_t.sarif_json_schema
