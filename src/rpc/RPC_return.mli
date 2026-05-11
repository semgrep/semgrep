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
module Out = Semgrep_output_v1_j

val autofix : bool -> Out.edit list -> int * (int * string list) list
val format : Out.output_format -> Out.format_context -> Out.cli_output -> string

val sarif_format :
  Out.fpath (* path to a temporary files containing the rules *) ->
  Out.format_context ->
  is_pro:bool ->
  show_dataflow_traces:bool ->
  Out.cli_output ->
  string

val contributions : unit -> Out.contributions

val validate :
  par_conf:Parallelism_config.t ->
  num_jobs:int ->
  Out.fpath ->
  Out.core_error option

(* TODO: switch all those option ref to Hook.t *)
val hook_resolve_dependencies :
  (par_conf:Parallelism_config.t ->
  download_dependency_source_code:bool ->
  allow_local_builds:bool ->
  package_manager_env:(string * string) list ->
  Out.dependency_source list ->
  (Out.dependency_source * Out.resolution_result) list)
  option
  ref

val hook_transitive_reachability_analyzer :
  (Out.transitive_reachability_filter_params -> Out.transitive_finding list)
  option
  ref

val hook_dump_rule_partitions :
  (Out.dump_rule_partitions_params -> bool) option ref

val hook_match_subprojects : (Out.fpath list -> Out.subproject list) option ref

val hook_run_symbol_analysis :
  (Out.symbol_analysis_params -> (Out.symbol_analysis, string) result) option
  ref
