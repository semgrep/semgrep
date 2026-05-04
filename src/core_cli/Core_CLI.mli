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
(* entry point of semgrep-core *)
val main : string array -> unit

(* internals used also in semgrep-core-proprietary *)

val lang : Lang.t option ref
val num_jobs : Core_scan_config.num_jobs ref
val debug : bool ref
val log_to_file : Fpath.t option ref
val trace : bool ref
val env_extra : string
val symbol_analysis : bool ref
val dump_ast : ?naming:bool -> Lang.t -> Fpath.t -> unit

(* compute Core_scan_config.t given command-line flags *)
val mk_config : ?rules:Rule.rules -> unit -> Core_scan_config.t

val output_core_results :
  Core_result.result_or_exn -> Core_scan_config.t -> unit
(** [output_core_results] takes the results of a core scan and
    format the results on stdout either in a JSON or Textual format
    (depending on the value in config.output_format)
*)

val maybe_with_eio : ?rules:Rule.rules -> (Core_scan_config.t -> 'a) -> 'a

val maybe_with_tracing :
  string ->
  string ->
  Trace_data.analysis_flags ->
  Core_scan_config.t ->
  (Core_scan_config.t -> 'a) ->
  'a

(* This requires many capabilities partly because of semgrep-core -rpc
 * which now does lots of things (including calling Core_scan for
 * transitive reachability).
 *)
val options : (unit -> Arg_.action_spec list) -> Arg_.cmdline_options
val action : string ref

(* Checks if the action is an RPC call *)
val is_rpc_call : unit -> bool

val all_actions :
  ?par_conf:Parallelism_config.t -> unit -> Arg_.action_spec list

val register_exception_printers : unit -> unit

(* Reset all CLI option refs to defaults. Call before re-entering
 * main_exn in the same process (e.g. in tests). *)
val reset_options : unit -> unit

(* this can raise exn; useful in test context *)
val main_exn : string array -> unit
