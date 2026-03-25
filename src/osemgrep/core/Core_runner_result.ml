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
(* Was in Core_runner.ml but the type below is also used in Output.mli
 * so to avoid some circular dependency, simpler to have it in core/
 *)

(* LATER: ideally we should just return Core_result.t
   without the need for the intermediate Out.core_output.
*)
type t = {
  (* ocaml: not in original python implem, but just enough to get
   * Cli_json_output.cli_output_of_core_results to work
   *)
  core : Semgrep_output_v1_t.core_output;
  hrules : Rule.hrules;
  scanned : Fpath_.Fpath_set.t;
}

(* Add errors that were obtained previously but didn't block the run *)
let add_errors (errors : Core_error.t list) (res : t) : t =
  let errors = List.map Core_json_output.error_to_error errors in
  let core = res.core in
  { res with core = { core with errors = errors @ core.errors } }
