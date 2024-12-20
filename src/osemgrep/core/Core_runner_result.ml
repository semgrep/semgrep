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
  scanned : Fpath.t Set_.t;
}

(* Add errors that were obtained previously but didn't block the run *)
let add_errors (errors : Semgrep_output_v1_t.core_error list) (res : t) : t =
  let core = res.core in
  { res with core = { core with errors = errors @ core.errors } }
