(* core algorithm, using AST-based or basic or regex fix strategy *)
val render_fix : Core_match.t -> Textedit.t option

(* Produce the fixes for the list of matches. Currently used for tests but
 * also in the Pre_post_core_scan.Autofix_processor now that we handle
 * autofix mostly in semgrep-core.
 *)
val produce_autofixes :
  Core_result.processed_match list -> Core_result.processed_match list

val produce_autofix : Core_result.processed_match -> Core_result.processed_match

(* Apply the fix for the list of matches to the given file, returning the
   resulting file contents. Currently used only for tests.

   Raises an exception in case of a conflict.
*)
val apply_fixes_to_file_exn : Fpath.t -> Textedit.t list -> string

(* for use by osemgrep *)
val apply_fixes_of_core_matches :
  ?dryrun:bool -> Semgrep_output_v1_t.core_match list -> unit
