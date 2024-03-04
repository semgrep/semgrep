(* Apply the fix for the list of matches to the given file, returning the
 * resulting file contents. Currently used only for tests.
 *)
val produce_autofixes :
  Core_result.processed_match list -> Core_result.processed_match list

val apply_fixes : ?dryrun:bool -> Textedit.t list -> unit

(* for use by osemgrep *)
val apply_fixes_of_core_matches :
  ?dryrun:bool -> Semgrep_output_v1_t.core_match list -> unit

(* Apply the fix for the list of matches to the given file, returning the
   resulting file contents. Currently used only for tests.

   Raises an exception in case of a conflict.
*)
val apply_fixes_to_file_exn : Fpath.t -> Textedit.t list -> string
