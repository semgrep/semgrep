(* Apply the fix for the list of matches to the given file, returning the
 * resulting file contents. Currently used only for tests.
 *)
val produce_autofixes :
  Pattern_match.t list -> (Pattern_match.t * Textedit.t option) list

val apply_fixes : Textedit.t list -> unit

(* for use by osemgrep *)
val apply_fixes_of_core_matches : Semgrep_output_v1_t.core_match list -> unit

(* Apply the fix for the list of matches to the given file, returning the
 * resulting file contents. Currently used only for tests.
 *)
val apply_fixes_to_file :
  (Pattern_match.t * Textedit.t option) list -> file:string -> string
