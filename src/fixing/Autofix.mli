(* Attempts to render a fix. If successful, returns the edit that should occur
 * in the target file. If unsuccessful, returns None. *)
(* val render_fix : Pattern_match.t -> Textedit.t option *)

(* Apply the fix for the list of matches to the given file, returning the
 * resulting file contents. Currently used only for tests, but with some changes
 * could be used in production as well. *)
(* val apply_fixes_to_file : Semgrep_output_v1_t.core_match list -> file:string -> string *)

val produce_autofixes :
  (Pattern_match.t * Textedit.t option) list ->
  (Pattern_match.t * Textedit.t option) list

val apply_fixes : (Pattern_match.t * Textedit.t option) list -> unit

val apply_fixes_to_file :
  (Pattern_match.t * Textedit.t option) list -> file:string -> string
