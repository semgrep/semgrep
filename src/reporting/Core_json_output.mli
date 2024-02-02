(* Can return an Error because when have a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
 *)
val match_to_match :
  Core_result.processed_match ->
  (Semgrep_output_v1_t.core_match, Core_error.t) Either.t

(* Note that this uses and reset !Core_error.g_errors internally *)
val core_output_of_matches_and_errors :
  Core_result.t -> Semgrep_output_v1_t.core_output

(* for abstract_content and subpatterns matching-explanations
 * TODO: should not use! the result may miss some commas
 *)
val metavar_string_of_any : AST_generic.any -> string

(* now used also in osemgrep *)
val error_to_error : Core_error.t -> Semgrep_output_v1_t.core_error

(* This is used only in the testing code, to reuse the
 * Semgrep_error_code.compare_actual_to_expected
 *)
val match_to_push_error : Pattern_match.t -> unit

val dedup_and_sort :
  Semgrep_output_v1_t.core_match list -> Semgrep_output_v1_t.core_match list
