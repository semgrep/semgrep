type render_fix = Pattern_match.t -> Textedit.t option

(* Can return an Error because when have a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
 *)
val match_to_match :
  render_fix option ->
  Pattern_match.t ->
  (Semgrep_output_v1_t.core_match, Semgrep_error_code.error) Common.either

val core_output_of_matches_and_errors :
  render_fix option ->
  int (* number of files processed, for the stats.okfiles *) ->
  Report.final_result ->
  Semgrep_output_v1_t.core_output

(* for abstract_content and subpatterns matching-explanations
 * TODO: should not use! the result may miss some commas
 *)
val metavar_string_of_any : AST_generic.any -> string

(* now used also in osemgrep *)
val error_to_error : Semgrep_error_code.error -> Semgrep_output_v1_t.core_error

(* This is used only in the testing code, to reuse the
 * Semgrep_error_code.compare_actual_to_expected
 *)
val match_to_error : Pattern_match.t -> unit
