module Out = Semgrep_output_v1_j

(* Can return an Error when we get a NoTokenLocation exn when
 * trying to get the range of a match or metavar.
 *)
val match_to_match :
  Core_result.processed_match -> (Out.core_match, Core_error.t) result

(* Note that this uses and reset !Core_error.g_errors internally *)
val core_output_of_matches_and_errors : Core_result.t -> Out.core_output

(* for abstract_content and subpatterns matching-explanations
 * TODO: should not use! the result may miss some commas
 *)
val metavar_string_of_any : AST_generic.any -> string

(* now used also in osemgrep *)
val error_to_error : Core_error.t -> Out.core_error
val dedup_and_sort : Out.core_match list -> Out.core_match list
