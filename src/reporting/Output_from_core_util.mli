(*
   Utilities for working with the types defined in Semgrep_output_v1.atd
*)

val position_of_token_location : Tok.location -> Semgrep_output_v1_t.position

val position_range :
  Tok.location ->
  Tok.location ->
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position

val location_of_token_location : Tok.location -> Semgrep_output_v1_t.location

(*
   Sort results in the most natural way possible, typically preferring
   match location first.
*)
val sort_match_results :
  Semgrep_output_v1_t.core_match_results ->
  Semgrep_output_v1_t.core_match_results
