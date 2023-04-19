(*
   Utilities for working with the types defined in Semgrep_output_v1.atd
*)

val position_of_token_location :
  Tok.token_location -> Output_from_core_t.position

val position_range :
  Tok.token_location ->
  Tok.token_location ->
  Output_from_core_t.position * Output_from_core_t.position

val location_of_token_location :
  Tok.token_location -> Output_from_core_t.location

(*
   Sort results in the most natural way possible, typically preferring
   match location first.
*)
val sort_match_results :
  Output_from_core_t.core_match_results -> Output_from_core_t.core_match_results
