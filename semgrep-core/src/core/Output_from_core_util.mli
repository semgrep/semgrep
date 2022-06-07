(*
   Utilities for working with the types defined in Semgrep_output_v0.atd
*)

val position_of_token_location :
  Parse_info.token_location -> Output_from_core_t.position

val position_range :
  Parse_info.token_location ->
  Parse_info.token_location ->
  Output_from_core_t.position * Output_from_core_t.position

val location_of_token_location :
  Parse_info.token_location -> Output_from_core_t.location

(*
   Sort results in the most natural way possible, typically preferring
   match location first.
*)
val sort_match_results :
  Output_from_core_t.core_match_results -> Output_from_core_t.core_match_results
