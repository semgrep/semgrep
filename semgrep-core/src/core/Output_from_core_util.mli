(*
   Utilities for working with the types defined in Semgrep_core_response.atd
   (Semgrep_core_response_t module)
*)

(*
   Sort results in the most natural way possible, typically preferring
   match location first.
*)
val sort_match_results :
  Output_from_core_t.core_match_results -> Output_from_core_t.core_match_results
