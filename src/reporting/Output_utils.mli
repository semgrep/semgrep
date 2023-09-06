(*
   Utilities for working with the types defined in Semgrep_output_v1.atd
*)

(* internal function used for the 'lines:' field in the JSON output
 * but also now in Output.ml for the Emacs output.
 *)
val lines_of_file :
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position ->
  Fpath.t ->
  string list

(* for now used only to interpolate metavars in the 'message:' field *)
val contents_of_file :
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position ->
  Fpath.t ->
  string

val position_of_token_location : Tok.location -> Semgrep_output_v1_t.position

val position_range :
  Tok.location ->
  Tok.location ->
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position

val location_of_token_location : Tok.location -> Semgrep_output_v1_t.location
val tokens_to_single_loc : Tok.t list -> Semgrep_output_v1_t.location option

(*
   Sort results in the most natural way possible, typically preferring
   match location first.
*)
val sort_match_results :
  Semgrep_output_v1_t.core_output -> Semgrep_output_v1_t.core_output
