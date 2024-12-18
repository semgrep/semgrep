(*
   Utilities for working with the types defined in Semgrep_output_v1.atd
*)

(* internal function used for the 'lines:' field in the JSON output
 * but also now in Output.ml for the Emacs output.
 * This may return Error "out of bound access ...".
 *)
val lines_of_file_at_range :
  Semgrep_output_v1_t.position * Semgrep_output_v1_t.position ->
  Fpath.t ->
  (string list, string) result

(* used to interpolate metavars in the 'message:' field and
 * for the dataflow call traces.
 *)
val content_of_file_at_range :
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
val sort_core_matches :
  Semgrep_output_v1_t.core_match list -> Semgrep_output_v1_t.core_match list

(* Sort matches in an order suitable for displaying results. *)
val sort_cli_matches :
  Semgrep_output_v1_t.cli_match list -> Semgrep_output_v1_t.cli_match list
