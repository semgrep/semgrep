(* Helpers to factorize code between the regexp and spacegrep matchers *)

type ('target_content, 'xpattern) t = {
  (* init returns an option to let the matcher the option to skip
   * certain files (e.g., big binary or minified files for spacegrep)
   *)
  init : Fpath.t -> 'target_content option;
  matcher :
    'target_content ->
    Fpath.t ->
    'xpattern ->
    (match_range * Metavariable.bindings) list;
}

(* bugfix: I used to just report one token_location, and if the match
 * was on multiple lines anyway the token_location.str was contain
 * the whole string. However, external programs using a startp/endp
 * expect a different location if the end part is on a different line
 * (e.g., pysemgrep), so now we return a pair.
 *)
and match_range = Tok.location * Tok.location

val matches_of_matcher :
  ('xpattern * Xpattern.pattern_id * string) list ->
  ('target_content, 'xpattern) t ->
  Fpath.t ->
  Origin.t ->
  Core_profiling.times Core_result.match_result

(* use an hmemo internally to speedup things *)
val line_col_of_charpos : Fpath.t -> int -> int * int
val mval_of_string : string -> Tok.t -> Metavariable.mvalue
val info_of_token_location : Tok.location -> Tok.t
