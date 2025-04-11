val rule_id_re_str : string
val nosem_inline_re : Pcre2_.t
val nosem_previous_line_re : Pcre2_.t

(* Produce the `is_ignored` fields for the processed match, without filtering
 * them out. This is used by Core_scan to postprocess all matches.
 * This may raise Common.ErrorOnFile in case of Pcre error.
 *)
val produce_ignored :
  Core_result.processed_match -> Core_result.processed_match * Core_error.t list

(* remove the matches in that were whitelisted by a 'nosemgrep:' comment in
   the code by the user.
   requires the ignores to have been "produced" via [produce_ignored] above first!
*)
val filter_ignored :
  keep_ignored:bool ->
  Semgrep_output_v1_t.core_match list ->
  Semgrep_output_v1_t.core_match list
