(* remove the matches in [cli_output] that were whitelisted by a
 * 'nosemgrep:' comment in the code by the user.
 *)
val process_ignores :
  keep_ignored:bool ->
  strict:bool ->
  Semgrep_output_v1_j.cli_output ->
  Semgrep_output_v1_j.cli_output

(* used by osemgrep but also by the language_server *)

val rule_id_re_str : string
val nosem_inline_re : Pcre.regexp
val nosem_previous_line_re : Pcre.regexp
