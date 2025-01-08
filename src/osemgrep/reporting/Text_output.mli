(* Semgrep text output. For the JSON output see Cli_json_output.ml *)

val text_output :
  max_chars_per_line:int ->
  max_lines_per_finding:int ->
  Semgrep_output_v1_t.cli_output ->
  string

(* internals, used also for incremental display of matches *)
val matches_output :
  max_chars_per_line:int ->
  max_lines_per_finding:int ->
  Semgrep_output_v1_t.cli_match list ->
  string
