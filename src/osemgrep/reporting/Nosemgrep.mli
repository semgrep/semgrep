(* remove the matches in [cli_output] that were whitelisted by a
 * 'nosemgrep:' comment in the code by the user.
 *)
val process_ignores :
  strict:bool ->
  Semgrep_output_v1_j.cli_output ->
  Semgrep_output_v1_j.cli_output
