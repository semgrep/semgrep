(* Formats the CLI output to the SARIF format. *)
val sarif_output :
  hide_nudge:bool ->
  engine_label:string ->
  Rule.hrules ->
  Semgrep_output_v1_t.cli_output ->
  Sarif.Sarif_v_2_1_0_t.sarif_json_schema
