(* Formats the CLI output to the SARIF format. *)
val sarif_output :
  Rule.hrules ->
  Semgrep_output_v1_t.format_context ->
  Semgrep_output_v1_t.cli_output ->
  is_pro:bool ->
  show_dataflow_traces:bool ->
  Sarif.Sarif_v_2_1_0_t.sarif_json_schema
