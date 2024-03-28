val sarif_output :
  bool ->
  string ->
  Rule.hrules ->
  Semgrep_output_v1_t.cli_output ->
  Sarif.Sarif_v_2_1_0_t.sarif_json_schema
