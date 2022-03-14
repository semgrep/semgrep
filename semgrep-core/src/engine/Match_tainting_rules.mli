val check_rule :
  Rule.t ->
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Rule.taint_spec ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result

(* Deep Semgrep *)
val check_def_with_rules :
  Lang.t ->
  Dataflow_tainting.config list ->
  string ->
  AST_generic.function_definition ->
  unit

(* Deep Semgrep *)
val taint_config_of_rule :
  Config_semgrep_t.t ->
  Equivalence.equivalences ->
  Common.filename ->
  AST_generic.program * Semgrep_error_code.error list ->
  Rule.rule ->
  Rule.taint_spec ->
  (Dataflow_tainting.var option ->
  Dataflow_tainting.finding list ->
  Dataflow_tainting.Taint.t Dataflow_core.env ->
  unit) ->
  Dataflow_tainting.config
