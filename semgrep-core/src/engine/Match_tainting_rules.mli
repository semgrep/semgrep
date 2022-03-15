val check_rule :
  Rule.t ->
  (string -> Metavariable.bindings -> Parse_info.t list Lazy.t -> unit) ->
  Config_semgrep.t * Equivalence.equivalences ->
  Rule.taint_spec ->
  Xtarget.t ->
  Report.rule_profiling Report.match_result

(* It could be a private function, but it is also used by Deep Semgrep *)
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
