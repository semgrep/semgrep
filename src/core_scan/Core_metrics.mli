(* This module contains Opentelemetry metrics for the core engine. Please review
   Ometrics.ml before adding more metrics. *)
val meter_scan_inputs :
  invalid_rules:Rule_error.invalid_rule list ->
  valid_rules:Rule.t list ->
  targets:Target.t list ->
  errors:Core_error.t list ->
  skipped:Semgrep_output_v1_t.skipped_target list ->
  unit
(** [meter_scan_inputs] records various metrics about our scan inputs*)
