type replacement_ctx

val of_bindings : Metavariable.bindings -> replacement_ctx
val of_out : Semgrep_output_v1_t.metavars -> replacement_ctx
val interpolate_metavars : string -> replacement_ctx -> string
