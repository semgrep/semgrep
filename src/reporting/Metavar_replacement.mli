type replacement_ctx

val of_bindings : Metavariable.bindings -> replacement_ctx
val of_out : Semgrep_output_v1_t.metavars -> replacement_ctx

val interpolate_metavars :
  ?fmt:(string -> string) -> string -> replacement_ctx -> string
(** [interpolate_metavars ?fmt msg ctx] will replace all metavars from in [msg]
    from [replacement_ctx]. Additionally, if [fmt] is supplied, it will be
    applied to the final metavar value content before it is substituted into
    [msg]. This can be used to do things like truncate the metavar content *)
