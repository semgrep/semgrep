(*****************************************************************************)
(* Logging *)
(*****************************************************************************)

val no_telemetry_tag : string Logs.Tag.def
(** [no_telemetry_tag] is a logging tag that when applied to a log, said log
    will not be emitted by the tracing/telemetry backend.

    Example:
    {[
      let tags = Logs.Tag.(
          add no_telemetry_tag (name no_telemetry_tag) tags)
      in
      Logs.info (fun m ->
          m ~tags
            "This log will not be sent to the telemetry backend");
    ]}
*)

val no_telemetry_tag_set : Logs.Tag.set
(** [no_telemetry_tag_set] is a logging tag set containing {!no_telemetry_tag}.
    See {!no_telemetry_tag} for more information, and an example *)

val otel_reporter : Logs.reporter
(** [otel_reporter] is a reporter that can be used with {!Logs.set_reporter} to
    send logs to the Otel backend.To disable logging for just this reporter, tag
    the log with {!no_telemetry_tag}

    NOTE: This reporter WILL cause deadlocks if it is used in a GC alarm. To add
    Logs to a GC alarm and not trigger this, tag them with
    {!no_telemetry_tag} *)
