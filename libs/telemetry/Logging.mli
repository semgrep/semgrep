(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val no_telemetry_tag_set : Logs.Tag.set
(** [no_telemetry_tag_set] is a logging tag set containing
    {!Opentelemetry_logs.no_telemetry_tag}. See
    {!Opentelemetry_logs.no_telemetry_tag} for more information, and an
    example *)

val attach_otel_reporter :
  ?service_name:string ->
  ?attributes:(string * Opentelemetry.value) list ->
  Logs.reporter ->
  Logs.reporter
(** [attach_otel_reporter ?service_name ?attributes logger] attaches a
    {!Logs.reporter} to an existing reporter to additionally send logs to the
    Otel backend. To disable logging for just this reporter, tag the log with
    {!Opentelemetry_logs.no_telemetry_tag}.
 *)
