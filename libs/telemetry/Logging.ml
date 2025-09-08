(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module is for sending logs via telemetry. It is NOT a general logging
   library (See Logs for that). This is just a thin wrapper over
   opentelemetry-logs *)

let no_telemetry_tag_set = Opentelemetry_logs.(emit_telemetry false)

(* Same as Opentelemetry_logs.attach_otel_reporter but does not log if level is
   DEBUG *)
let attach_otel_reporter ?service_name ?attributes reporter =
  let combine r1 r2 =
    let report src level ~over k msgf =
      (* Let's not send debug logs for now, as they can be expensive and
               and we're not sure of the usefulness *)
      (* COUPLING: we do something similar in tracing.py. If we want to
               enable sending debug logs here we probably want to send them from
               pysemgrep too! *)
      match level with
      | Logs.Debug -> r1.Logs.report src level ~over k msgf
      | _ ->
          let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
          r2.Logs.report src level ~over (fun () -> v) msgf
    in
    { Logs.report }
  in
  let otel_reporter =
    Opentelemetry_logs.otel_reporter ?service_name ?attributes ()
  in
  combine reporter otel_reporter
