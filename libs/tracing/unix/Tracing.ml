(* Emma Jin
 *
 * Copyright (C) 2023 Emma Jin
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module Otel = Opentelemetry

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* Tracing library for Semgrep using several libraries:
 *
 * - trace (https://github.com/c-cube/ocaml-trace) for the trace
     instrumentation frontend (e.g. the annotations)
 * - opentelemetry (https://github.com/imandra-ai/ocaml-opentelemetry)
     for the backend that processes traces
 * - opentelemetry-client-ocurl (included with opentelemetry) for the
     collector. TODO use opentelemetry-client-cohttp-lwt instead since
     we rely on cottp in other places already
 * - ambient-context (https://github.com/ELLIOTTCABLE/ocaml-ambient-context)
     which we set up for opentelemetry to use
 *
 * The goal of tracing is to track how we perform in real scans. Things we
 * might do with this data include tracking the p95 scan time, tracking the
 * p95 scan time of a particular phase, alerting on significantly large scans,
 * or digging into the trace of a scan that's taking too long to figure out
 * where it's taking the most time.
 *
 * We use the `trace` frontend for instrumenting the code so that if we want
 * to use a different backend (permanently, or for our own profiling), we can
 * switch it out in just this file.
 *
 * Functions can be instrumented using a ppx or directly with the `with_span`
 * function. The results are sent to <TODO fill this out when we get the
 * permanent endpoint>, which collects them to send to a viewer.
 *
 * If you want to send traces to a different endpoint, prepend your command with
 * `OTEL_EXPORTER_OTLP_ENDPOINT=<url>`
 *
 * TODO we'll probably need instructions for some system of tags?
 *)

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

(* TODO Now that `ppx_trace` exists, I'm going to use that instead. However, I'm
   still fighting libraries how to make that work, so I'm starting with these helpers *)

(* TODO this doesn't work with multiple processes. I suspect we're going to have
   to set it up for each process *)

let with_span = Trace_core.with_span

let run_with_span span_name ?data f =
  let data = Option.map (fun d () -> d) data in
  Trace_core.with_span ?data ~__FILE__ ~__LINE__ span_name @@ fun _sp -> f ()

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

(* Set according to README of https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let configure_tracing service_name =
  Otel.Globals.service_name := service_name;
  Otel.GC_metrics.basic_setup ();
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ())

let with_setup f =
  let otel_backend =
    Opentelemetry_client_ocurl.create_backend
      (* TODO configure this to use a permanent endpoint *) ()
  in
  (* This forwards the spans from Trace to the Opentelemetry collector *)
  Opentelemetry_trace.setup_with_otel_backend otel_backend;

  (* This sets up the OTel collector and runs the given function.
   * Note that the function is traced by default. This makes sure we
     always trace the given function; it also ensures that all the spans from
     the given run are nested under a single trace.
   * ALT: we could also have wrapped this with a `Otel.Scope.with_ambient_scope`
     to ensure the trace_id is the same for all spans, but we decided that
     having the top level time is a good default. *)
  Opentelemetry_client_ocurl.with_setup () @@ fun () ->
  run_with_span "All time" f

(* Alt: using cohttp_lwt

   Lwt_platform.run (let res = Opentelemetry_client_cohttp_lwt.with_setup ~config () @@ fun () ->
   run_with_span "All time" f in
     Lwt.bind (Lwt_platform.sleep 0.01) (fun () -> Lwt.return res)) *)
