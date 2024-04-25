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

(** Tracing library for Semgrep using several libraries:
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
 * function. The results are sent to the default endpoint (see constants below),
 * which collects them to send to a viewer.
 *
 * If you want to send traces to a different endpoint, append your command with
 * the `--trace-endpoint=<url> argument
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type span = Trace_core.span

(* Implement the show and pp functions manually since we know
   Trace_core.span is int64*)
let show_span = Int64.to_string
let pp_span fmt = Format.fprintf fmt "%Ldl"

type user_data = Trace_core.user_data

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let default_endpoint = "https://telemetry.semgrep.dev"
let default_dev_endpoint = "https://telemetry.dev2.semgrep.dev"
let default_local_endpoint = "http://localhost:4318"

(*****************************************************************************)
(* Wrapping functions Trace gives us to instrument the code *)
(*****************************************************************************)
let with_span = Trace_core.with_span
let add_data_to_span = Trace_core.add_data_to_span

(* This function is helpful for Semgrep, which stores an optional span *)
let add_data_to_opt_span sp data =
  Option.iter (fun sp -> Trace_core.add_data_to_span sp data) sp

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)
(* Set according to README of https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let configure_tracing service_name =
  Otel.Globals.service_name := service_name;
  Otel.GC_metrics.basic_setup ();
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ());
  let otel_backend = Opentelemetry_client_ocurl.create_backend () in
  (* This forwards the spans from Trace to the Opentelemetry collector *)
  Opentelemetry_trace.setup_with_otel_backend otel_backend

let with_tracing fname trace_endpoint data f =
  (* This sets up the OTel collector and runs the given function.
   * Note that the function is traced by default. This makes sure we
     always trace the given function; it also ensures that all the spans from
     the given run are nested under a single trace.
   * ALT: we could also have wrapped this with a `Otel.Scope.with_ambient_scope`
     to ensure the trace_id is the same for all spans, but we decided that
     having the top level time is a good default. *)
  let url =
    match trace_endpoint with
    | Some url -> (
        match url with
        | "semgrep-prod" -> default_endpoint
        | "semgrep-dev" -> default_dev_endpoint
        | "semgrep-local" -> default_local_endpoint
        | _ -> url)
    | None -> default_endpoint
  in
  let data () = data in
  let config = Opentelemetry_client_ocurl.Config.make ~url () in
  Opentelemetry_client_ocurl.with_setup ~config () @@ fun () ->
  with_span ~__FILE__ ~__LINE__ ~data fname @@ fun sp -> f sp

(* Alt: using cohttp_lwt

   Lwt_platform.run (let res = Opentelemetry_client_cohttp_lwt.with_setup ~config () @@ fun () ->
   run_with_span "All time" f in
     Lwt.bind (Lwt_platform.sleep 0.01) (fun () -> Lwt.return res)) *)
