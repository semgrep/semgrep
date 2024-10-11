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
module Log = Log_commons.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tracing library wrapper relying on OpenTelemetry and several libraries:
 *
 * - trace (https://github.com/c-cube/ocaml-trace) for the trace
 *   instrumentation frontend (e.g. the annotations)
 * - opentelemetry (https://github.com/imandra-ai/ocaml-opentelemetry)
 *   for the backend that processes traces
 * - opentelemetry-client-ocurl (included with opentelemetry) for the
 *   collector.
 *   TODO use opentelemetry-client-cohttp-lwt instead since
 *   we rely on cottp in other places already
 * - ambient-context (https://github.com/ELLIOTTCABLE/ocaml-ambient-context)
 *   which we set up for opentelemetry to use
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
 * Functions can be instrumented using a ppx (see ../ppx/) or directly with
 * the `with_span` function. The results are sent to the default endpoint
 * (see constants below), which collects them to send to a viewer.
 *
 * If you want to send traces to a different endpoint, append your command with
 * the `--trace-endpoint=<url> argument
 *
 * Communicates with the Python tracing in cli/src/semgrep/tracing.py
 *
 * See also https://www.notion.so/semgrep/How-to-add-tracing-b0e1eaa1531e408cbb074663d1f840a6
 *
 * TODO:
 *  - code in libs/ should be independent of semgrep, so some of the
 *    hardcoded endpoints below should be moved to src/, not in libs/
 *  - get rid of our dependency to curl, but the current alternative is to
 *    use Opentelemetry_client_cohttp_lwt which require to lwt-ify the code
 *    which is annoying. Does opentelemetry have an eio backend?
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

type config = {
  endpoint : Uri.t;
  (* To add data to our opentelemetry top span, so easier to filter *)
  top_level_span : span option;
}
[@@deriving show]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

(* Coupling: these need to be kept in sync with tracing.py *)
let trace_level_var = "SEMGREP_TRACE_LEVEL"
let parent_span_id_var = "SEMGREP_TRACE_PARENT_SPAN_ID"
let parent_trace_id_var = "SEMGREP_TRACE_PARENT_TRACE_ID"

(*****************************************************************************)
(* Levels *)
(*****************************************************************************)

type level =
  | Info  (** Traces for timings we want to track regularly (default level) *)
  | Debug  (** Traces to help profile a specific run *)
  | Trace  (** All traces *)

(* TODO: replace by [@@deriving show] above, but then weird compilation errors*)
let show_level = function
  | Info -> "Info"
  | Debug -> "Debug"
  | Trace -> "Trace"

(* TODO? why define our own type repeating an existing one? *)
let level_to_trace_level level =
  match level with
  | Info -> Trace_core.Level.Info
  | Debug -> Trace_core.Level.Debug1
  | Trace -> Trace_core.Level.Trace

(*****************************************************************************)
(* Wrapping functions Trace gives us to instrument the code *)
(*****************************************************************************)

let add_data_to_span = Trace_core.add_data_to_span

let opt_add_data_to_span data sp =
  sp |> Option.iter (fun sp -> Trace_core.add_data_to_span sp data)

(* This function is helpful for Semgrep, which stores an optional span *)
let add_data data (tracing_opt : config option) =
  tracing_opt
  |> Option.iter (fun tracing ->
         tracing.top_level_span |> opt_add_data_to_span data)

(* We get nice ui in Jaeger if we do this *)
let mark_span_error sp = add_data_to_span sp [ ("error", `Bool true) ]

let add_yojson_to_span sp yojson =
  yojson
  |> List_.map (fun (key, yojson) ->
         (key, `String (Yojson.Safe.to_string yojson)))
  |> add_data_to_span sp

(*****************************************************************************)
(* Span/Event entrypoints *)
(*****************************************************************************)

let trace_exn sp ?(escaped = false) exn =
  let e = Exception.catch exn in
  let exn_msg = Printexc.to_string exn in
  let exn_stacktrace =
    e |> Exception.get_trace |> Printexc.raw_backtrace_to_string
  in
  let attrs =
    [
      ("exception.escaped", `Bool escaped);
      ("exception.message", `String exn_msg);
      ("exception.stacktrace", `String exn_stacktrace);
    ]
  in
  add_data_to_span sp attrs

(* General function to run with span to use for instrumentation *)
let with_span ?(level = Info) ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f =
  let level = level_to_trace_level level in
  Trace_core.with_span ~level ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
    (fun sp ->
      (* See: https://opentelemetry.io/docs/specs/semconv/exceptions/exceptions-spans/#stacktrace-representation*)
      (* TODO: upstream this: https://github.com/imandra-ai/ocaml-opentelemetry/issues/50  *)
      try f sp with
      | exn ->
          let e = Exception.catch exn in
          trace_exn sp ~escaped:true exn;
          mark_span_error sp;
          Trace_core.exit_span sp;
          Exception.reraise e)

(* Run the entrypoint function with a span. If a parent span is given
   (e.g. via Semgrep Managed Scanning), use that as the parent span
   so that we can connect the semgrep-core trace to other traces. *)
let with_top_level_span ?(level = Info) ?parent_span_id ?parent_trace_id
    ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f =
  match (parent_span_id, parent_trace_id) with
  | None, None ->
      with_span ~level ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f
  | None, Some _
  | Some _, None ->
      Log.err (fun m ->
          m "Both %s and %s should be set when creating a subspan"
            parent_span_id_var parent_trace_id_var);
      with_span ~level ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f
  | Some span_id, Some trace_id ->
      let scope : Otel.Scope.t =
        {
          span_id = Otel.Span_id.of_hex span_id;
          trace_id = Otel.Trace_id.of_hex trace_id;
          events = [];
          attrs = [];
        }
      in
      Otel.Scope.with_ambient_scope scope (fun () ->
          with_span ~level ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f)

let trace_data_only ?(level = Info) ~__FUNCTION__ ~__FILE__ ~__LINE__ name
    (f : unit -> (string * Yojson.Safe.t) list) =
  with_span ~level ~__FUNCTION__ ~__FILE__ ~__LINE__ name (fun sp ->
      f () |> add_yojson_to_span sp)

let log_trace_message () =
  match Otel.Scope.get_ambient_scope () with
  | None ->
      (* nosemgrep: no-logs-in-library *)
      Logs.info (fun m ->
          m "Tracing is enabled for this scan. There was no trace id recorded.")
  | Some scope ->
      let id = Otel.Trace_id.to_hex scope.trace_id in
      (* nosemgrep: no-logs-in-library *)
      Logs.info (fun m ->
          m "Tracing is enabled for this scan. The trace id is <%s>." id)

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
  let url = Uri.to_string trace_endpoint in
  let level =
    match Sys.getenv_opt trace_level_var with
    | Some level -> (
        match String.lowercase_ascii level with
        | "info" -> Info
        | "debug" -> Debug
        | "trace" -> Trace
        | _ -> Info)
    | None -> Info
  in
  let parent_span_id = Sys.getenv_opt parent_span_id_var in
  let parent_trace_id = Sys.getenv_opt parent_trace_id_var in
  let data () = data in
  Trace_core.set_current_level (level_to_trace_level level);
  let config = Opentelemetry_client_ocurl.Config.make ~url () in
  Opentelemetry_client_ocurl.with_setup ~config () @@ fun () ->
  with_top_level_span ?parent_span_id ?parent_trace_id ~__FILE__ ~__LINE__ ~data
    fname
  @@ fun sp ->
  log_trace_message ();
  f sp

(* Alt: using cohttp_lwt (we probably want to do this when we switch to Eio w/ *)
(* their compatibility layer)

   Lwt_platform.run (let res = Opentelemetry_client_cohttp_lwt.with_setup ~config () @@ fun () ->
   run_with_span "All time" f in
     Lwt.bind (Lwt_platform.sleep 0.01) (fun () -> Lwt.return res))
*)
