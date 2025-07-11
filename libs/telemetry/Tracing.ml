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
open Telemetry
open Common

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
(* Constants *)
(*****************************************************************************)
(* Coupling: these need to be kept in sync with tracing.py *)
let trace_level_var = "SEMGREP_TRACE_LEVEL"
let parent_span_id_var = "SEMGREP_TRACE_PARENT_SPAN_ID"
let parent_trace_id_var = "SEMGREP_TRACE_PARENT_TRACE_ID"

let empty_scope =
  Otel.Scope.make
    ~trace_id:Otel.Trace_id.(create ())
    ~span_id:Otel.Span_id.(create ())
    ()

(*****************************************************************************)
(* Levels *)
(*****************************************************************************)

type level =
  | Info  (** Traces for timings we want to track regularly (default level) *)
  | Debug  (** Traces to help profile a specific run *)
  | Trace  (** All traces *)

let trace_level : level option Domain.DLS.key = Domain.DLS.new_key (const None)

let get_level () =
  match Domain.DLS.get trace_level with
  | Some level -> level
  | None -> Info

let set_level level = Domain.DLS.set trace_level (Some level)

let filter_level (x : level) =
  match (x, get_level ()) with
  | Info, Info
  | Debug, Debug
  | Info, Debug
  | _, Trace ->
      true
  | _, Info
  | Trace, Debug ->
      false

(* TODO: replace by [@@deriving show] above, but then weird compilation errors*)
let show_level = function
  | Info -> "Info"
  | Debug -> "Debug"
  | Trace -> "Trace"

(*****************************************************************************)
(* Wrapping functions Trace gives us to instrument the code *)
(*****************************************************************************)
let add_data_to_span sc attrs = Otel.Scope.add_attrs sc (fun () -> attrs)

let opt_add_data_to_span data sc =
  sc |> Option.iter (fun sc -> add_data_to_span sc data)

(* This function is helpful for Semgrep, which stores an optional span *)
let add_data data (tracing_opt : config option) =
  tracing_opt
  |> Option.iter (fun tracing ->
         tracing.top_level_scope |> opt_add_data_to_span data)

let add_global_attribute = Otel.Globals.add_global_attribute
let record_exn = Otel.Scope.record_exception

let record_exn_curr_span exn raw_backtrace =
  (* Only record if there's an active scope *)
  let _ =
    Option.map
      (fun sc -> Otel.Scope.record_exception sc exn raw_backtrace)
      (get_current_scope ())
  in
  ()

(*****************************************************************************)
(* Span/Event entrypoints *)
(*****************************************************************************)

let with_ ?attrs ?kind ?trace_id ?parent name f =
  Otel.Trace.with_ ?attrs ?kind ?trace_id ?parent name f

let with_code_info_to_attrs ?__FUNCTION__ ~__FILE__ ~__LINE__ data =
  let code_attrs =
    [
      ( Opentelemetry.Conventions.Attributes.Code.function_,
        `String (Option.value ~default:"" __FUNCTION__) );
      (Opentelemetry.Conventions.Attributes.Code.filepath, `String __FILE__);
      (Opentelemetry.Conventions.Attributes.Code.line, `Int __LINE__);
    ]
  in
  match data with
  | Some data -> data @ code_attrs
  | None -> code_attrs

let with_span ?(level = Info) ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f =
  if filter_level level then
    let attrs =
      with_code_info_to_attrs ?__FUNCTION__ ~__FILE__ ~__LINE__ data
    in
    with_ ~attrs name f
  else f empty_scope

(* Run the entrypoint function with a span. If a parent span is given
   (e.g. via Semgrep Managed Scanning), use that as the parent span
   so that we can connect the semgrep-core trace to other traces. *)
let with_top_level_span ?(level = Info) ?parent_span_id ?parent_trace_id
    ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f =
  ignore level;
  let trace_id = Option.map Otel.Trace_id.of_hex parent_trace_id in
  let parent = Option.map Otel.Span_id.of_hex parent_span_id in
  let attrs = with_code_info_to_attrs ?__FUNCTION__ ~__FILE__ ~__LINE__ data in
  let kind = Otel.Span_kind.Span_kind_server in
  with_ ~attrs ~kind ?trace_id ?parent name f

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

let with_tracing ?(stop_otel_after = true) fname data f =
  (* This sets up the OTel collector and runs the given function.
   * Note that the function is traced by default. This makes sure we
     always trace the given function; it also ensures that all the spans from
     the given run are nested under a single trace.
   * ALT: we could also have wrapped this with a `Otel.Scope.with_ambient_scope`
     to ensure the trace_id is the same for all spans, but we decided that
     having the top level time is a good default. *)
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
  set_level level;
  let parent_span_id = Sys.getenv_opt parent_span_id_var in
  let parent_trace_id = Sys.getenv_opt parent_trace_id_var in
  (* TODO some sort of filter for trace level *)
  let f' () =
    with_top_level_span ?parent_span_id ?parent_trace_id ~__FILE__ ~__LINE__
      ~data fname
    @@ fun sp ->
    log_trace_message ();
    f sp
  in
  if stop_otel_after then
    (* coupling: [restart_otel] *)
    Common.protect ~finally:stop_otel f'
  else f' ()

(* TODO: switch to otel eio once we are on multicore/it is supported by the otel
   lib *)
