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
  env : string option;
  (* To add data to our opentelemetry top span, so easier to filter *)
  top_level_span : span option;
}
[@@deriving show]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
(* The endpoint that otel traces will be sent to. This should only ever be set
   in configure_tracing, which is called once, at the beginning. The ref isn't
   nice, but we need it to start and stop tracing without having to pass around
   an env. See [with_tracing_paused]*)
let active_endpoint = ref None

(* Coupling: these need to be kept in sync with tracing.py *)
let trace_level_var = "SEMGREP_TRACE_LEVEL"
let parent_span_id_var = "SEMGREP_TRACE_PARENT_SPAN_ID"
let parent_trace_id_var = "SEMGREP_TRACE_PARENT_TRACE_ID"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( let@ ) = ( @@ )

(* Needed so we can reset span id's randomness on tracing restart *)
(* See restart_tracing for more detail *)
let mk_rand_bytes_8 rand_ () : bytes =
  let@ () = Otel.Lock.with_lock in
  let b = Bytes.create 8 in
  for i = 0 to 1 do
    let r = Random.State.bits rand_ in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.State.bits rand_ in
  Bytes.set b 6 (Char.chr (r land 0xff));
  Bytes.set b 7 (Char.chr ((r lsr 8) land 0xff));
  b

let mk_rand_bytes_16 rand_ () : bytes =
  let@ () = Otel.Lock.with_lock in
  let b = Bytes.create 16 in
  for i = 0 to 4 do
    let r = Random.State.bits rand_ in
    (* 30 bits, of which we use 24 *)
    Bytes.set b (i * 3) (Char.chr (r land 0xff));
    Bytes.set b ((i * 3) + 1) (Char.chr ((r lsr 8) land 0xff));
    Bytes.set b ((i * 3) + 2) (Char.chr ((r lsr 16) land 0xff))
  done;
  let r = Random.State.bits rand_ in
  Bytes.set b 15 (Char.chr (r land 0xff));
  (* last byte *)
  b
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

let trace_exn sp exn =
  let e = Exception.catch exn in
  let exn_type = Printexc.to_string_default exn in
  let exn_msg = Printexc.to_string exn in
  let exn_stacktrace =
    e |> Exception.get_trace |> Printexc.raw_backtrace_to_string
  in

  (* Datadog friendly attrs for the span
     See:
     https://docs.datadoghq.com/tracing/error_tracking/#use-span-tags-to-track-error-spans
  *)

  (* Note these are not what the otel spec expects, but the ocaml otel libary
     will do this in a future version:
     https://github.com/imandra-ai/ocaml-opentelemetry/pull/63
  *)
  let attrs =
    [
      ("error.message", `String exn_msg);
      ("error.stack", `String exn_stacktrace);
      ("error.type", `String exn_type);
      (* Forces datadog to actually track an error *)
      ("track_error", `Bool true);
    ]
  in
  add_data_to_span sp attrs

let enter_span ?(level = Info) =
  let level = level_to_trace_level level in
  Trace_core.enter_span ~level

let exit_span = Trace_core.exit_span

(* General function to run with span to use for instrumentation *)
let with_span ?(level = Info) ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name f =
  let level = level_to_trace_level level in
  Trace_core.with_span ~level ?__FUNCTION__ ~__FILE__ ~__LINE__ ?data name
    (fun sp ->
      (* TODO: Otel library does this for us in the next version(current is 0.10) *)
      try f sp with
      | exn ->
          let e = Exception.catch exn in
          trace_exn sp exn;
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

(* setup_otel sets the Otel tracing backend and Trace_core tracing backend *)
let setup_otel trace_endpoint =
  (* nosemgrep: no-logs-in-library *)
  let url = Uri.to_string trace_endpoint in
  Log.info (fun m -> m "Tracing endpoint set to %s" url);
  let config = Opentelemetry_client_ocurl.Config.make ~url () in
  let otel_backend = Opentelemetry_client_ocurl.create_backend ~config () in
  (* hack: let's just keep track of the endpoint for if we restart tracing
     instead of having to pass it down everywhere. We will assume that we will
     only ever report to one endpoint for the lifetime of the program *)
  (* coupling: [restart_tracing] *)
  active_endpoint := Some trace_endpoint;
  (* This forwards the spans from Trace to the Opentelemetry collector *)
  Opentelemetry_trace.setup_with_otel_backend otel_backend

(* Set according to README of https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let configure_tracing ?(env = "local") ?version service_name trace_endpoint =
  Otel.Globals.service_name := service_name;
  Otel.Globals.default_span_kind := Otel.Span.Span_kind_internal;
  version
  |> Option.iter (fun version ->
         Otel.Globals.add_global_attribute "service.version" (`String version));
  Otel.Globals.add_global_attribute "deployment.environment.name" (`String env);
  Log.info (fun m -> m "Setting up tracing with service name %s" service_name);
  Otel.GC_metrics.basic_setup ();
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ());
  setup_otel trace_endpoint

let stop_tracing () =
  (* hack: get the backend so we can easily stop tracing at any time. See
     [with_paused_tracing] for why we want the option to do this

     See the following for why we do it this way:
     https://github.com/imandra-ai/ocaml-opentelemetry/issues/70
  *)
  Otel.Collector.get_backend ()
  |> Option.iter (fun backend ->
         Log.info (fun m -> m "Stopping tracing");
         let module Backend = (val backend : Otel.Collector.BACKEND) in
         Backend.tick ();
         Trace_core.shutdown ();
         Otel.Collector.set_backend (module Otel.Collector.Noop_backend);
         (* Cleanup doesn't seem to always send so let's tick one more time to
            flush, see:
            https://github.com/imandra-ai/ocaml-opentelemetry/issues/69
         *)
         Backend.cleanup ())

let restart_tracing () =
  (* We must re-initialize the randomness on restart since this usually happens
     after a parmap fork. If we don't do this then all parmap forks will have
     the same randomness and use duplicate span ids! This behavior is fine in
     jaeger but duplicates don't show up in datadog *)
  let new_random_state = Random.State.make_self_init () in
  Otel.Rand_bytes.rand_bytes_8 := mk_rand_bytes_8 new_random_state;
  Otel.Rand_bytes.rand_bytes_16 := mk_rand_bytes_16 new_random_state;
  !active_endpoint
  |> Option.iter (fun endpoint ->
         Log.info (fun m -> m "Restarting tracing");
         setup_otel endpoint)

(* Otel SOMETIMES segfaults if the traced process forks while the collector is running. So we
   need to stop the backends before forking, then continue after forking is
   done.

   See https://github.com/imandra-ai/ocaml-opentelemetry/issues/68
*)
let with_tracing_paused f =
  stop_tracing ();
  Common.protect ~finally:restart_tracing f

let with_tracing fname data f =
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
  let parent_span_id = Sys.getenv_opt parent_span_id_var in
  let parent_trace_id = Sys.getenv_opt parent_trace_id_var in
  let data () = data in
  Trace_core.set_current_level (level_to_trace_level level);
  let f' () =
    with_top_level_span ?parent_span_id ?parent_trace_id ~__FILE__ ~__LINE__
      ~data fname
    @@ fun sp ->
    log_trace_message ();
    f sp
  in
  Common.protect ~finally:stop_tracing f'

(* Alt: using cohttp_lwt (we probably want to do this when we switch to Eio w/ *)
(* their compatibility layer)

   Lwt_platform.run (let res = Opentelemetry_client_cohttp_lwt.with_setup ~config () @@ fun () ->
   run_with_span "All time" f in
     Lwt.bind (Lwt_platform.sleep 0.01) (fun () -> Lwt.return res))
*)
