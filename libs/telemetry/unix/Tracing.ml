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
(* Types *)
(*****************************************************************************)

type scope = Otel.Scope.t

let empty_scope =
  Otel.Scope.make
    ~trace_id:Otel.Trace_id.(create ())
    ~span_id:Otel.Span_id.(create ())
    ()

let show_scope (sp : scope) = sp.trace_id |> Otel.Trace_id.to_hex
let pp_scope fmt (sp : scope) = Format.fprintf fmt "%s" (show_scope sp)

type user_data = Otel.value

type config = {
  endpoint : Uri.t;
  env : string option;
  (* To add data to our opentelemetry top level span, so easier to filter *)
  top_level_scope : scope option;
}
[@@deriving show]

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
(* The endpoint that otel traces will be sent to. This should only ever be set
   in configure_tracing, which is called once, at the beginning. The ref isn't
   nice, but we need it to start and stop tracing without having to pass around
   an env. See [with_tracing_paused]

   TODO(SAF-1938): This is a Domain-local value in order to more closely match
   with ParMap (which re-creates its own endpoint after forking in order to
   pull random seeds - see [restart_tracing]).  Once we are using multicore by
   default, we should revisit this.
   *)
let active_endpoint = Domain.DLS.new_key (const None)

(* Coupling: these need to be kept in sync with tracing.py *)
let trace_level_var = "SEMGREP_TRACE_LEVEL"
let parent_span_id_var = "SEMGREP_TRACE_PARENT_SPAN_ID"
let parent_trace_id_var = "SEMGREP_TRACE_PARENT_TRACE_ID"

(* Service related attributes *)
module Attributes = struct
  open Opentelemetry.Conventions

  let version = Attributes.Service.version
  let instance_id = Attributes.Service.instance_id
  let deployment_environment_name = "deployment.environment.name"
end

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( let@ ) = ( @@ )

(* Needed so we can reset scope id's randomness on tracing restart *)
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

(* Convert log level to Otel severity *)
let log_level_to_severity (level : Logs.level) : Otel.Logs.severity =
  match level with
  (* Is there a better option than unspecified? Maybe info, and make info info2? *)
  | Logs.App -> Otel.Logs.Severity_number_unspecified
  | Logs.Info -> Otel.Logs.Severity_number_info
  | Logs.Error -> Otel.Logs.Severity_number_error
  | Logs.Warning -> Otel.Logs.Severity_number_warn
  | Logs.Debug -> Otel.Logs.Severity_number_debug

(*****************************************************************************)
(* Wrapping functions Trace gives us to instrument the code *)
(*****************************************************************************)
let get_current_scope () = Otel.Scope.get_ambient_scope ()
let add_data_to_span sc attrs = Otel.Scope.add_attrs sc (fun () -> attrs)

let opt_add_data_to_span data sc =
  sc |> Option.iter (fun sc -> add_data_to_span sc data)

(* This function is helpful for Semgrep, which stores an optional span *)
let add_data data (tracing_opt : config option) =
  tracing_opt
  |> Option.iter (fun tracing ->
         tracing.top_level_scope |> opt_add_data_to_span data)

let add_global_attribute = Otel.Globals.add_global_attribute

(* Inline to maintain proper exception recording *)
let[@inline] record_exn sc = Otel.Scope.record_exception sc

let[@inline] record_exn_curr_span exn raw_backtrace =
  (* Only record if there's an active span *)
  let _ =
    Option.map
      (fun sc -> Otel.Scope.record_exception sc exn raw_backtrace)
      (get_current_scope ())
  in
  ()

(*****************************************************************************)
(* Logging *)
(*****************************************************************************)
(* TODO: upstream almost all of this into the otel library*)

(* Log a message to otel with some attrs *)
let log ?(attrs = []) ~level msg =
  (* Not sure why this is picked up by this rule...*)
  (* nosemgrep: no-logs-in-library *)
  let log_level = Logs.level_to_string (Some level) in
  (* Let's just grab the current span_id and trace_id here for now, instead of
     as params since they're the otel kind, and it'd be a bit annoying to
     convert between otrace and otel ids *)
  let current_scope = Otel.Scope.get_ambient_scope () in
  let span_id =
    current_scope |> Option.map (fun (scope : Otel.Scope.t) -> scope.span_id)
  in
  let trace_id =
    current_scope |> Option.map (fun (scope : Otel.Scope.t) -> scope.trace_id)
  in
  let severity = log_level_to_severity level in
  let log = Otel.Logs.make_str ~severity ~log_level ?trace_id ?span_id msg in
  (* Noop if no backend is set *)
  Otel.Logs.emit ~attrs [ log ]

let no_telemetry_tag = Logs_.create_tag "no_telemetry"
let no_telemetry_tag_set = Logs_.create_tag_set [ no_telemetry_tag ]

let otel_reporter : Logs.reporter =
  let report src level ~over k msgf =
    msgf (fun ?header ?(tags : Logs.Tag.set option) fmt ->
        let k _ =
          over ();
          k ()
        in
        Format.kasprintf
          (fun msg ->
            let tags = tags ||| Logs_.(create_tag_set []) in
            let attrs =
              let tags =
                (* This looks weird but is the easiest way to print log tags *)
                Logs.Tag.fold
                  (fun (tag : Logs.Tag.t) acc ->
                    let s = Format.asprintf "%a" Logs.Tag.pp tag in
                    s :: acc)
                  tags []
                |> [%to_yojson: string list] |> Yojson.Safe.to_string
              in
              let src_str = Logs.Src.name src in
              [
                (* Worth sending header?  *)
                ("header", `String (Option.value ~default:"" header));
                ("tags", `String tags);
                ("src", `String src_str);
                ("message", `String msg);
              ]
            in
            let do_not_emit = Logs.Tag.mem no_telemetry_tag tags in
            (match level with
            (* Let's not send debug logs for now, as they can be expensive and
               and we're not sure of the usefulness *)
            (* COUPLING: we do something similar in tracing.py. If we want to
               enable sending debug logs here we probably want to send them from
               pysemgrep too! *)
            | Logs.Debug -> ()
            (* Let's allow users to tag their logs when they don't want them
               emitted. This could be because they're in the GC alarm, or
               because they log info we don't want to leave the machine *)
            | _ when do_not_emit -> ()
            | _ -> log ~attrs ~level msg);
            k ())
          fmt)
  in
  { Logs.report }

(*****************************************************************************)
(* Metrics *)
(*****************************************************************************)

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

(* Safe to call whenever *)
let stop_tracing () =
  (* hack: get the backend so we can easily stop tracing at any time. See
     [with_paused_tracing] for why we want the option to do this
  *)
  Otel.Collector.get_backend ()
  |> Option.iter (fun backend ->
         Log.info (fun m -> m "Stopping tracing");
         let module Backend = (val backend : Otel.Collector.BACKEND) in
         Otel.Collector.remove_backend ();
         Backend.cleanup ())

(* setup_otel sets the Otel tracing backend and Trace_core tracing backend *)
let setup_otel trace_endpoint =
  let url = Uri.to_string trace_endpoint in
  Log.info (fun m -> m "Tracing endpoint set to %s" url);
  let config = Opentelemetry_client_ocurl.Config.make ~url () in
  let otel_backend = Opentelemetry_client_ocurl.create_backend ~config () in
  (* hack: let's just keep track of the endpoint for if we restart tracing
     instead of having to pass it down everywhere. We will assume that we will
     only ever report to one endpoint for the lifetime of the program *)
  Domain.DLS.set active_endpoint (Some trace_endpoint);
  (* Set the Otel Collector *)
  Otel.Collector.set_backend otel_backend

(* Set according to README of https://github.com/imandra-ai/ocaml-opentelemetry/ *)
let configure_tracing ?(attrs : (string * user_data) list = []) service_name
    trace_endpoint =
  Otel.Globals.service_name := service_name;
  Otel.Globals.default_span_kind := Otel.Span.Span_kind_internal;
  let attrs = attrs @ Otel.GC_metrics.get_runtime_attributes () in
  List.iter
    (fun (key, value) -> Otel.Globals.add_global_attribute key value)
    attrs;
  Log.info (fun m -> m "Setting up tracing with service name %s" service_name);
  Otel.GC_metrics.basic_setup ();
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ());
  setup_otel trace_endpoint

let restart_tracing () =
  (* We must re-initialize the randomness on restart since this usually happens
     after a parmap fork. If we don't do this then all parmap forks will have
     the same randomness and use duplicate span ids! This behavior is fine in
     jaeger but duplicates don't show up in datadog *)
  let new_random_state = Random.State.make_self_init () in
  Otel.Rand_bytes.rand_bytes_8 := mk_rand_bytes_8 new_random_state;
  Otel.Rand_bytes.rand_bytes_16 := mk_rand_bytes_16 new_random_state;
  Domain.DLS.get active_endpoint
  |> Option.iter (fun endpoint ->
         Log.info (fun m -> m "Restarting tracing");
         setup_otel endpoint)

(* Otel SOMETIMES segfaults if the traced process forks while the collector is running. So we
   need to stop the backends before forking, then continue after forking is
   done.

   See https://github.com/imandra-ai/ocaml-opentelemetry/issues/68
*)
let with_tracing_paused f =
  (* Don't exit current spans here since we only want to pause *)
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
  (* coupling: [restart_tracing] *)
  Common.protect ~finally:stop_tracing f'

(* TODO: switch to otel eio once we are on multicore/it is supported by the otel
   lib *)
