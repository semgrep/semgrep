(** Tracing library for Semgrep
 *
 * Provide a simple interface to send contextualized performance metrics
 * for Semgrep code using Opentelemetry traces. By default, they send to
 * our Datadog endpoint, but the collector can be customized using the
 * SEMGREP_OTEL_ENDPOINT environment variable.
 *
 * To trace a program, start by calling `configure_tracing`. Then, wrap
 * the entry point of the program (e.g. `Core_command.semgrep_core_dispatch`)
 * with `with_tracing`. Traces will now be sent for the duration of that
 * function.
 *
 * Running `with_tracing` always sends a trace for the wrapped function.
 * To trace other functions called within it, run those using `with_span`.
 * You can attach data to the traces by running `add_data_to_span`.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type span = Trace_core.span [@@deriving show]

type config = {
  endpoint : Uri.t;
  (* Telemetry software like datadog and opentelemetry will organize traces by
     the environment they come from (e.g. development, staging, production). env
     here sets that metadata *)
  env : string option;
  (* To add data to our opentelemetry top span, so easier to filter *)
  top_level_span : span option;
}
[@@deriving show]

type user_data = Trace_core.user_data

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)
module Attributes : sig
  val version : string
  val instance_id : string
  val deployment_environment_name : string
end

(*****************************************************************************)
(* Levels *)
(*****************************************************************************)

type level =
  | Info  (** Enable standard tracing (default level) *)
  | Debug  (** Enable commonly used debug tracing *)
  | Trace  (** Enable everything *)

val show_level : level -> string

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

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

(* for adding data *)
val add_data_to_span : span -> (string * Trace_core.user_data) list -> unit
(** Expose the Trace function to add data to a span *)

val add_data : (string * Trace_core.user_data) list -> config option -> unit
(** Convenience version of add_data_to_span for Semgrep *)

val add_global_attribute : string -> Trace_core.user_data -> unit
(** Expose the Trace function to add global attributes to the top level span *)

(* manual span entering and exiting *)
val enter_span :
  ?level:level ->
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * user_data) list) ->
  string ->
  span
(** [enter_span ~__FILE__ ~__LINE__ "some_name"] will manually enter a span and
    return it. Must call exit_span after. Prefer [with_span] instead as it has
    better error handling *)

val exit_span : span -> unit
(** [exit_span span] will exit a span. Must be called after `enter_span`. Prefer
    [with_span] instead as it has better error handling *)

(* with span funcs *)

val with_span :
  ?level:level ->
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Trace_core.user_data) list) ->
  string ->
  (span -> 'a) ->
  'a
(** Expose the function to instrument code to send traces.
    prefer using the ppx *)

val trace_data_only :
  ?level:level ->
  __FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  string ->
  (unit -> (string * Yojson.Safe.t) list) ->
  unit

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

val configure_tracing :
  ?attrs:(string * user_data) list -> string -> Uri.t -> unit
(** [configure_tracing service_name tracing_endpoint] Before instrumenting
    anything, configure some settings. This should only be run once in a
    program, because it creates a backend with threads, HTTP connections, etc.
    when called. [service_name] is the name of the service. [~attrs] can be used
    to set additional global attributes (such as ["service.version"]), which are
    tags that will be applied to all outgoing traces/metrics/logs etc.

    NOTE: this will set the active trace endpoint to
    whatever is passed. This endpoint will be used when restarting tracing via
    [restart_tracing] *)

val stop_tracing : exit_active_spans:bool -> unit -> unit
(** [stop_tracing ~exit_active_spans ()] explicitly shuts down the Otel
    collector. If tracing has been setup this MUST be called before forking
    (such as in {!Parmap}), or you WILL experience random segfaults. This is
    safe to call multiple times in a row. See [restart_tracing] to continue
    tracing after calling this.

    Example:
    {[
      stop_tracing ();
      (if Unix.fork () = 0 then
      print_endline "child"
      else
      print_endline "parent");
      restart_tracing ();
    ]}
 *)

val restart_tracing : unit -> unit
(** [restart_tracing ()] will re-setup the Otel backend after [stop_tracing] is
    called to continue tracing. This is a no-op if [configure_tracing] has not
    been called. Will fail if called multiple times. See {!stop_tracing} for an
    example*)

val with_tracing :
  string -> (string * Trace_core.user_data) list -> (span -> 'a) -> 'a
(** [with_tracing span_name attributes f] Start tracing with a top level span
    named [span_name] that has attributes [attributes] and run [f]. Stops
    instrumenting once that function is finished. *)

val with_tracing_paused : (unit -> 'a) -> 'a
(** [with_tracing_paused f] will run [f] with tracing paused. This is usually
    called before forking, as Otel can segfault if it is not paused before
    forking. Essentially this calls [stop_tracing] and then
    [restart_tracing]. *)
