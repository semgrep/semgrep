open Telemetry
(** Tracing library for Semgrep
 *
 * Provide a simple interface to send contextualized performance metrics
 * for Semgrep code using Opentelemetry traces. By default, they send to
 * our Datadog endpoint, but the collector can be customized using the
 * SEMGREP_OTEL_ENDPOINT environment variable.
 *
 * To trace a program, start by calling `configure_otel`. Then, wrap
 * the entry point of the program (e.g. `Core_command.semgrep_core_dispatch`)
 * with `with_tracing`. Traces will now be sent for the duration of that
 * function.
 *
 * Running `with_tracing` always sends a trace for the wrapped function.
 * To trace other functions called within it, run those using `with_span`.
 * You can attach data to the traces by running `add_data_to_span`.
 *)
(*****************************************************************************)
(* Levels *)
(*****************************************************************************)

type level =
  | Info  (** Enable standard tracing (default level) *)
  | Debug  (** Enable commonly used debug tracing *)
  | Trace  (** Enable everything *)

val show_level : level -> string

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

(* for adding data *)
val add_data_to_span : scope -> (string * user_data) list -> unit
(** Expose the Trace function to add data to a span *)

val add_data : (string * user_data) list -> config option -> unit
(** Convenience version of add_data_to_span for Semgrep *)

val add_global_attribute : string -> user_data -> unit
(** Expose the Trace function to add global attributes to the top level span *)

val record_exn : scope -> exn -> Printexc.raw_backtrace -> unit
(** [record_exn curr_span exn (Printexc.get_raw_backtrace ())] will record any
    error onto the specified span so we can track it. This is useful if you want
    to catch an exception, but still record it in the trace *)

val record_exn_curr_span : exn -> Printexc.raw_backtrace -> unit
(** [record_exn_curr_span exn (Printexc.get_raw_backtrace ())] will record any
    exception raised in the current span. This is a convenience function that
    uses {!get_current_span} to get the current span. Note it will not record
    the exception anywhere if no span is active (i.e. tracing is not active) *)

(* with span funcs *)

val with_span :
  ?level:level ->
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(string * user_data) list ->
  string ->
  (scope -> 'a) ->
  'a
(** Expose the function to instrument code to send traces.
    prefer using the ppx *)

val with_tracing :
  ?stop_otel_after:
    (* Usually, `with_tracing` will stop otel after its execution, for reasons given
     in `Telemetry.mli` for [stop_otel].
     This is most important for when running tracing before invoking Parmap.
     In the case of the language server, we would like to run many small traced
     spans, with no Parmap involved, and so we don't need this behavior.
     So, we allow this to be disabled.
   *)
    bool ->
  string ->
  (string * user_data) list ->
  (scope -> 'a) ->
  'a
(** [with_tracing span_name attributes f] Start tracing with a top level span
    named [span_name] that has attributes [attributes] and run [f]. Stops
    instrumenting once that function is finished. *)
