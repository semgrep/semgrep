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
type user_data = Trace_core.user_data

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

val with_span :
  ?level:Trace_core__.Level.t ->
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Trace_core.user_data) list) ->
  string ->
  (span -> 'a) ->
  'a
(** Expose the function to instrument code to send traces.
    TODO: after we have a ppx, prefer using the ppx *)

val add_data_to_span : span -> (string * Trace_core.user_data) list -> unit
(** Expose the Trace function to add data to a span *)

val add_data_to_opt_span :
  span option -> (string * Trace_core.user_data) list -> unit
(** Convenience version of add_data_to_span for Semgrep *)

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

val configure_tracing : string -> unit
(** Before instrumenting anything, configure some settings.
    This should only be run once in a program, because it creates a
    backend with threads, HTTP connections, etc. when called *)

val with_tracing :
  string -> (string * Trace_core.user_data) list -> (span -> 'a) -> 'a
(** Setup instrumentation and run the passed function.
   Stops instrumenting once that function is finished. *)
