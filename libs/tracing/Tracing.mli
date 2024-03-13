(** Tracing library for Semgrep
 *
 * Provide a simple interface to send contextualized performance metrics
 * for Semgrep code using Opentelemetry traces. By default, they send to
 * our Datadog endpoint, but the collector can be customized using the
 * SEMGREP_OTEL_ENDPOINT environment variable.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type span = Int64.t [@@deriving show]

type analysis_flags = {
  secrets_validators : bool;
  (* True when secrets validators are enabled *)
  allow_all_origins : bool;
  (* True when secrets validators from any origin may be used.
     This value is discarded if secrets_validators is false *)
  historical_scan : bool;
  (* True when historical scans are enabled *)
  deep_intra_file : bool;
  (* True when deep intrafile scans (aka interproc taint) is enabled *)
  deep_inter_file : bool;
      (* True when interfile scans are enabled. Only one of `deep_inter_file`
         and `deep_intrafile` should be true. *)
}
[@@deriving show]

type top_level_data = { version : string; analysis_flags : analysis_flags }
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val no_analysis_features : unit -> analysis_flags
(** For analysis run with the oss engine, we know all the flags will be false *)

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

val enter_span :
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Trace_core.user_data) list) ->
  string ->
  span

val exit_span : span -> unit

val with_span :
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Trace_core.user_data) list) ->
  string ->
  (span -> 'a) ->
  'a
(** Expose the function to instrument code to send traces.
    In general, we prefer using the ppx *)

val add_data_to_span : span -> (string * Trace_core.user_data) list -> unit
(** Expose the Trace function to add data to a span *)

val add_data_to_opt_span :
  span option -> (string * Trace_core.user_data) list -> unit
(** Add data to an optional span. This makes it easier to add data to the
    top level span that we pass down when present *)

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

val configure_tracing : string -> unit
(** Before instrumenting anything, configure some settings. *)

val with_setup : string -> top_level_data -> (span -> 'a) -> 'a
(** Setup instrumentation and run the passed function.
   Stops instrumenting once that function is finished. *)
