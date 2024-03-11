(** Tracing library for Semgrep using several libraries.
   See header of Tracing.ml for details
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type analysis_flags = {
  secrets_validators : bool;
  historical_scan : bool;
  allow_all_origins : bool;
  deep_intra_file : bool;
  deep_inter_file : bool;
}

type top_level_data = { version : string; analysis_flags : analysis_flags }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

val oss_analysis : unit -> analysis_flags
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
  int64

val exit_span : int64 -> unit

val with_span :
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Trace_core.user_data) list) ->
  string ->
  (Trace_core.span -> 'a) ->
  'a
(** Expose the function to instrument code to send traces.
    In general, we prefer using the ppx *)

val add_data_to_span : int64 -> (string * Trace_core.user_data) list -> unit
(** Expose the Trace function to add data to a span *)

val add_data_to_opt_span :
  int64 option -> (string * Trace_core.user_data) list -> unit
(** Add data to an optional span. This makes it easier to add data to the
    top level span that we pass down when present *)

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

val configure_tracing : string -> unit
(** Before instrumenting anything, configure some settings. *)

val with_setup : top_level_data -> (int64 -> 'a) -> 'a
(** Setup instrumentation and run the passed function.
   Stops instrumenting once that function is finished. *)
