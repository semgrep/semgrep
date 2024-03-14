(** Tracing library for Semgrep using several libraries.
   See header of Tracing.ml for details
 *)

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

val configure_tracing : string -> unit
(** Before instrumenting anything, configure some settings. *)

val with_setup : (unit -> 'a) -> string option -> 'a
(** Setup instrumentation and run the passed function.
   Stops instrumenting once that function is finished. *)
