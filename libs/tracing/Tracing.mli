(* Tracing library for Semgrep using several libraries.
   See header of Tracing.ml for details
 *)

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

(* Expose the function to instrument code to send traces.
   In general, we prefer using the ppx *)
val with_span :
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Trace_core.user_data) list) ->
  string ->
  (Trace_core.span -> 'a) ->
  'a

(* Expose a function to instrument code to send traces with
   one kind of data.
   TODO deprecate this after we switch to using the official trace ppx *)
val run_with_span :
  string -> ?data:(string * Trace_core.user_data) list -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

(* Before instrumenting anything, configure some settings. *)
val configure_tracing : string -> unit

(* Setup instrumentation and run the passed function.
   Stops instrumenting once that function is finished. *)
val with_setup : (unit -> 'a) -> 'a
