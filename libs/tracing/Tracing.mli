(** Tracing library for Semgrep using several libraries.
   See header of Tracing.ml for details
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type top_level_data = { version : string }

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
