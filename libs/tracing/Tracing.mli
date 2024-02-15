(** Tracing library for Semgrep using several libraries.
   See header of Tracing.ml for details
 *)

(*****************************************************************************)
(* Functions to instrument the code *)
(*****************************************************************************)

val add_data_to_span : int64 -> (string * Trace_core.user_data) list -> unit
(** Expose the Trace function to add data to a span *)

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

val configure_tracing : string -> unit
(** Before instrumenting anything, configure some settings. *)

val with_setup : (unit -> 'a) -> 'a
(** Setup instrumentation and run the passed function.
   Stops instrumenting once that function is finished. *)
