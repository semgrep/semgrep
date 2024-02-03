val with_span :
  ?__FUNCTION__:string ->
  __FILE__:string ->
  __LINE__:int ->
  ?data:(unit -> (string * Trace_core.user_data) list) ->
  string ->
  (Trace_core.span -> 'a) ->
  'a

val run_with_span :
  string -> ?data:(string * Trace_core.user_data) list -> (unit -> 'a) -> 'a

val initial_configuration : unit -> unit
val with_setup : (unit -> 'a) -> 'a
